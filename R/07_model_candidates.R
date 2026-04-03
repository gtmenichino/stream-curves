## ── 07: Model Candidates ────────────────────────────────────────────────────
## Best subsets regression (OLS) or GLM candidates for each metric.

library(dplyr)
library(leaps)
library(MASS)
library(tidyr)
library(ggplot2)
library(cli)

#' Resolve regsubsets dummy variable names back to original column names
#'
#' leaps::regsubsets() expands factor variables into dummy indicators
#' (e.g., "Ecoregion" -> "EcoregionRegion_B", "EcoregionRegion_C"). This helper
#' maps those dummy names back to the original factor column names.
#'
#' @param selected_names Character vector of names from regsubsets $which matrix
#' @param data Data frame used in the model (to check column names and types)
#' @return Character vector of unique original column names
resolve_dummy_names <- function(selected_names, data) {
  data_cols <- names(data)
  resolved <- vapply(selected_names, function(nm) {
    ## If name matches a column directly, return as-is
    if (nm %in% data_cols) return(nm)
    ## Otherwise check if nm starts with a factor column name
    for (col in data_cols) {
      if (is.factor(data[[col]]) && startsWith(nm, col)) return(col)
    }
    nm
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  unique(resolved)
}

resolve_predictor_lookup <- function(allowed_preds, predictor_config, data) {
  lookup <- tibble::tibble(
    predictor_key = allowed_preds,
    column_name = vapply(allowed_preds, function(pred_key) {
      predictor_config[[pred_key]]$column_name %||% pred_key
    }, character(1))
  ) |>
    dplyr::filter(.data$column_name %in% names(data))

  lookup
}

map_columns_to_keys <- function(column_names, key_by_col) {
  unique(vapply(column_names, function(col_name) {
    key_by_col[[col_name]] %||% col_name
  }, character(1), USE.NAMES = FALSE))
}

selected_terms_from_row <- function(row, model_data, key_by_col) {
  selected_names <- names(row)[row]
  resolved_columns <- resolve_dummy_names(selected_names, model_data)
  map_columns_to_keys(resolved_columns, key_by_col)
}

#' Build model candidates for a single metric
#'
#' @param data Tibble with derived variables
#' @param metric_key Metric key from metric_registry
#' @param strat_decision One-row tibble from make_stratification_decisions()
#' @param metric_config Parsed metric_registry.yaml
#' @param predictor_config Parsed predictor_registry.yaml
#' @param strat_config Parsed stratification_registry.yaml
#' @return List with: candidates_df (tibble), importance_df (tibble), plots (list)
build_model_candidates <- function(data, metric_key, strat_decision,
                                    metric_config, predictor_config, strat_config) {

  mc <- metric_config[[metric_key]]
  col_name <- mc$column_name

  ## ── Determine predictors ──────────────────────────────────────────────────
  allowed_preds <- mc$allowed_predictors
  if (is.null(allowed_preds) || length(allowed_preds) == 0) {
    cli::cli_alert_warning("No allowed predictors for {metric_key}")
    return(list(candidates_df = tibble::tibble(), importance_df = tibble::tibble(), plots = list()))
  }

  pred_lookup <- resolve_predictor_lookup(allowed_preds, predictor_config, data)
  available_pred_keys <- pred_lookup$predictor_key
  available_pred_cols <- pred_lookup$column_name
  key_by_col <- stats::setNames(pred_lookup$predictor_key, pred_lookup$column_name)

  if (length(available_pred_cols) == 0) {
    cli::cli_alert_warning("No available predictors found in the data for {metric_key}")
    return(list(candidates_df = tibble::tibble(), importance_df = tibble::tibble(), plots = list()))
  }

  ## ── Handle stratification mode ────────────────────────────────────────────
  strat_var <- NULL
  strat_mode <- mc$stratification_mode %||% "covariate"

  if (!is.null(strat_decision) && strat_decision$decision_type == "single") {
    strat_key <- strat_decision$selected_strat
    strat_var <- strat_config[[strat_key]]$column_name %||% strat_key
    if (!strat_var %in% names(data)) {
      cli::cli_alert_warning("Stratification variable {strat_var} not in data")
      strat_var <- NULL
    }
  }

  ## ── Prepare model data ────────────────────────────────────────────────────
  model_cols <- c(col_name, available_pred_cols)
  if (!is.null(strat_var) && strat_mode == "covariate") {
    model_cols <- c(model_cols, strat_var)
  }

  model_data <- data |>
    dplyr::select(dplyr::any_of(model_cols)) |>
    tidyr::drop_na()

  n_complete <- nrow(model_data)
  cli::cli_alert_info(
    "{metric_key}: {n_complete} complete cases for modeling (of {nrow(data)} total)"
  )

  if (n_complete < mc$min_sample_size) {
    cli::cli_alert_warning("{metric_key}: insufficient complete cases ({n_complete})")
    return(list(candidates_df = tibble::tibble(), importance_df = tibble::tibble(), plots = list()))
  }

  ## ── Count model pathway ───────────────────────────────────────────────────
  if (isTRUE(mc$count_model)) {
    return(build_count_model_candidates(
      model_data, metric_key, col_name, available_pred_cols, strat_var, mc, key_by_col
    ))
  }

  ## ── Best subsets regression (OLS) ─────────────────────────────────────────
  ## Build formula for regsubsets (predictors only, no strat for now)
  pred_formula <- paste(available_pred_cols, collapse = " + ")
  if (!is.null(strat_var) && strat_mode == "covariate") {
    pred_formula <- paste(strat_var, "+", pred_formula)
  }
  full_formula <- as.formula(paste(col_name, "~", pred_formula))

  ## Run exhaustive best subsets
  best_model <- tryCatch(
    leaps::regsubsets(
      full_formula,
      data = model_data,
      method = "exhaustive",
      nvmax = length(available_pred_cols) + if (!is.null(strat_var)) 1 else 0,
      really.big = FALSE
    ),
    error = function(e) {
      cli::cli_alert_warning("Best subsets failed for {metric_key}: {e$message}")
      NULL
    }
  )

  if (is.null(best_model)) {
    return(list(candidates_df = tibble::tibble(), importance_df = tibble::tibble(), plots = list()))
  }

  s <- summary(best_model)

  ## Build model table — resolve dummy names back to original columns
  candidates_df <- tibble::tibble(
    metric     = metric_key,
    model_id   = seq_along(s$bic),
    predictors = apply(s$which[, -1, drop = FALSE], 1, function(x) {
      terms <- selected_terms_from_row(x, model_data, key_by_col)
      paste(terms, collapse = ", ")
    }),
    n_predictors = apply(s$which[, -1, drop = FALSE], 1, function(x) {
      length(selected_terms_from_row(x, model_data, key_by_col))
    }),
    r_squared  = s$rsq,
    adj_r2     = s$adjr2,
    cp         = s$cp,
    bic        = s$bic,
    n_obs      = n_complete
  )

  ## Sort by BIC and compute delta BIC
  candidates_df <- candidates_df |>
    dplyr::arrange(bic) |>
    dplyr::mutate(
      delta_bic = bic - min(bic),
      rank      = dplyr::row_number()
    )

  ## ── Predictor importance ──────────────────────────────────────────────────
  top_mask <- candidates_df$delta_bic < 2
  all_predictor_keys <- available_pred_keys
  if (sum(top_mask) > 0) {
    predictor_matrix <- s$which[, -1, drop = FALSE]
    ord <- order(s$bic)
    predictor_matrix <- predictor_matrix[ord, , drop = FALSE]
    selected_top_terms <- apply(predictor_matrix[top_mask, , drop = FALSE], 1, function(x) {
      selected_terms_from_row(x, model_data, key_by_col)
    })

    if (!is.list(selected_top_terms)) {
      selected_top_terms <- list(selected_top_terms)
    }

    importance_df <- tibble::tibble(
      metric = metric_key,
      predictor = all_predictor_keys,
      importance = vapply(all_predictor_keys, function(pred_key) {
        mean(vapply(selected_top_terms, function(sel) pred_key %in% sel, logical(1)))
      }, numeric(1))
    ) |>
      dplyr::arrange(dplyr::desc(.data$importance))
  } else {
    importance_df <- tibble::tibble(
      metric = metric_key,
      predictor = all_predictor_keys,
      importance = 0
    )
  }

  ## ── Plots ─────────────────────────────────────────────────────────────────
  plots <- list()

  ## BIC vs number of predictors
  plots$bic_vs_npred <- ggplot2::ggplot(candidates_df, ggplot2::aes(x = n_predictors, y = bic)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = min(candidates_df$bic) + 2, linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = paste0(mc$display_name, ": BIC vs Number of Predictors"),
      x = "Number of Predictors",
      y = "BIC"
    ) +
    streamcurves_minimal_plot_theme()

  ## Adj R² vs number of predictors
  plots$adjr2_vs_npred <- ggplot2::ggplot(candidates_df, ggplot2::aes(x = n_predictors, y = adj_r2)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = paste0(mc$display_name, ": Adjusted R\u00b2 vs Number of Predictors"),
      x = "Number of Predictors",
      y = "Adjusted R\u00b2"
    ) +
    streamcurves_minimal_plot_theme()

  ## Model heatmap (top models)
  top_candidates <- candidates_df |> dplyr::filter(delta_bic < 2)
  if (nrow(top_candidates) > 0) {
    predictor_matrix_sorted <- s$which[, -1, drop = FALSE][order(s$bic), , drop = FALSE]
    top_pred_matrix <- predictor_matrix_sorted[top_mask, , drop = FALSE]
    heat_data <- as.data.frame(top_pred_matrix)
    heat_data$model <- paste0("Model ", seq_len(nrow(heat_data)))

    heat_long <- heat_data |>
      tidyr::pivot_longer(
        cols = -model,
        names_to = "predictor",
        values_to = "included"
      )

    plots$model_heatmap <- ggplot2::ggplot(heat_long, ggplot2::aes(x = predictor, y = model, fill = included)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white")) +
      ggplot2::labs(
        title = paste0(mc$display_name, ": Top Models (\u0394BIC < 2)"),
        x = "Predictor",
        y = "Model",
        fill = "Included"
      ) +
      streamcurves_minimal_plot_theme(
        axis_text_x_angle = 45,
        axis_text_x_hjust = 1
      )
  }

  ## Predictor importance bar chart
  imp_plot_data <- importance_df |> dplyr::filter(importance > 0)
  if (nrow(imp_plot_data) > 0) {
    plots$predictor_importance <- ggplot2::ggplot(
      imp_plot_data,
      ggplot2::aes(x = reorder(predictor, importance), y = importance)
    ) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = paste0(mc$display_name, ": Predictor Importance"),
        x = "Predictor",
        y = "Inclusion Frequency (Top Models)"
      ) +
      streamcurves_minimal_plot_theme()
  }

  list(
    candidates_df = candidates_df,
    importance_df = importance_df,
    plots = plots
  )
}


#' Build count model candidates (Poisson / Negative Binomial)
#'
#' @param model_data Complete-cases tibble
#' @param metric_key Metric key
#' @param col_name Response column name
#' @param available_preds Character vector of predictor column names
#' @param strat_var Stratification variable (or NULL)
#' @param mc Metric config entry
#' @param key_by_col Named vector mapping predictor columns to predictor keys
#' @return List with candidates_df, importance_df, plots
build_count_model_candidates <- function(model_data, metric_key, col_name,
                                          available_preds, strat_var, mc, key_by_col) {

  cli::cli_alert_info("{metric_key}: building count model candidates (Poisson/NB)")

  results <- list()
  model_list <- list()
  model_id <- 0

  ## Build candidate formulas with 1 to all predictors
  for (k in seq_along(available_preds)) {
    combos <- combn(available_preds, k, simplify = FALSE)
    ## Limit combinations for large predictor sets
    if (length(combos) > 50) {
      combos <- combos[1:50]
    }

    for (combo in combos) {
      model_id <- model_id + 1
      pred_str <- paste(combo, collapse = " + ")
      if (!is.null(strat_var)) {
        pred_str <- paste(strat_var, "+", pred_str)
      }
      formula <- as.formula(paste(col_name, "~", pred_str))

      ## Try Poisson first
      pois_fit <- tryCatch(
        glm(formula, data = model_data, family = poisson),
        error = function(e) NULL
      )

      if (!is.null(pois_fit)) {
        ## Check overdispersion
        disp_ratio <- sum(residuals(pois_fit, type = "pearson")^2) / pois_fit$df.residual
        use_nb <- disp_ratio > 1.5

        if (use_nb) {
          nb_fit <- tryCatch(
            MASS::glm.nb(formula, data = model_data),
            error = function(e) NULL
          )
          if (!is.null(nb_fit)) {
            predictor_keys <- map_columns_to_keys(combo, key_by_col)
            term_labels <- unique(c(if (!is.null(strat_var)) strat_var else character(0), predictor_keys))
            model_list[[model_id]] <- list(
              model = nb_fit, family = "negbin",
              predictors = paste(term_labels, collapse = ", "),
              n_predictors = length(term_labels)
            )
          }
        } else {
          predictor_keys <- map_columns_to_keys(combo, key_by_col)
          term_labels <- unique(c(if (!is.null(strat_var)) strat_var else character(0), predictor_keys))
          model_list[[model_id]] <- list(
            model = pois_fit, family = "poisson",
            predictors = paste(term_labels, collapse = ", "),
            n_predictors = length(term_labels)
          )
        }
      }
    }
  }

  ## Build candidates table
  if (length(model_list) == 0) {
    return(list(candidates_df = tibble::tibble(), importance_df = tibble::tibble(), plots = list()))
  }

  candidates_df <- purrr::map_dfr(model_list, function(m) {
    tibble::tibble(
      metric       = metric_key,
      model_id     = NA_integer_,
      predictors   = m$predictors,
      n_predictors = m$n_predictors,
      r_squared    = NA_real_,
      adj_r2       = NA_real_,
      cp           = NA_real_,
      bic          = BIC(m$model),
      n_obs        = nrow(model_data)
    )
  }) |>
    dplyr::arrange(bic) |>
    dplyr::mutate(
      model_id  = dplyr::row_number(),
      delta_bic = bic - min(bic),
      rank      = dplyr::row_number()
    )

  ## Importance from top models
  top_models <- candidates_df |> dplyr::filter(delta_bic < 2)
  all_preds <- unique(unlist(strsplit(top_models$predictors, ", ")))
  importance_df <- tibble::tibble(
    metric    = metric_key,
    predictor = all_preds,
    importance = sapply(all_preds, function(p) {
      mean(grepl(p, top_models$predictors, fixed = TRUE))
    })
  ) |>
    dplyr::arrange(dplyr::desc(importance))

  list(
    candidates_df = candidates_df,
    importance_df = importance_df,
    plots = list()
  )
}


#' Run model building for all metrics
#'
#' @param data Tibble with derived variables
#' @param strat_decisions Tibble from make_stratification_decisions()
#' @param metric_config Parsed metric_registry.yaml
#' @param predictor_config Parsed predictor_registry.yaml
#' @return List with: all_candidates (tibble), all_importance (tibble), all_plots (list)
run_all_model_building <- function(data, strat_decisions, metric_config,
                                    predictor_config, strat_config) {

  cli::cli_alert_info("Building model candidates for all metrics...")

  ## Filter to eligible metrics
  eligible <- purrr::keep(names(metric_config), function(mk) {
    mc <- metric_config[[mk]]
    if (mc$metric_family %in% c("categorical")) return(FALSE)
    isTRUE(mc$best_subsets_allowed) || isTRUE(mc$count_model)
  })

  cli::cli_alert_info("Processing {length(eligible)} eligible metrics...")

  ## Choose parallel or sequential map
  map_fn <- if (requireNamespace("furrr", quietly = TRUE) &&
                !inherits(future::plan(), "sequential")) {
    function(...) furrr::future_map(..., .options = furrr::furrr_options(seed = TRUE))
  } else {
    purrr::map
  }

  ## Process in parallel
  results_list <- map_fn(eligible, function(metric_key) {
    strat_dec <- strat_decisions |> dplyr::filter(metric == metric_key)
    if (nrow(strat_dec) == 0) strat_dec <- NULL else strat_dec <- strat_dec[1, ]
    build_model_candidates(data, metric_key, strat_dec, metric_config, predictor_config, strat_config)
  })

  ## Combine
  all_candidates <- dplyr::bind_rows(purrr::map(results_list, "candidates_df"))
  all_importance <- dplyr::bind_rows(purrr::map(results_list, "importance_df"))
  all_plots <- purrr::list_c(purrr::imap(results_list, function(res, idx) {
    mk <- eligible[idx]
    if (length(res$plots) > 0) {
      purrr::set_names(res$plots, paste0(mk, "_", names(res$plots)))
    } else {
      list()
    }
  }))

  cli::cli_alert_success(
    "Model building complete: {length(unique(all_candidates$metric))} metrics processed"
  )

  list(
    all_candidates = all_candidates,
    all_importance = all_importance,
    all_plots = all_plots
  )
}
