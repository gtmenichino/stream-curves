## ── 12: Regional / Hydraulic Geometry Curves ────────────────────────────────
## Power-function fits for bankfull dimensions vs drainage area.

library(dplyr)
library(ggplot2)
library(cli)

#' Fit a single regional curve (power function via log-log regression)
#'
#' @param data Tibble with derived variables
#' @param response_var Response column name (e.g., "BW_ft")
#' @param predictor_var Predictor column name (e.g., "DA_km2")
#' @param group_var Grouping variable for stratified fits (NULL = unstratified)
#' @return List with: model_summary (tibble), plot (ggplot)
fit_regional_curve <- function(data, response_var, predictor_var,
                                group_var = NULL) {

  display_response <- response_var

  if (!is.null(group_var)) {
    ## Stratified fit
    groups <- unique(data[[group_var]])
    groups <- groups[!is.na(groups)]

    model_rows <- purrr::map_dfr(groups, function(g) {
      df <- data |>
        dplyr::filter(.data[[group_var]] == g) |>
        dplyr::select(dplyr::all_of(c(response_var, predictor_var))) |>
        tidyr::drop_na() |>
        dplyr::filter(.data[[response_var]] > 0, .data[[predictor_var]] > 0)

      if (nrow(df) < 3) {
        return(tibble::tibble(
          response = response_var, predictor = predictor_var,
          group_var = group_var, group_level = as.character(g),
          n_obs = nrow(df), intercept = NA_real_, slope = NA_real_,
          coefficient_a = NA_real_, exponent_b = NA_real_,
          r_squared = NA_real_, adj_r2 = NA_real_, p_value = NA_real_,
          fit_status = "insufficient_data"
        ))
      }

      log_formula <- as.formula(paste0("log10(", response_var, ") ~ log10(", predictor_var, ")"))
      fit <- tryCatch(lm(log_formula, data = df), error = function(e) NULL)

      if (is.null(fit)) {
        return(tibble::tibble(
          response = response_var, predictor = predictor_var,
          group_var = group_var, group_level = as.character(g),
          n_obs = nrow(df), intercept = NA_real_, slope = NA_real_,
          coefficient_a = NA_real_, exponent_b = NA_real_,
          r_squared = NA_real_, adj_r2 = NA_real_, p_value = NA_real_,
          fit_status = "fit_failed"
        ))
      }

      s <- summary(fit)
      coefs <- coef(fit)

      tibble::tibble(
        response     = response_var,
        predictor    = predictor_var,
        group_var    = group_var,
        group_level  = as.character(g),
        n_obs        = nrow(df),
        intercept    = coefs[1],
        slope        = coefs[2],
        coefficient_a = 10^coefs[1],
        exponent_b   = coefs[2],
        r_squared    = s$r.squared,
        adj_r2       = s$adj.r.squared,
        p_value      = if (nrow(s$coefficients) >= 2) s$coefficients[2, 4] else NA_real_,
        fit_status   = "complete"
      )
    })

    ## Stratified plot
    plot_data <- data |>
      dplyr::select(dplyr::all_of(c(response_var, predictor_var, group_var))) |>
      tidyr::drop_na() |>
      dplyr::filter(.data[[response_var]] > 0, .data[[predictor_var]] > 0)

    p <- tryCatch({
      ggplot2::ggplot(plot_data, ggplot2::aes(
        x = .data[[predictor_var]],
        y = .data[[response_var]],
        color = .data[[group_var]]
      )) +
        ggplot2::geom_point(size = 2.5, alpha = 0.7) +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.2) +
        ggplot2::scale_x_log10() +
        ggplot2::scale_y_log10() +
        ggplot2::labs(
          title = paste0("Regional Curve: ", response_var, " vs ", predictor_var),
          subtitle = paste0("Stratified by ", group_var, " (log-log power function)"),
          x = paste0(predictor_var, " (log scale)"),
          y = paste0(response_var, " (log scale)"),
          color = group_var
        ) +
        streamcurves_minimal_plot_theme()
    }, error = function(e) NULL)

    return(list(model_summary = model_rows, plot = p))
  }

  ## ── Unstratified fit ──────────────────────────────────────────────────────
  df <- data |>
    dplyr::select(dplyr::all_of(c(response_var, predictor_var))) |>
    tidyr::drop_na() |>
    dplyr::filter(.data[[response_var]] > 0, .data[[predictor_var]] > 0)

  if (nrow(df) < 3) {
    return(list(
      model_summary = tibble::tibble(
        response = response_var, predictor = predictor_var,
        group_var = NA_character_, group_level = "all",
        n_obs = nrow(df), intercept = NA_real_, slope = NA_real_,
        coefficient_a = NA_real_, exponent_b = NA_real_,
        r_squared = NA_real_, adj_r2 = NA_real_, p_value = NA_real_,
        fit_status = "insufficient_data"
      ),
      plot = NULL
    ))
  }

  log_formula <- as.formula(paste0("log10(", response_var, ") ~ log10(", predictor_var, ")"))
  fit <- tryCatch(lm(log_formula, data = df), error = function(e) NULL)

  if (is.null(fit)) {
    return(list(
      model_summary = tibble::tibble(
        response = response_var, predictor = predictor_var,
        group_var = NA_character_, group_level = "all",
        n_obs = nrow(df), intercept = NA_real_, slope = NA_real_,
        coefficient_a = NA_real_, exponent_b = NA_real_,
        r_squared = NA_real_, adj_r2 = NA_real_, p_value = NA_real_,
        fit_status = "fit_failed"
      ),
      plot = NULL
    ))
  }

  s <- summary(fit)
  coefs <- coef(fit)

  model_row <- tibble::tibble(
    response     = response_var,
    predictor    = predictor_var,
    group_var    = NA_character_,
    group_level  = "all",
    n_obs        = nrow(df),
    intercept    = coefs[1],
    slope        = coefs[2],
    coefficient_a = 10^coefs[1],
    exponent_b   = coefs[2],
    r_squared    = s$r.squared,
    adj_r2       = s$adj.r.squared,
    p_value      = if (nrow(s$coefficients) >= 2) s$coefficients[2, 4] else NA_real_,
    fit_status   = "complete"
  )

  ## Prediction for ribbon
  pred_range <- seq(log10(min(df[[predictor_var]])), log10(max(df[[predictor_var]])), length.out = 100)
  pred_df <- data.frame(x = 10^pred_range)
  names(pred_df) <- predictor_var
  pred_result <- predict(fit, newdata = pred_df, interval = "confidence")
  ribbon_data <- tibble::tibble(
    x = pred_df[[predictor_var]],
    y = 10^pred_result[, "fit"],
    ymin = 10^pred_result[, "lwr"],
    ymax = 10^pred_result[, "upr"]
  )

  eq_label <- paste0(
    response_var, " = ",
    round(10^coefs[1], 3), " \u00d7 ", predictor_var, "^",
    round(coefs[2], 3),
    "\nR\u00b2 = ", round(s$r.squared, 3),
    ", n = ", nrow(df)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[predictor_var]], y = .data[[response_var]])) +
    ggplot2::geom_ribbon(
      data = ribbon_data,
      ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax),
      fill = "steelblue", alpha = 0.2
    ) +
    ggplot2::geom_line(
      data = ribbon_data,
      ggplot2::aes(x = x, y = y),
      color = "steelblue", linewidth = 1
    ) +
    ggplot2::geom_point(size = 2.5, alpha = 0.7) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::annotate("text", x = min(df[[predictor_var]]), y = max(df[[response_var]]),
                       label = eq_label, hjust = 0, vjust = 1, size = 3.5) +
    ggplot2::labs(
      title = paste0("Regional Curve: ", response_var, " vs ", predictor_var),
      subtitle = "Power function fit (log-log)",
      x = paste0(predictor_var, " (log scale)"),
      y = paste0(response_var, " (log scale)")
    ) +
    streamcurves_minimal_plot_theme()

  list(model_summary = model_row, plot = p)
}


#' Build a ggpubr boxplot for a regional response by a stratification variable
#'
#' @param data Tibble with derived variables
#' @param response_col Response column name (e.g., "BW_ft")
#' @param response_label Display name for the response variable
#' @param strat_col Stratification column name
#' @param strat_label Display name for the stratification variable
#' @param pairwise_comparisons List of list(group1, group2) pairs, or NULL
#' @return ggplot object or NULL on error
build_regional_boxplot <- function(data, response_col, response_label,
                                   strat_col, strat_label,
                                   pairwise_comparisons = NULL) {
  tryCatch({
    df <- data |>
      dplyr::select(dplyr::all_of(c(response_col, strat_col))) |>
      tidyr::drop_na()

    if (nrow(df) < 3) return(NULL)

    df[[strat_col]] <- factor(df[[strat_col]])

    ## Build group labels with n
    group_n <- df |>
      dplyr::count(.data[[strat_col]], name = "n")
    group_labels <- group_n |>
      dplyr::mutate(label = paste0(.data[[strat_col]], "\n(n=", n, ")"))
    label_map <- stats::setNames(group_labels$label, group_labels[[strat_col]])
    df$group_label <- label_map[as.character(df[[strat_col]])]

    ## Map pairwise comparisons to label-mapped pairs
    comparisons <- NULL
    if (!is.null(pairwise_comparisons) && length(pairwise_comparisons) > 0) {
      comparisons <- lapply(pairwise_comparisons, function(pair) {
        c(label_map[pair[[1]]], label_map[pair[[2]]])
      })
      comparisons <- Filter(function(x) all(!is.na(x)), comparisons)
      if (length(comparisons) == 0) comparisons <- NULL
    }

    p_base <- ggpubr::ggboxplot(
      df,
      x = "group_label",
      y = response_col,
      fill = "group_label",
      palette = "viridis"
    )

    if (!is.null(comparisons)) {
      p_base <- p_base +
        ggpubr::stat_compare_means(
          comparisons = comparisons,
          method = "wilcox.test",
          method.args = list(exact = FALSE)
        )
    }

    p_base +
      ggpubr::stat_compare_means(
        method = "kruskal.test",
        label.y.npc = "top",
        label.x.npc = "center"
      ) +
      ggplot2::labs(
        title = paste0(response_label, " by ", strat_label),
        x = strat_label,
        y = response_label
      ) +
      streamcurves_plot_text_theme(legend_position = "none")
  }, error = function(e) {
    cli::cli_alert_warning("Regional boxplot failed for {response_col} x {strat_col}: {e$message}")
    NULL
  })
}


#' Run all regional curves
#'
#' @param data Tibble with derived variables
#' @param metric_config Parsed metric_registry.yaml
#' @return List with: results (tibble), plots (list)
run_regional_curves <- function(data, metric_config) {

  cli::cli_alert_info("Running regional / hydraulic geometry curves...")

  ## Response variables for regional curves
  responses <- c("BW_ft", "BD_ft", "BA_ft2")
  predictor <- "DA_km2"

  ## Stratification variables to try
  strat_vars <- c(NULL, "Ecoregion", "DACAT", "StreamType2")

  all_results <- tibble::tibble()
  all_plots <- list()

  for (resp in responses) {
    if (!resp %in% names(data)) {
      cli::cli_alert_warning("Response column {resp} not in data, skipping")
      next
    }
    if (!predictor %in% names(data)) {
      cli::cli_alert_warning("Predictor column {predictor} not in data, skipping")
      next
    }

    ## Unstratified
    cli::cli_alert_info("Fitting: {resp} ~ {predictor} (unstratified)")
    result <- fit_regional_curve(data, resp, predictor, group_var = NULL)
    all_results <- dplyr::bind_rows(all_results, result$model_summary)
    if (!is.null(result$plot)) {
      all_plots[[paste0("regional_", resp, "_unstratified")]] <- result$plot
    }

    ## Stratified
    for (sv in strat_vars) {
      if (!is.null(sv) && sv %in% names(data)) {
        cli::cli_alert_info("Fitting: {resp} ~ {predictor} | {sv}")
        result <- fit_regional_curve(data, resp, predictor, group_var = sv)
        all_results <- dplyr::bind_rows(all_results, result$model_summary)
        if (!is.null(result$plot)) {
          all_plots[[paste0("regional_", resp, "_", sv)]] <- result$plot
        }
      }
    }
  }

  cli::cli_alert_success(
    "Regional curves complete: {nrow(all_results)} fits, {length(all_plots)} plots"
  )

  list(results = all_results, plots = all_plots)
}
