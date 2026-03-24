## ── 09: Residual Diagnostics ────────────────────────────────────────────────
## Model assumption checks: normality, heteroscedasticity, collinearity, influence.

library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
library(cli)

#' Run diagnostics for a single metric model
#'
#' @param data Tibble with derived variables
#' @param metric_key Metric key
#' @param formula Model formula (character string)
#' @param model_family "gaussian" or "poisson" or "negbin"
#' @param metric_config Parsed metric_registry.yaml
#' @return List with: summary_row (tibble), plots (list)
run_diagnostics <- function(data, metric_key, formula, model_family = "gaussian",
                             metric_config = NULL) {

  cli::cli_alert_info("Running diagnostics for {metric_key}...")

  mc <- if (!is.null(metric_config)) metric_config[[metric_key]] else list(display_name = metric_key)
  formula_obj <- as.formula(formula)

  ## ── Fit model ─────────────────────────────────────────────────────────────
  formula_vars <- all.vars(formula_obj)
  model_data <- data |> tidyr::drop_na(dplyr::any_of(formula_vars))

  if (model_family == "gaussian") {
    model <- tryCatch(lm(formula_obj, data = model_data), error = function(e) NULL)
  } else if (model_family == "poisson") {
    model <- tryCatch(glm(formula_obj, data = model_data, family = poisson), error = function(e) NULL)
  } else if (model_family == "negbin") {
    model <- tryCatch(MASS::glm.nb(formula_obj, data = model_data), error = function(e) NULL)
  } else {
    model <- tryCatch(lm(formula_obj, data = model_data), error = function(e) NULL)
  }

  if (is.null(model)) {
    cli::cli_alert_warning("Model fitting failed for {metric_key}")
    return(list(
      summary_row = tibble::tibble(
        metric = metric_key, formula = formula, model_family = model_family,
        n_obs = nrow(model_data),
        shapiro_p = NA_real_, shapiro_status = "not_applicable",
        bp_p = NA_real_, bp_status = "not_applicable",
        max_vif = NA_real_, vif_status = "not_applicable",
        max_cooks = NA_real_, cooks_status = "not_applicable",
        overall_status = "fail", notes = "model_fitting_failed"
      ),
      plots = list()
    ))
  }

  ## ── Shapiro-Wilk test (normality) ─────────────────────────────────────────
  resids <- residuals(model)
  shapiro_result <- tryCatch(shapiro.test(resids), error = function(e) NULL)
  shapiro_p <- if (!is.null(shapiro_result)) shapiro_result$p.value else NA_real_
  shapiro_status <- dplyr::case_when(
    is.na(shapiro_p) ~ "not_applicable",
    shapiro_p >= 0.05 ~ "pass",
    shapiro_p >= 0.01 ~ "caution",
    TRUE ~ "fail"
  )

  ## ── Breusch-Pagan test (heteroscedasticity) ───────────────────────────────
  bp_p <- NA_real_
  bp_status <- "not_applicable"
  if (model_family == "gaussian") {
    bp_result <- tryCatch(lmtest::bptest(model), error = function(e) NULL)
    bp_p <- if (!is.null(bp_result)) bp_result$p.value else NA_real_
    bp_status <- dplyr::case_when(
      is.na(bp_p) ~ "not_applicable",
      bp_p >= 0.05 ~ "pass",
      bp_p >= 0.01 ~ "caution",
      TRUE ~ "fail"
    )
  }

  ## ── VIF (collinearity) ────────────────────────────────────────────────────
  max_vif <- NA_real_
  vif_status <- "not_applicable"
  n_pred <- length(all.vars(formula_obj)) - 1
  if (n_pred >= 2) {
    vif_vals <- tryCatch(car::vif(model), error = function(e) NULL)
    if (!is.null(vif_vals)) {
      ## Handle GVIF for factors (take GVIF^(1/(2*Df)))
      if (is.matrix(vif_vals)) {
        max_vif <- max(vif_vals[, "GVIF^(1/(2*Df))"], na.rm = TRUE)
      } else {
        max_vif <- max(vif_vals, na.rm = TRUE)
      }
      vif_status <- dplyr::case_when(
        max_vif < 5  ~ "pass",
        max_vif < 10 ~ "caution",
        TRUE         ~ "fail"
      )
    }
  }

  ## ── Cook's distance (influence) ───────────────────────────────────────────
  cooks <- cooks.distance(model)
  max_cooks <- max(cooks, na.rm = TRUE)
  ## Threshold: 4/n
  cooks_threshold <- 4 / nrow(model_data)
  n_influential <- sum(cooks > cooks_threshold, na.rm = TRUE)
  cooks_status <- dplyr::case_when(
    max_cooks < cooks_threshold     ~ "pass",
    n_influential <= 2              ~ "caution",
    TRUE                            ~ "fail"
  )

  ## ── Overall status ────────────────────────────────────────────────────────
  statuses <- c(shapiro_status, bp_status, vif_status, cooks_status)
  statuses <- statuses[statuses != "not_applicable"]
  overall <- dplyr::case_when(
    any(statuses == "fail")    ~ "fail",
    any(statuses == "caution") ~ "caution",
    TRUE                       ~ "pass"
  )

  ## ── Diagnostic plots ──────────────────────────────────────────────────────
  plots <- list()
  if (model_family == "gaussian") {
    plots$diagnostic_4panel <- tryCatch({
      plot_data <- tibble::tibble(
        fitted   = fitted(model),
        residuals = resids,
        std_resid = rstandard(model),
        sqrt_std_resid = sqrt(abs(rstandard(model))),
        leverage = hatvalues(model),
        cooks_d  = cooks,
        obs      = seq_along(resids)
      )

      p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fitted, y = residuals)) +
        ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        ggplot2::geom_smooth(se = FALSE, color = "blue", method = "loess",
                              formula = y ~ x) +
        ggplot2::labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
        ggplot2::theme_minimal()

      p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = std_resid)) +
        ggplot2::stat_qq() +
        ggplot2::stat_qq_line(color = "red") +
        ggplot2::labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
        ggplot2::theme_minimal()

      p3 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fitted, y = sqrt_std_resid)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(se = FALSE, color = "blue", method = "loess",
                              formula = y ~ x) +
        ggplot2::labs(title = "Scale-Location", x = "Fitted Values",
                      y = expression(sqrt("Standardized Residuals"))) +
        ggplot2::theme_minimal()

      p4 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = leverage, y = std_resid)) +
        ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::labs(title = "Residuals vs Leverage", x = "Leverage", y = "Standardized Residuals") +
        ggplot2::theme_minimal()

      patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2)
    }, error = function(e) {
      cli::cli_alert_warning("Diagnostic plot failed for {metric_key}: {e$message}")
      NULL
    })
  }

  list(
    summary_row = tibble::tibble(
      metric         = metric_key,
      formula        = formula,
      model_family   = model_family,
      n_obs          = nrow(model_data),
      shapiro_p      = shapiro_p,
      shapiro_status = shapiro_status,
      bp_p           = bp_p,
      bp_status      = bp_status,
      max_vif        = max_vif,
      vif_status     = vif_status,
      max_cooks      = max_cooks,
      cooks_status   = cooks_status,
      overall_status = overall,
      notes          = NA_character_
    ),
    plots = plots,
    model = model
  )
}


#' Run diagnostics for all selected models
#'
#' @param data Tibble with derived variables
#' @param model_selections Tibble from select_final_models()
#' @param strat_decisions Tibble from make_stratification_decisions()
#' @param metric_config Parsed metric_registry.yaml
#' @return List with: summary_df (tibble), all_plots (list), all_models (list)
run_all_diagnostics <- function(data, model_selections, strat_decisions,
                                 metric_config) {

  cli::cli_alert_info("Running diagnostics for all selected models...")

  ## Filter to rows with a model selected
  valid_rows <- which(!is.na(model_selections$predictors))
  cli::cli_alert_info("Processing {length(valid_rows)} models...")

  ## Choose parallel or sequential map
  map_fn <- if (requireNamespace("furrr", quietly = TRUE) &&
                !inherits(future::plan(), "sequential")) {
    function(...) furrr::future_map(..., .options = furrr::furrr_options(seed = TRUE))
  } else {
    purrr::map
  }

  ## Process in parallel
  results_list <- map_fn(valid_rows, function(i) {
    sel <- model_selections[i, ]
    metric_key <- sel$metric
    mc <- metric_config[[metric_key]]

    pred_list <- trimws(unlist(strsplit(sel$predictors, ",")))
    response <- mc$column_name

    strat_dec <- strat_decisions |> dplyr::filter(metric == metric_key)
    if (nrow(strat_dec) > 0 && strat_dec$decision_type[1] == "single") {
      strat_var <- strat_dec$selected_strat[1]
      if (!strat_var %in% pred_list && strat_var %in% names(data)) {
        pred_list <- c(strat_var, pred_list)
      }
    }

    formula_str <- paste(response, "~", paste(pred_list, collapse = " + "))
    model_family <- if (isTRUE(mc$count_model)) "poisson" else "gaussian"

    tryCatch(
      run_diagnostics(data, metric_key, formula_str, model_family, metric_config),
      error = function(e) {
        list(
          summary_row = tibble::tibble(
            metric = metric_key, formula = formula_str, model_family = model_family,
            n_obs = NA_integer_,
            shapiro_p = NA_real_, shapiro_status = "not_applicable",
            bp_p = NA_real_, bp_status = "not_applicable",
            max_vif = NA_real_, vif_status = "not_applicable",
            max_cooks = NA_real_, cooks_status = "not_applicable",
            overall_status = "fail", notes = paste("error:", e$message)
          ),
          plots = list(),
          model = NULL
        )
      }
    )
  })

  ## Combine
  summary_df <- dplyr::bind_rows(purrr::map(results_list, "summary_row"))
  all_plots <- purrr::list_c(purrr::imap(results_list, function(res, idx) {
    mk <- model_selections$metric[valid_rows[idx]]
    if (length(res$plots) > 0) {
      purrr::set_names(res$plots, paste0(mk, "_", names(res$plots)))
    } else {
      list()
    }
  }))
  all_models <- purrr::set_names(
    purrr::compact(purrr::map(results_list, "model")),
    purrr::map_chr(
      purrr::keep(seq_along(results_list), ~ !is.null(results_list[[.x]]$model)),
      ~ model_selections$metric[valid_rows[.x]]
    )
  )

  n_pass <- sum(summary_df$overall_status == "pass", na.rm = TRUE)
  n_caution <- sum(summary_df$overall_status == "caution", na.rm = TRUE)
  n_fail <- sum(summary_df$overall_status == "fail", na.rm = TRUE)

  cli::cli_alert_success(
    "Diagnostics complete: {n_pass} pass, {n_caution} caution, {n_fail} fail"
  )
  if (n_fail > 0) {
    fail_metrics <- summary_df |> dplyr::filter(overall_status == "fail") |> dplyr::pull(metric)
    cli::cli_alert_warning("  Failed: {paste(fail_metrics, collapse = ', ')}")
  }
  if (n_caution > 0) {
    caution_metrics <- summary_df |> dplyr::filter(overall_status == "caution") |> dplyr::pull(metric)
    cli::cli_alert_info("  Caution: {paste(caution_metrics, collapse = ', ')}")
  }

  list(
    summary_df = summary_df,
    all_plots = all_plots,
    all_models = all_models
  )
}
