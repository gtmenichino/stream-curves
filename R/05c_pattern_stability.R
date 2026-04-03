## ── 05c: Pattern Stability Assessment ────────────────────────────────────────
## LOESS-based assessment of metric-predictor relationships within strata.

library(dplyr)
library(ggplot2)
library(cli)

#' Assess pattern stability for a metric across predictors within strata
#'
#' @param data Tibble with derived variables
#' @param metric_key Metric key from metric_registry
#' @param strat_key Stratification key (or NULL for unstratified)
#' @param predictor_keys Character vector of predictor keys to evaluate
#' @param metric_config Parsed metric_registry.yaml
#' @param strat_config Parsed stratification_registry.yaml
#' @param predictor_config Parsed predictor_registry.yaml
#' @param build_plots Whether to build scatterplots with LOESS overlays
#' @return List with: results (tibble), plots (named list of ggplots)
assess_pattern_stability <- function(data, metric_key, strat_key = NULL,
                                      predictor_keys, metric_config,
                                      strat_config, predictor_config,
                                      build_plots = TRUE) {

  mc <- metric_config[[metric_key]]
  col_name <- mc$column_name

  ## Resolve stratification column
  strat_col <- NULL
  if (!is.null(strat_key) && strat_key != "none") {
    sc <- strat_config[[strat_key]]
    if (!is.null(sc) && !is.null(sc$column_name)) {
      strat_col <- sc$column_name
    }
  }

  results <- list()
  plots <- list()

  for (pred_key in predictor_keys) {
    pc <- predictor_config[[pred_key]]
    if (is.null(pc)) next
    pred_col <- pc$column_name

    ## Check columns exist
    needed_cols <- c(col_name, pred_col)
    if (!is.null(strat_col)) needed_cols <- c(needed_cols, strat_col)
    if (!all(needed_cols %in% names(data))) next

    df <- data |>
      dplyr::select(dplyr::all_of(needed_cols)) |>
      tidyr::drop_na()

    if (nrow(df) < 6) next

    ## Ensure predictor is numeric
    if (!is.numeric(df[[pred_col]])) next

    ## ── Fit LOESS overall ───────────────────────────────────────────────────
    loess_fit <- tryCatch(
      loess(as.formula(paste0("`", col_name, "` ~ `", pred_col, "`")),
            data = df, span = 0.75),
      error = function(e) NULL
    )

    loess_r2 <- NA_real_
    n_sign_changes <- NA_integer_
    shape <- "unknown"
    stability <- "unknown"

    if (!is.null(loess_fit)) {
      fitted_vals <- predict(loess_fit)
      actual_vals <- df[[col_name]]

      ## R-squared of LOESS
      ss_res <- sum((actual_vals - fitted_vals)^2, na.rm = TRUE)
      ss_tot <- sum((actual_vals - mean(actual_vals, na.rm = TRUE))^2, na.rm = TRUE)
      if (ss_tot > 0) {
        loess_r2 <- 1 - ss_res / ss_tot
      }

      ## Count sign changes in first differences (monotonicity check)
      sorted_idx <- order(df[[pred_col]])
      sorted_fitted <- fitted_vals[sorted_idx]
      diffs <- diff(sorted_fitted)
      diffs <- diffs[!is.na(diffs) & diffs != 0]

      if (length(diffs) > 1) {
        signs <- sign(diffs)
        n_sign_changes <- sum(diff(signs) != 0, na.rm = TRUE)
      } else {
        n_sign_changes <- 0L
      }

      ## Classify shape
      if (length(diffs) == 0) {
        shape <- "flat"
      } else if (n_sign_changes == 0 && all(diffs >= 0)) {
        shape <- "monotonic_increasing"
      } else if (n_sign_changes == 0 && all(diffs <= 0)) {
        shape <- "monotonic_decreasing"
      } else if (n_sign_changes <= 1) {
        shape <- "humped"
      } else if (n_sign_changes <= 3) {
        shape <- "mildly_nonlinear"
      } else {
        shape <- "noisy"
      }

      ## Rate stability
      if (shape %in% c("monotonic_increasing", "monotonic_decreasing") &&
          !is.na(loess_r2) && loess_r2 > 0.2) {
        stability <- "stable"
      } else if (shape %in% c("humped", "mildly_nonlinear") &&
                 !is.na(loess_r2) && loess_r2 > 0.1) {
        stability <- "marginal"
      } else if (shape == "flat" || (is.na(loess_r2) || loess_r2 < 0.05)) {
        stability <- "unstable"
      } else {
        stability <- "marginal"
      }
    }

    results[[length(results) + 1]] <- tibble::tibble(
      metric = metric_key,
      stratification = strat_key %||% "none",
      predictor = pred_key,
      pattern_shape = shape,
      loess_r_squared = loess_r2,
      n_sign_changes = n_sign_changes,
      stability_rating = stability
    )

    ## ── Build scatterplot with LOESS ────────────────────────────────────────
    if (isTRUE(build_plots)) {
      p <- tryCatch({
        p_base <- ggplot2::ggplot(df, ggplot2::aes(
          x = .data[[pred_col]], y = .data[[col_name]]
        ))

        if (!is.null(strat_col)) {
          p_base <- p_base +
            ggplot2::aes(color = .data[[strat_col]]) +
            ggplot2::geom_point(alpha = 0.6, size = 2) +
            ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                                 se = TRUE, alpha = 0.15) +
            ggplot2::scale_color_viridis_d()
        } else {
          p_base <- p_base +
            ggplot2::geom_point(alpha = 0.6, size = 2, color = "#2c3e50") +
            ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                                 se = TRUE, color = "#3498db", fill = "#3498db",
                                 alpha = 0.15)
        }

        p_base +
          ggplot2::labs(
            title = paste0(mc$display_name, " vs ", pc$display_name),
            subtitle = paste0("Shape: ", shape, " | R\u00b2 = ",
                              if (!is.na(loess_r2)) round(loess_r2, 3) else "NA",
                              " | Stability: ", stability),
            x = pc$display_name,
            y = mc$display_name
          ) +
          streamcurves_minimal_plot_theme(profile = "large_analysis")
      }, error = function(e) NULL)

      if (!is.null(p)) {
        plots[[paste0(pred_key, "_", strat_key %||% "none")]] <- p
      }
    }
  }

  list(
    results = dplyr::bind_rows(results),
    plots = plots
  )
}
