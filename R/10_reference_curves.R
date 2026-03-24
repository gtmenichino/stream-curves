## ── 10: Reference Curve Development ─────────────────────────────────────────
## IQR-based scoring with piecewise-linear reference curves.

library(dplyr)
library(ggplot2)
library(cli)

#' Build a reference curve for a single metric
#'
#' @param data Tibble with derived variables
#' @param metric_key Metric key
#' @param metric_config Parsed metric_registry.yaml
#' @return List with: curve_row (tibble), bar_chart_plot (ggplot), curve_plot (ggplot)
build_reference_curve <- function(data, metric_key, metric_config,
                                   stratum_label = NULL) {

  mc <- metric_config[[metric_key]]
  col_name <- mc$column_name
  higher_is_better <- mc$higher_is_better

  ## ── Extract reference values ──────────────────────────────────────────────
  ## All 39 sites are reference-standard in initial build
  ref_values <- data[[col_name]]
  ref_values <- ref_values[!is.na(ref_values)]

  if (length(ref_values) < 5) {
    cli::cli_alert_warning("{metric_key}: too few reference values ({length(ref_values)})")
    return(list(
      curve_row = tibble::tibble(
        metric = metric_key, display_name = mc$display_name,
        n_reference = length(ref_values),
        q25 = NA_real_, q75 = NA_real_, iqr = NA_real_,
        functioning_min = NA_real_, functioning_max = NA_real_,
        at_risk_min = NA_real_, at_risk_max = NA_real_,
        not_functioning_min = NA_real_, not_functioning_max = NA_real_,
        higher_is_better = higher_is_better,
        curve_point1_x = NA_real_, curve_point1_y = NA_real_,
        curve_point2_x = NA_real_, curve_point2_y = NA_real_,
        curve_point3_x = NA_real_, curve_point3_y = NA_real_,
        median_val = NA_real_,
        mean_val = NA_real_,
        sd_val = NA_real_,
        min_val = NA_real_,
        max_val = NA_real_,
        curve_status = "insufficient_data",
        stratum = stratum_label %||% NA_character_
      ),
      bar_chart_plot = NULL,
      curve_plot = NULL
    ))
  }

  ## ── IQR computation ───────────────────────────────────────────────────────
  q25 <- quantile(ref_values, 0.25, na.rm = TRUE)
  q75 <- quantile(ref_values, 0.75, na.rm = TRUE)
  iqr_val <- q75 - q25

  ## ── Define functional categories (3-point piecewise-linear curve) ────────
  if (isTRUE(higher_is_better)) {
    ## Curve: (0, 0.00) → (Q25, 0.70) → (Q75, 1.00); capped at 1.0 above Q75
    ## Category boundaries derived from curve
    functioning_min <- as.numeric(q25)
    functioning_max <- as.numeric(q75)

    ## at_risk_min = metric value where score = 0.30
    ## On the line from (0,0) to (Q25, 0.70): score = 0.70 * x / Q25
    ## score = 0.30 → x = Q25 * 0.30/0.70 = Q25 * 3/7
    at_risk_min     <- as.numeric(q25 * 3 / 7)
    at_risk_max     <- as.numeric(q25)
    not_functioning_min <- 0
    not_functioning_max <- at_risk_min

    ## Curve points
    cp1_x <- 0;               cp1_y <- 0.00
    cp2_x <- as.numeric(q25); cp2_y <- 0.70
    cp3_x <- as.numeric(q75); cp3_y <- 1.00

  } else {
    ## Mirror: (Q25, 1.00) → (Q75, 0.70) → (Q75 + IQR*7/3, 0.00); capped at 1.0 below Q25
    functioning_min <- as.numeric(q25)
    functioning_max <- as.numeric(q75)

    ## at_risk_max: score = 0.30 on line from (Q75,0.70) to (Q75+IQR*7/3, 0.00)
    ## slope segment covers 0.70 score over IQR*7/3 distance
    ## score = 0.70 - 0.70 * (x - Q75) / (IQR * 7/3)
    ## score = 0.30 → 0.70*(x-Q75)/(IQR*7/3) = 0.40 → x-Q75 = IQR*4/3
    at_risk_min     <- as.numeric(q75)
    at_risk_max     <- as.numeric(q75 + iqr_val * 4 / 3)
    not_functioning_min <- at_risk_max
    not_functioning_max <- as.numeric(q75 + iqr_val * 7 / 3)

    ## Curve points
    cp1_x <- as.numeric(q25);                    cp1_y <- 1.00
    cp2_x <- as.numeric(q75);                    cp2_y <- 0.70
    cp3_x <- as.numeric(q75 + iqr_val * 7 / 3);  cp3_y <- 0.00
  }

  ## ── Degenerate Q25 guard ────────────────────────────────────────────────
  curve_status <- "complete"
  if (isTRUE(higher_is_better) && as.numeric(q25) <= 0) {
    curve_status <- "degenerate_q25"
    cli::cli_alert_warning("{metric_key}: Q25 <= 0, scoring curve is degenerate")
  }

  ## ── Reference bar chart (renamed histogram plot) ────────────────────────
  p_bar <- tryCatch({
    plot_data <- tibble::tibble(value = ref_values)

    ## Define band boundaries for plotting
    if (isTRUE(higher_is_better)) {
      x_range <- c(
        min(not_functioning_min, min(ref_values), na.rm = TRUE) - iqr_val * 0.1,
        max(ref_values, na.rm = TRUE) + iqr_val * 0.2
      )
    } else {
      x_range <- c(
        min(ref_values, na.rm = TRUE) - iqr_val * 0.2,
        max(not_functioning_max, max(ref_values), na.rm = TRUE) + iqr_val * 0.1
      )
    }

    p_base <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value)) +
      ## Colored bands
      ggplot2::annotate("rect",
        xmin = functioning_min, xmax = if (isTRUE(higher_is_better)) Inf else -Inf,
        ymin = -Inf, ymax = Inf,
        fill = "green", alpha = 0.15
      ) +
      ggplot2::annotate("rect",
        xmin = at_risk_min, xmax = at_risk_max,
        ymin = -Inf, ymax = Inf,
        fill = "yellow", alpha = 0.15
      ) +
      ggplot2::annotate("rect",
        xmin = not_functioning_min,
        xmax = not_functioning_max,
        ymin = -Inf, ymax = Inf,
        fill = "red", alpha = 0.15
      ) +
      ## Data points as rug
      ggplot2::geom_rug(sides = "b", color = "steelblue", alpha = 0.7) +
      ## Histogram
      ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density)),
        bins = 12, fill = "steelblue", alpha = 0.5, color = "white"
      ) +
      ## Threshold lines
      ggplot2::geom_vline(xintercept = as.numeric(q25), linetype = "dashed", color = "darkgreen", linewidth = 1) +
      ggplot2::geom_vline(xintercept = as.numeric(q75), linetype = "dashed", color = "darkgreen", linewidth = 1) +
      ## Labels
      ggplot2::annotate("text", x = as.numeric(q25), y = Inf, label = paste0("Q25=", round(q25, 1)),
                         vjust = 2, hjust = 1.1, size = 3, color = "darkgreen") +
      ggplot2::annotate("text", x = as.numeric(q75), y = Inf, label = paste0("Q75=", round(q75, 1)),
                         vjust = 2, hjust = -0.1, size = 3, color = "darkgreen") +
      ggplot2::labs(
        title = paste0(mc$display_name, ": Reference Bar Chart",
                       if (!is.null(stratum_label)) paste0(" (", stratum_label, ")") else ""),
        subtitle = paste0("IQR [", round(q25, 1), ", ", round(q75, 1), "] = Functioning (0.70-1.00) | n = ", length(ref_values)),
        x = paste0(mc$display_name, " (", mc$units, ")"),
        y = "Density"
      ) +
      ggplot2::coord_cartesian(xlim = x_range) +
      ggplot2::theme_minimal()

    p_base
  }, error = function(e) {
    cli::cli_alert_warning("Reference bar chart plot failed for {metric_key}: {e$message}")
    NULL
  })

  ## ── Reference curve plot (3-point piecewise-linear scoring curve) ───────
  p_curve <- NULL
  if (curve_status != "degenerate_q25") {
    p_curve <- tryCatch({
      ## Build curve data frame
      if (isTRUE(higher_is_better)) {
        ## (0, 0) → (Q25, 0.70) → (Q75, 1.00) + flat cap beyond Q75
        x_cap_end <- as.numeric(q75) + iqr_val * 0.3
        curve_df <- tibble::tibble(
          x = c(cp1_x, cp2_x, cp3_x, x_cap_end),
          y = c(cp1_y, cp2_y, cp3_y, 1.00)
        )
      } else {
        ## flat cap below Q25 + (Q25, 1.00) → (Q75, 0.70) → (Q75+IQR*7/3, 0.00)
        x_cap_start <- max(0, as.numeric(q25) - iqr_val * 0.3)
        curve_df <- tibble::tibble(
          x = c(x_cap_start, cp1_x, cp2_x, cp3_x),
          y = c(1.00,         cp1_y, cp2_y, cp3_y)
        )
      }

      ## Point markers (the 3 key curve points only)
      points_df <- tibble::tibble(
        x = c(cp1_x, cp2_x, cp3_x),
        y = c(cp1_y, cp2_y, cp3_y)
      )

      ## X-axis range (exact curve bounds, no padding)
      x_lo <- min(curve_df$x)
      x_hi <- max(curve_df$x)

      ggplot2::ggplot() +
        ## Horizontal scoring bands on Y-axis
        ggplot2::annotate("rect", xmin = x_lo, xmax = x_hi,
                           ymin = 0.70, ymax = 1.00,
                           fill = "green", alpha = 0.12) +
        ggplot2::annotate("rect", xmin = x_lo, xmax = x_hi,
                           ymin = 0.30, ymax = 0.70,
                           fill = "yellow", alpha = 0.12) +
        ggplot2::annotate("rect", xmin = x_lo, xmax = x_hi,
                           ymin = 0.00, ymax = 0.30,
                           fill = "red", alpha = 0.12) +
        ## Horizontal dashed lines at score thresholds
        ggplot2::geom_hline(yintercept = 0.70, linetype = "dashed", color = "grey40", linewidth = 0.5) +
        ggplot2::geom_hline(yintercept = 0.30, linetype = "dashed", color = "grey40", linewidth = 0.5) +
        ## Vertical dashed lines at Q25 and Q75
        ggplot2::geom_vline(xintercept = as.numeric(q25), linetype = "dashed", color = "darkgreen", linewidth = 0.5) +
        ggplot2::geom_vline(xintercept = as.numeric(q75), linetype = "dashed", color = "darkgreen", linewidth = 0.5) +
        ## Scoring curve line
        ggplot2::geom_line(data = curve_df, ggplot2::aes(x = x, y = y),
                            color = "steelblue", linewidth = 1.2) +
        ## Key curve points
        ggplot2::geom_point(data = points_df, ggplot2::aes(x = x, y = y),
                             color = "steelblue", size = 3) +
        ## Q25/Q75 labels
        ggplot2::annotate("label", x = as.numeric(q25), y = 0.02,
                           label = paste0("Q25=", round(q25, 1)),
                           size = 3, color = "darkgreen",
                           fill = "white", alpha = 0.7, label.size = 0,
                           label.padding = ggplot2::unit(0.15, "lines")) +
        ggplot2::annotate("label", x = as.numeric(q75), y = 0.02,
                           label = paste0("Q75=", round(q75, 1)),
                           size = 3, color = "darkgreen",
                           fill = "white", alpha = 0.7, label.size = 0,
                           label.padding = ggplot2::unit(0.15, "lines")) +
        ## Band labels on the right
        ggplot2::annotate("text", x = x_hi, y = 0.85, label = "Functioning",
                           hjust = 1, size = 2.8, color = "darkgreen", fontface = "italic") +
        ggplot2::annotate("text", x = x_hi, y = 0.50, label = "At-Risk",
                           hjust = 1, size = 2.8, color = "goldenrod4", fontface = "italic") +
        ggplot2::annotate("text", x = x_hi, y = 0.15, label = "Not Functioning",
                           hjust = 1, size = 2.8, color = "firebrick", fontface = "italic") +
        ggplot2::labs(
          title = paste0(mc$display_name, ": Reference Curve",
                         if (!is.null(stratum_label)) paste0(" (", stratum_label, ")") else ""),
          subtitle = paste0("3-point scoring curve | Q25=", round(q25, 1),
                            ", Q75=", round(q75, 1), " | n=", length(ref_values)),
          x = paste0(mc$display_name, " (", mc$units, ")"),
          y = "Index Score"
        ) +
        ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1)) +
        ggplot2::coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(0, 1.02), expand = FALSE) +
        ggplot2::theme_minimal()
    }, error = function(e) {
      cli::cli_alert_warning("Reference curve plot failed for {metric_key}: {e$message}")
      NULL
    })
  }

  list(
    curve_row = tibble::tibble(
      metric              = metric_key,
      display_name        = mc$display_name,
      n_reference         = length(ref_values),
      median_val          = as.numeric(stats::median(ref_values, na.rm = TRUE)),
      mean_val            = as.numeric(mean(ref_values, na.rm = TRUE)),
      sd_val              = as.numeric(stats::sd(ref_values, na.rm = TRUE)),
      min_val             = as.numeric(min(ref_values, na.rm = TRUE)),
      max_val             = as.numeric(max(ref_values, na.rm = TRUE)),
      q25                 = as.numeric(q25),
      q75                 = as.numeric(q75),
      iqr                 = as.numeric(iqr_val),
      functioning_min     = functioning_min,
      functioning_max     = functioning_max,
      at_risk_min         = at_risk_min,
      at_risk_max         = at_risk_max,
      not_functioning_min = not_functioning_min,
      not_functioning_max = not_functioning_max,
      higher_is_better    = higher_is_better,
      curve_point1_x      = cp1_x,
      curve_point1_y      = cp1_y,
      curve_point2_x      = cp2_x,
      curve_point2_y      = cp2_y,
      curve_point3_x      = cp3_x,
      curve_point3_y      = cp3_y,
      curve_status        = curve_status,
      stratum             = stratum_label %||% NA_character_
    ),
    bar_chart_plot = p_bar,
    curve_plot = p_curve
  )
}


#' Build overlay plot of scoring curves across multiple strata
#'
#' @param curve_rows Combined tibble of curve_row results (one row per stratum)
#' @param metric_config Parsed metric_registry.yaml
#' @return ggplot object with overlaid scoring curves
build_overlay_curve_plot <- function(curve_rows, metric_config) {
  if (nrow(curve_rows) < 2) return(NULL)

  metric_key <- curve_rows$metric[1]
  mc <- metric_config[[metric_key]]
  higher_is_better <- mc$higher_is_better

  ## Build piecewise-linear curve data for each stratum
  all_curves <- purrr::map_dfr(seq_len(nrow(curve_rows)), function(i) {
    row <- curve_rows[i, ]
    if (row$curve_status != "complete") return(NULL)

    cp1_x <- row$curve_point1_x; cp1_y <- row$curve_point1_y
    cp2_x <- row$curve_point2_x; cp2_y <- row$curve_point2_y
    cp3_x <- row$curve_point3_x; cp3_y <- row$curve_point3_y
    iqr_val <- row$iqr

    if (isTRUE(higher_is_better)) {
      x_cap_end <- cp3_x + iqr_val * 0.3
      tibble::tibble(
        x = c(cp1_x, cp2_x, cp3_x, x_cap_end),
        y = c(cp1_y, cp2_y, cp3_y, 1.00),
        stratum = row$stratum
      )
    } else {
      x_cap_start <- max(0, cp1_x - iqr_val * 0.3)
      tibble::tibble(
        x = c(x_cap_start, cp1_x, cp2_x, cp3_x),
        y = c(1.00, cp1_y, cp2_y, cp3_y),
        stratum = row$stratum
      )
    }
  })

  if (nrow(all_curves) < 2) return(NULL)

  ## Key points for each stratum
  all_points <- purrr::map_dfr(seq_len(nrow(curve_rows)), function(i) {
    row <- curve_rows[i, ]
    if (row$curve_status != "complete") return(NULL)
    tibble::tibble(
      x = c(row$curve_point1_x, row$curve_point2_x, row$curve_point3_x),
      y = c(row$curve_point1_y, row$curve_point2_y, row$curve_point3_y),
      stratum = row$stratum
    )
  })

  x_lo <- min(all_curves$x, na.rm = TRUE)
  x_hi <- max(all_curves$x, na.rm = TRUE)

  ggplot2::ggplot() +
    ## Scoring bands (reduced alpha)
    ggplot2::annotate("rect", xmin = x_lo, xmax = x_hi,
                       ymin = 0.70, ymax = 1.00,
                       fill = "green", alpha = 0.08) +
    ggplot2::annotate("rect", xmin = x_lo, xmax = x_hi,
                       ymin = 0.30, ymax = 0.70,
                       fill = "yellow", alpha = 0.08) +
    ggplot2::annotate("rect", xmin = x_lo, xmax = x_hi,
                       ymin = 0.00, ymax = 0.30,
                       fill = "red", alpha = 0.08) +
    ## Threshold lines
    ggplot2::geom_hline(yintercept = 0.70, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 0.30, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    ## Scoring curves per stratum
    ggplot2::geom_line(data = all_curves,
                        ggplot2::aes(x = x, y = y, color = stratum),
                        linewidth = 1.2) +
    ## Key points per stratum
    ggplot2::geom_point(data = all_points,
                         ggplot2::aes(x = x, y = y, color = stratum),
                         size = 3) +
    ggplot2::labs(
      title = paste0(mc$display_name, ": Cross-Stratum Scoring Curves"),
      subtitle = paste0("Strata: ", paste(unique(all_curves$stratum), collapse = ", ")),
      x = paste0(mc$display_name, " (", mc$units, ")"),
      y = "Index Score",
      color = "Stratum"
    ) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    ggplot2::coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(0, 1.02), expand = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}


#' Build overlay distribution plot across multiple strata
#'
#' @param data Full data tibble
#' @param metric_key Metric key
#' @param metric_config Parsed metric_registry.yaml
#' @param strat_var Stratification column name
#' @param levels Character vector of stratum levels to include
#' @return ggplot object with overlaid density + rug plots
build_overlay_bar_chart <- function(data, metric_key, metric_config, strat_var, levels) {
  mc <- metric_config[[metric_key]]
  col_name <- mc$column_name

  plot_data <- data |>
    dplyr::filter(.data[[strat_var]] %in% levels) |>
    dplyr::select(value = dplyr::all_of(col_name), stratum = dplyr::all_of(strat_var)) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(stratum = as.character(stratum))

  if (nrow(plot_data) < 4) return(NULL)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = value, fill = stratum, color = stratum)) +
    ggplot2::geom_density(alpha = 0.35) +
    ggplot2::geom_rug(alpha = 0.7, show.legend = FALSE) +
    ggplot2::labs(
      title = paste0(mc$display_name, ": Cross-Stratum Distributions"),
      subtitle = paste0("Strata: ", paste(levels, collapse = ", ")),
      x = paste0(mc$display_name, " (", mc$units, ")"),
      y = "Density",
      fill = "Stratum",
      color = "Stratum"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}


#' Run reference curve development for all metrics
#'
#' @param data Tibble with derived variables
#' @param metric_config Parsed metric_registry.yaml
#' @param model_selections (deprecated, unused) Tibble from select_final_models()
#' @param diagnostic_summary (deprecated, unused) Tibble from run_all_diagnostics()
#' @param all_models (deprecated, unused) Named list of fitted model objects
#' @return List with: registry (tibble), bar_chart_plots (list), curve_plots (list)
run_all_reference_curves <- function(data, metric_config,
                                      model_selections = NULL,
                                      diagnostic_summary = NULL,
                                      all_models = NULL) {

  cli::cli_alert_info("Building reference curves for all metrics...")

  ## Filter to eligible metrics
  eligible <- purrr::keep(names(metric_config), function(mk) {
    mc <- metric_config[[mk]]
    !(mc$metric_family %in% c("categorical")) && !is.null(mc$higher_is_better)
  })

  cli::cli_alert_info("Processing {length(eligible)} metrics...")

  ## Choose parallel or sequential map
  map_fn <- if (requireNamespace("furrr", quietly = TRUE) &&
                !inherits(future::plan(), "sequential")) {
    function(...) furrr::future_map(..., .options = furrr::furrr_options(seed = TRUE))
  } else {
    purrr::map
  }

  ## Process in parallel
  results_list <- map_fn(eligible, function(metric_key) {
    build_reference_curve(data, metric_key, metric_config)
  })

  ## Combine
  registry <- dplyr::bind_rows(purrr::map(results_list, "curve_row"))

  bar_chart_list <- purrr::map(results_list, "bar_chart_plot")
  non_null_bar <- !vapply(bar_chart_list, is.null, logical(1))
  bar_chart_plots <- purrr::set_names(bar_chart_list[non_null_bar], eligible[non_null_bar])

  curve_list <- purrr::map(results_list, "curve_plot")
  non_null_curve <- !vapply(curve_list, is.null, logical(1))
  curve_plots <- purrr::set_names(curve_list[non_null_curve], eligible[non_null_curve])

  cli::cli_alert_success(
    "Reference curves complete: {nrow(registry)} curves built, {length(bar_chart_plots)} bar charts, {length(curve_plots)} curve plots"
  )

  list(registry = registry, bar_chart_plots = bar_chart_plots, curve_plots = curve_plots)
}
