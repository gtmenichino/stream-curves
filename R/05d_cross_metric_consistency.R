## ── 05d: Cross-Metric Stratification Consistency ─────────────────────────────
## Evaluates how consistently each stratification performs across multiple metrics.

library(dplyr)
library(tidyr)
library(ggplot2)
library(cli)

#' Compute stratification consistency across metrics
#'
#' @param all_layer1_results Named list of screening result tibbles (keyed by metric)
#' @param all_layer2_results Named list of effect size tibbles (keyed by metric)
#' @param metric_config Parsed metric_registry.yaml
#' @param strat_config Parsed stratification_registry.yaml
#' @return List with: consistency_matrix (wide tibble for heatmap),
#'   summary (tibble), heatmap_plot (ggplot)
compute_strat_consistency <- function(all_layer1_results, all_layer2_results,
                                       metric_config, strat_config,
                                       sig_threshold = 0.05) {

  ## ── Combine all Layer 1 results ────────────────────────────────────────────
  l1_combined <- dplyr::bind_rows(all_layer1_results)

  if (nrow(l1_combined) == 0) {
    cli::cli_alert_warning("No screening results available for consistency analysis.")
    return(list(
      consistency_matrix = tibble::tibble(),
      summary = tibble::tibble(),
      heatmap_plot = NULL
    ))
  }

  ## ── Build significance matrix ──────────────────────────────────────────────
  sig_matrix <- l1_combined |>
    dplyr::filter(!is.na(p_value)) |>
    dplyr::mutate(significant = p_value < sig_threshold) |>
    dplyr::select(metric, stratification, significant, p_value)

  ## Early return if all p_values were NA (e.g., all paired/skipped)
  if (nrow(sig_matrix) == 0) {
    cli::cli_alert_warning("No valid screening results after filtering NA p-values.")
    return(list(
      consistency_matrix = tibble::tibble(),
      summary = tibble::tibble(),
      heatmap_plot = NULL
    ))
  }

  ## ── Merge effect sizes if available ────────────────────────────────────────
  if (length(all_layer2_results) > 0) {
    l2_combined <- dplyr::bind_rows(all_layer2_results)
    if (nrow(l2_combined) > 0) {
      sig_matrix <- sig_matrix |>
        dplyr::left_join(
          l2_combined |> dplyr::select(metric, stratification, epsilon_squared, effect_size_label),
          by = c("metric", "stratification")
        )
    } else {
      sig_matrix$epsilon_squared <- NA_real_
      sig_matrix$effect_size_label <- NA_character_
    }
  } else {
    sig_matrix$epsilon_squared <- NA_real_
    sig_matrix$effect_size_label <- NA_character_
  }

  ## ── Per-stratification summary ─────────────────────────────────────────────
  strat_summary <- sig_matrix |>
    dplyr::group_by(stratification) |>
    dplyr::summarise(
      n_metrics_tested = dplyr::n(),
      n_significant = sum(significant, na.rm = TRUE),
      pct_significant = round(n_significant / n_metrics_tested * 100, 1),
      mean_effect_size = mean(epsilon_squared, na.rm = TRUE),
      .groups = "drop"
    )

  ## Guard: replace NaN with NA for clean downstream handling
  strat_summary <- strat_summary |>
    dplyr::mutate(mean_effect_size = dplyr::if_else(is.nan(mean_effect_size), NA_real_, mean_effect_size))

  ## Compute normalized effect size safely
  max_es <- suppressWarnings(max(strat_summary$mean_effect_size, na.rm = TRUE))
  if (!is.finite(max_es) || max_es == 0) max_es <- 1

  strat_summary <- strat_summary |>
    dplyr::mutate(
      norm_effect_size = dplyr::if_else(
        is.na(mean_effect_size),
        0,
        mean_effect_size / max_es
      ),
      consistency_score = round(pct_significant / 100 * 0.6 + norm_effect_size * 0.4, 3)
    ) |>
    dplyr::select(-norm_effect_size) |>
    dplyr::arrange(dplyr::desc(consistency_score))

  ## ── Wide-format matrix for heatmap ─────────────────────────────────────────
  ## Cell value: 1 = significant, 0.5 = tested but not significant, NA = not tested
  heatmap_data <- sig_matrix |>
    dplyr::mutate(
      cell_value = dplyr::case_when(
        significant ~ 1,
        TRUE ~ 0
      )
    ) |>
    dplyr::select(metric, stratification, cell_value)

  wide_matrix <- heatmap_data |>
    tidyr::pivot_wider(names_from = stratification, values_from = cell_value)

  ## ── Heatmap plot ───────────────────────────────────────────────────────────
  heatmap_long <- heatmap_data |>
    dplyr::mutate(
      ## Display names
      metric_label = sapply(metric, function(mk) {
        metric_config[[mk]]$display_name %||% mk
      }),
      strat_label = sapply(stratification, function(sk) {
        strat_config[[sk]]$display_name %||% sk
      }),
      status = dplyr::if_else(cell_value == 1, "Significant", "Not Significant")
    )

  heatmap_plot <- tryCatch({
    ggplot2::ggplot(heatmap_long, ggplot2::aes(
      x = strat_label, y = metric_label, fill = status
    )) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_manual(
        values = c("Significant" = "#27ae60", "Not Significant" = "#e74c3c"),
        na.value = "#ecf0f1",
        name = "Result"
      ) +
      ggplot2::labs(
        title = "Stratification Consistency Across Metrics",
        subtitle = paste0("Green = significant (p < ", sig_threshold, "), Red = not significant"),
        x = "Stratification Variable",
        y = "Metric"
      ) +
      streamcurves_minimal_plot_theme(
        axis_text_x_angle = 45,
        axis_text_x_hjust = 1,
        panel_grid_blank = TRUE
      )
  }, error = function(e) {
    cli::cli_alert_warning("Heatmap plot failed: {e$message}")
    NULL
  })

  list(
    consistency_matrix = wide_matrix,
    summary = strat_summary,
    heatmap_plot = heatmap_plot
  )
}
