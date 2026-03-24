## ── 11: Cross-Metric Redundancy Analysis ────────────────────────────────────
## Correlation matrix, PCA, and redundancy flagging across metrics.

library(dplyr)
library(tidyr)
library(ggplot2)
library(cli)

#' Run cross-metric redundancy analysis
#'
#' @param data Tibble with derived variables
#' @param metric_config Parsed metric_registry.yaml
#' @param reference_registry Tibble from run_all_reference_curves()$registry (optional)
#' @return List with: results (tibble), cor_matrix (matrix), plots (list)
run_cross_metric_analysis <- function(data, metric_config, reference_registry = NULL) {

  cli::cli_alert_info("Running cross-metric redundancy analysis...")

  ## ── Extract numeric metrics ───────────────────────────────────────────────
  numeric_metrics <- names(metric_config)[sapply(names(metric_config), function(mk) {
    mc <- metric_config[[mk]]
    mc$metric_family %in% c("continuous", "proportion", "count") &&
      mc$column_name %in% names(data) &&
      isTRUE(mc$include_in_summary)
  })]

  col_names <- sapply(numeric_metrics, function(mk) metric_config[[mk]]$column_name)
  display_names <- sapply(numeric_metrics, function(mk) metric_config[[mk]]$display_name)

  ## Build metrics matrix
  metric_data <- data |>
    dplyr::select(dplyr::all_of(unname(col_names)))

  ## Need at least 5 complete cases
  complete_rows <- complete.cases(metric_data)
  if (sum(complete_rows) < 5) {
    cli::cli_alert_warning("Too few complete cases for cross-metric analysis")
    ## Use pairwise complete observations instead
    use_pairwise <- TRUE
  } else {
    use_pairwise <- FALSE
  }

  ## ── Correlation matrices ──────────────────────────────────────────────────
  use_method <- if (use_pairwise) "pairwise.complete.obs" else "complete.obs"

  pearson_cor <- tryCatch(
    cor(metric_data, method = "pearson", use = use_method),
    error = function(e) NULL
  )

  spearman_cor <- tryCatch(
    cor(metric_data, method = "spearman", use = use_method),
    error = function(e) NULL
  )

  ## ── Flag redundant pairs ──────────────────────────────────────────────────
  results <- tibble::tibble()

  if (!is.null(pearson_cor)) {
    n_metrics <- ncol(pearson_cor)
    for (i in seq_len(n_metrics - 1)) {
      for (j in (i + 1):n_metrics) {
        r_pearson <- pearson_cor[i, j]
        r_spearman <- if (!is.null(spearman_cor)) spearman_cor[i, j] else NA_real_

        results <- dplyr::bind_rows(results, tibble::tibble(
          metric_1      = col_names[i],
          metric_2      = col_names[j],
          display_1     = display_names[i],
          display_2     = display_names[j],
          pearson_r     = r_pearson,
          spearman_rho  = r_spearman,
          abs_pearson   = abs(r_pearson),
          redundant_flag = abs(r_pearson) > 0.80
        ))
      }
    }

    results <- results |> dplyr::arrange(dplyr::desc(abs_pearson))
  }

  ## ── Plots ─────────────────────────────────────────────────────────────────
  plots <- list()

  ## Correlation heatmap
  if (!is.null(pearson_cor)) {
    cor_long <- pearson_cor |>
      as.data.frame() |>
      tibble::rownames_to_column("metric_1") |>
      tidyr::pivot_longer(-metric_1, names_to = "metric_2", values_to = "correlation")

    plots$correlation_heatmap <- ggplot2::ggplot(
      cor_long,
      ggplot2::aes(x = metric_1, y = metric_2, fill = correlation)
    ) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_gradient2(
        low = "blue", mid = "white", high = "red",
        midpoint = 0, limits = c(-1, 1)
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = round(correlation, 2)),
        size = 2.5
      ) +
      ggplot2::labs(
        title = "Cross-Metric Correlation Matrix (Pearson)",
        x = "", y = "", fill = "r"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = ggplot2::element_text(size = 7)
      )
  }

  ## PCA biplot
  if (!use_pairwise && sum(complete_rows) >= 5) {
    pca_data <- metric_data[complete_rows, ]
    pca_result <- tryCatch(prcomp(pca_data, scale. = TRUE, center = TRUE), error = function(e) NULL)

    if (!is.null(pca_result)) {
      ## Variance explained
      var_explained <- summary(pca_result)$importance[2, 1:min(5, ncol(pca_data))]

      ## Scores
      scores <- as.data.frame(pca_result$x[, 1:2])
      scores$obs <- seq_len(nrow(scores))

      ## Loadings
      loadings <- as.data.frame(pca_result$rotation[, 1:2])
      loadings$variable <- rownames(loadings)

      ## Scale loadings for biplot
      scale_factor <- max(abs(scores$PC1), abs(scores$PC2)) /
        max(abs(loadings$PC1), abs(loadings$PC2)) * 0.8

      plots$pca_biplot <- ggplot2::ggplot() +
        ggplot2::geom_point(
          data = scores,
          ggplot2::aes(x = PC1, y = PC2),
          color = "steelblue", size = 2, alpha = 0.7
        ) +
        ggplot2::geom_segment(
          data = loadings,
          ggplot2::aes(x = 0, y = 0, xend = PC1 * scale_factor, yend = PC2 * scale_factor),
          arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
          color = "red", linewidth = 0.7
        ) +
        ggplot2::geom_text(
          data = loadings,
          ggplot2::aes(x = PC1 * scale_factor * 1.1, y = PC2 * scale_factor * 1.1, label = variable),
          color = "red", size = 2.5, hjust = 0
        ) +
        ggplot2::labs(
          title = "PCA Biplot: Cross-Metric Analysis",
          subtitle = paste0(
            "PC1: ", round(var_explained[1] * 100, 1), "% | ",
            "PC2: ", round(var_explained[2] * 100, 1), "%"
          ),
          x = paste0("PC1 (", round(var_explained[1] * 100, 1), "%)"),
          y = paste0("PC2 (", round(var_explained[2] * 100, 1), "%)")
        ) +
        ggplot2::theme_minimal()
    }
  }

  n_redundant <- sum(results$redundant_flag, na.rm = TRUE)
  cli::cli_alert_success(
    "Cross-metric analysis complete: {nrow(results)} pairs, {n_redundant} flagged redundant (|r| > 0.80)"
  )

  list(
    results = results,
    cor_matrix = pearson_cor,
    plots = plots
  )
}
