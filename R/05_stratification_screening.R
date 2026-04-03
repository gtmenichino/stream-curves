## ── 05: Stratification Screening ────────────────────────────────────────────
## Boxplots + Kruskal-Wallis/Wilcoxon for each metric x stratification pair.

library(dplyr)
library(ggpubr)
library(ggplot2)
library(cli)

build_screening_plot_from_spec <- function(plot_spec,
                                           metric_config,
                                           strat_config,
                                           font_profile = "default",
                                           show_points = FALSE,
                                           masked_site_ids = integer(0)) {
  if (is.null(plot_spec) || !is.list(plot_spec)) {
    return(NULL)
  }

  mc <- metric_config[[plot_spec$metric_key %||% ""]]
  sc <- strat_config[[plot_spec$strat_key %||% ""]]
  if (is.null(mc) || is.null(sc)) {
    return(NULL)
  }

  df <- plot_spec$data %||% NULL
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(NULL)
  }

  masked_site_ids <- suppressWarnings(as.integer(masked_site_ids %||% integer(0)))
  masked_site_ids <- masked_site_ids[!is.na(masked_site_ids)]
  if (length(masked_site_ids) > 0 && streamcurves_site_id_column %in% names(df)) {
    df <- df |>
      dplyr::filter(!(.data[[streamcurves_site_id_column]] %in% masked_site_ids))
  }

  if (nrow(df) == 0) {
    return(NULL)
  }

  if (identical(plot_spec$type, "paired")) {
    p_base <- ggpubr::ggboxplot(
      df,
      x = plot_spec$secondary_col,
      y = plot_spec$y_col,
      fill = plot_spec$fill_col %||% plot_spec$secondary_col,
      palette = plot_spec$palette %||% "viridis",
      facet.by = plot_spec$primary_col,
      short.panel.labs = TRUE
    )

    if (isTRUE(show_points)) {
      p_base <- p_base +
        ggplot2::geom_jitter(
          ggplot2::aes_string(
            x = plot_spec$secondary_col,
            y = plot_spec$y_col
          ),
          width = 0.15,
          height = 0,
          alpha = 0.45,
          size = 1.4,
          color = "#2c3e50",
          inherit.aes = FALSE
        )
    }

    comparisons <- plot_spec$comparisons %||% NULL
    if (!is.null(comparisons) && length(comparisons) > 0) {
      p_base <- p_base +
        ggpubr::stat_compare_means(
          comparisons = comparisons,
          method = "wilcox.test",
          method.args = list(exact = FALSE),
          size = streamcurves_geom_text_size(font_profile)
        )
    }

    return(
      p_base +
        ggpubr::stat_compare_means(
          method = "kruskal.test",
          label.y.npc = "top",
          label.x.npc = "center",
          size = streamcurves_geom_text_size(font_profile)
        ) +
        ggplot2::labs(
          title = plot_spec$title %||% paste0(mc$display_name, " by ", sc$display_name),
          x = plot_spec$x_label %||% strat_config[[plot_spec$secondary_key %||% ""]]$display_name %||% plot_spec$secondary_col,
          y = plot_spec$y_label %||% mc$display_name
        ) +
        streamcurves_plot_text_theme(profile = font_profile, legend_position = "none")
    )
  }

  p_base <- ggpubr::ggboxplot(
    df,
    x = plot_spec$x_col,
    y = plot_spec$y_col,
    fill = plot_spec$fill_col %||% plot_spec$x_col,
    palette = plot_spec$palette %||% "viridis"
  )

  if (isTRUE(show_points)) {
    p_base <- p_base +
      ggplot2::geom_jitter(
        ggplot2::aes_string(
          x = plot_spec$x_col,
          y = plot_spec$y_col
        ),
        width = 0.15,
        height = 0,
        alpha = 0.45,
        size = 1.4,
        color = "#2c3e50",
        inherit.aes = FALSE
      )
  }

  comparisons <- plot_spec$comparisons %||% NULL
  if (!is.null(comparisons) && length(comparisons) > 0) {
    p_base <- p_base +
      ggpubr::stat_compare_means(
        comparisons = comparisons,
        method = "wilcox.test",
        method.args = list(exact = FALSE),
        size = streamcurves_geom_text_size(font_profile)
      )
  }

  p_base +
    ggpubr::stat_compare_means(
      method = "kruskal.test",
      label.y.npc = "top",
      label.x.npc = "center",
      size = streamcurves_geom_text_size(font_profile)
    ) +
    ggplot2::labs(
      title = plot_spec$title %||% paste0(mc$display_name, " by ", sc$display_name),
      x = plot_spec$x_label %||% sc$display_name,
      y = plot_spec$y_label %||% mc$display_name
    ) +
    streamcurves_plot_text_theme(profile = font_profile, legend_position = "none")
}

#' Screen a single metric against a single stratification variable
#'
#' @param data Tibble with derived variables
#' @param metric_key Metric key from metric_registry
#' @param strat_key Stratification key from stratification_registry
#' @param metric_config Parsed metric_registry.yaml
#' @param strat_config Parsed stratification_registry.yaml
#' @param compute_pairwise Whether to compute pairwise Wilcoxon results
#' @param build_plot Whether to build the screening plot
#' @return List with: result_row (tibble), pairwise_df (tibble), plot (ggplot)
screen_stratification <- function(data, metric_key, strat_key,
                                   metric_config, strat_config,
                                   compute_pairwise = TRUE,
                                   build_plot = TRUE) {

  mc <- metric_config[[metric_key]]
  sc <- strat_config[[strat_key]]
  col_name <- mc$column_name

  ## ── Handle paired stratifications ─────────────────────────────────────────
  if (!is.null(sc$type) && sc$type == "paired") {
    return(screen_paired_stratification(
      data, metric_key, strat_key,
      metric_config, strat_config,
      build_plot = build_plot
    ))
  }

  strat_col <- sc$column_name

  ## Check columns exist
  if (!col_name %in% names(data) || !strat_col %in% names(data)) {
    return(list(
      result_row = tibble::tibble(
        metric = metric_key, stratification = strat_key,
        test = NA_character_, statistic = NA_real_, p_value = NA_real_,
        n_groups = NA_integer_, min_group_n = NA_integer_,
        classification = "skipped", reason = "column_missing"
      ),
      pairwise_df = tibble::tibble(),
      plot = NULL,
      plot_spec = NULL
    ))
  }

  ## Prepare data: drop NAs in both metric and stratification
  plot_cols <- unique(c(
    col_name,
    strat_col,
    intersect(c(streamcurves_site_id_column, streamcurves_site_label_column), names(data))
  ))

  df <- data |>
    dplyr::select(dplyr::all_of(plot_cols)) |>
    tidyr::drop_na()

  ## Ensure stratification is factor
  df[[strat_col]] <- factor(df[[strat_col]])
  df <- df |> dplyr::filter(!is.na(.data[[strat_col]]))

  ## Group sizes
  group_n <- df |>
    dplyr::count(.data[[strat_col]], name = "n") |>
    dplyr::filter(n > 0)

  n_groups <- nrow(group_n)
  min_n <- min(group_n$n)
  min_group_size <- sc$min_group_size %||% 5

  ## Skip if too few groups or insufficient group size
  if (n_groups < 2) {
    return(list(
      result_row = tibble::tibble(
        metric = metric_key, stratification = strat_key,
        test = NA_character_, statistic = NA_real_, p_value = NA_real_,
        n_groups = n_groups, min_group_n = min_n,
        classification = "rejected_sparse", reason = "fewer_than_2_groups"
      ),
      pairwise_df = tibble::tibble(),
      plot = NULL,
      plot_spec = NULL
    ))
  }

  ## Filter out groups below min_group_size for testing
  valid_groups <- group_n |> dplyr::filter(n >= min_group_size)
  if (nrow(valid_groups) < 2) {
    return(list(
      result_row = tibble::tibble(
        metric = metric_key, stratification = strat_key,
        test = NA_character_, statistic = NA_real_, p_value = NA_real_,
        n_groups = n_groups, min_group_n = min_n,
        classification = "rejected_sparse",
        reason = paste0("fewer_than_2_groups_with_n>=", min_group_size)
      ),
      pairwise_df = tibble::tibble(),
      plot = NULL,
      plot_spec = NULL
    ))
  }

  ## ── Kruskal-Wallis test ───────────────────────────────────────────────────
  formula <- as.formula(paste0("`", col_name, "` ~ `", strat_col, "`"))

  kw_result <- tryCatch(
    kruskal.test(formula, data = df),
    error = function(e) NULL
  )

  if (is.null(kw_result)) {
    test_name <- NA_character_
    test_stat <- NA_real_
    test_p <- NA_real_
  } else {
    test_name <- "kruskal_wallis"
    test_stat <- kw_result$statistic
    test_p <- kw_result$p.value
  }

  ## ── Pairwise Wilcoxon ────────────────────────────────────────────────────
  pairwise_df <- tibble::tibble()
  if (isTRUE(compute_pairwise) &&
      !is.null(sc$pairwise_comparisons) &&
      length(sc$pairwise_comparisons) > 0) {
    pairwise_rows <- purrr::map_dfr(sc$pairwise_comparisons, function(pair) {
      g1 <- pair[[1]]
      g2 <- pair[[2]]
      d1 <- df |> dplyr::filter(.data[[strat_col]] == g1) |> dplyr::pull(!!col_name)
      d2 <- df |> dplyr::filter(.data[[strat_col]] == g2) |> dplyr::pull(!!col_name)

      if (length(d1) < 2 || length(d2) < 2) {
        return(tibble::tibble(
          metric = metric_key, stratification = strat_key,
          group1 = g1, group2 = g2,
          n1 = length(d1), n2 = length(d2),
          statistic = NA_real_, p_value = NA_real_, p_adjusted = NA_real_
        ))
      }

      wt <- tryCatch(
        wilcox.test(d1, d2, exact = FALSE),
        error = function(e) NULL
      )

      tibble::tibble(
        metric = metric_key, stratification = strat_key,
        group1 = g1, group2 = g2,
        n1 = length(d1), n2 = length(d2),
        statistic = if (!is.null(wt)) wt$statistic else NA_real_,
        p_value = if (!is.null(wt)) wt$p.value else NA_real_,
        p_adjusted = NA_real_
      )
    })

    ## BH adjustment
    if (nrow(pairwise_rows) > 0 && any(!is.na(pairwise_rows$p_value))) {
      pairwise_rows$p_adjusted <- p.adjust(pairwise_rows$p_value, method = "BH")
    }

    pairwise_df <- pairwise_rows
  }

  ## ── Classification ────────────────────────────────────────────────────────
  classification <- "not_selected"
  if (!is.na(test_p) && test_p < 0.05) {
    classification <- "selected"
  }
  if (min_n < min_group_size) {
    classification <- paste0(classification, "_sparse")
  }

  ## ── Boxplot ───────────────────────────────────────────────────────────────
  p <- NULL
  group_labels <- group_n |>
    dplyr::mutate(label = paste0(.data[[strat_col]], "\n(n=", n, ")"))

  label_map <- stats::setNames(group_labels$label, group_labels[[strat_col]])
  df$group_label <- label_map[as.character(df[[strat_col]])]

  comparisons <- NULL
  if (!is.null(sc$pairwise_comparisons) && length(sc$pairwise_comparisons) > 0) {
    comparisons <- lapply(sc$pairwise_comparisons, function(pair) {
      c(label_map[pair[[1]]], label_map[pair[[2]]])
    })
    comparisons <- Filter(function(x) all(!is.na(x)), comparisons)
    if (length(comparisons) == 0) comparisons <- NULL
  }

  plot_spec <- list(
    type = "standard",
    metric_key = metric_key,
    strat_key = strat_key,
    data = df,
    x_col = "group_label",
    y_col = col_name,
    fill_col = "group_label",
    comparisons = comparisons,
    title = paste0(mc$display_name, " by ", sc$display_name),
    x_label = sc$display_name,
    y_label = mc$display_name,
    palette = "viridis"
  )

  if (isTRUE(build_plot)) {
    p <- tryCatch(
      build_screening_plot_from_spec(plot_spec, metric_config, strat_config, show_points = FALSE),
      error = function(e) {
        cli::cli_alert_warning("Plot failed for {metric_key} x {strat_key}: {e$message}")
        NULL
      }
    )
  }

  list(
    result_row = tibble::tibble(
      metric = metric_key,
      stratification = strat_key,
      test = test_name,
      statistic = as.numeric(test_stat),
      p_value = test_p,
      n_groups = n_groups,
      min_group_n = min_n,
      classification = classification,
      reason = NA_character_
    ),
    pairwise_df = pairwise_df,
    plot = p,
    plot_spec = plot_spec
  )
}


#' Screen a paired stratification (faceted boxplot)
#'
#' @param data Tibble
#' @param metric_key Metric key
#' @param strat_key Stratification key (paired type)
#' @param metric_config Parsed metric_registry.yaml
#' @param strat_config Parsed stratification_registry.yaml
#' @param build_plot Whether to build the screening plot
#' @return List with result_row, pairwise_df, plot
screen_paired_stratification <- function(data, metric_key, strat_key,
                                          metric_config, strat_config,
                                          build_plot = TRUE) {

  mc <- metric_config[[metric_key]]
  sc <- strat_config[[strat_key]]
  col_name <- mc$column_name

  primary_key <- sc$primary
  secondary_key <- sc$secondary
  primary_col <- strat_config[[primary_key]]$column_name
  secondary_col <- strat_config[[secondary_key]]$column_name

  ## Check columns exist
  if (!all(c(col_name, primary_col, secondary_col) %in% names(data))) {
    return(list(
      result_row = tibble::tibble(
        metric = metric_key, stratification = strat_key,
        test = NA_character_, statistic = NA_real_, p_value = NA_real_,
        n_groups = NA_integer_, min_group_n = NA_integer_,
        classification = "skipped", reason = "column_missing"
      ),
      pairwise_df = tibble::tibble(),
      plot = NULL,
      plot_spec = NULL
    ))
  }

  plot_cols <- unique(c(
    col_name,
    primary_col,
    secondary_col,
    intersect(c(streamcurves_site_id_column, streamcurves_site_label_column), names(data))
  ))

  df <- data |>
    dplyr::select(dplyr::all_of(plot_cols)) |>
    tidyr::drop_na()

  df[[primary_col]] <- factor(df[[primary_col]])
  df[[secondary_col]] <- factor(df[[secondary_col]])

  ## Cell sizes
  cell_n <- df |>
    dplyr::count(.data[[primary_col]], .data[[secondary_col]], name = "n")

  min_cell_n <- min(cell_n$n)
  n_cells <- nrow(cell_n)

  ## Get secondary comparisons
  sec_comparisons <- strat_config[[secondary_key]]$pairwise_comparisons

  comparisons_list <- NULL
  if (!is.null(sec_comparisons) && length(sec_comparisons) > 0) {
    comparisons_list <- lapply(sec_comparisons, function(pair) {
      c(as.character(pair[[1]]), as.character(pair[[2]]))
    })
  }

  p <- NULL
  plot_spec <- list(
    type = "paired",
    metric_key = metric_key,
    strat_key = strat_key,
    data = df,
    primary_key = primary_key,
    secondary_key = secondary_key,
    primary_col = primary_col,
    secondary_col = secondary_col,
    y_col = col_name,
    fill_col = secondary_col,
    comparisons = comparisons_list,
    title = paste0(mc$display_name, " by ", sc$display_name),
    x_label = strat_config[[secondary_key]]$display_name,
    y_label = mc$display_name,
    palette = "viridis"
  )

  if (isTRUE(build_plot)) {
    p <- tryCatch(
      build_screening_plot_from_spec(plot_spec, metric_config, strat_config, show_points = FALSE),
      error = function(e) {
        cli::cli_alert_warning("Paired plot failed for {metric_key} x {strat_key}: {e$message}")
        NULL
      }
    )
  }

  ## Classification
  classification <- "exploratory_only"
  if (min_cell_n < (sc$min_group_size %||% 5)) {
    classification <- "rejected_sparse"
  }

  list(
    result_row = tibble::tibble(
      metric = metric_key,
      stratification = strat_key,
      test = "paired_kruskal",
      statistic = NA_real_,
      p_value = NA_real_,
      n_groups = n_cells,
      min_group_n = min_cell_n,
      classification = classification,
      reason = if (min_cell_n < 5) "cells_with_n_lt_5" else NA_character_
    ),
    pairwise_df = tibble::tibble(),
    plot = p,
    plot_spec = plot_spec
  )
}


#' Run stratification screening for all metric x stratification combinations
#'
#' @param data Tibble with derived variables
#' @param metric_config Parsed metric_registry.yaml
#' @param strat_config Parsed stratification_registry.yaml
#' @return List with: results (tibble), pairwise (tibble), plots (named list)
run_all_stratification_screening <- function(data, metric_config, strat_config) {

  cli::cli_alert_info("Running stratification screening for all metrics...")

  ## Build work list: all metric x stratification combinations
  work_items <- list()
  for (metric_key in names(metric_config)) {
    mc <- metric_config[[metric_key]]
    if (mc$metric_family %in% c("categorical")) next
    allowed_strats <- mc$allowed_stratifications
    if (is.null(allowed_strats)) next
    for (strat_key in allowed_strats) {
      if (!strat_key %in% names(strat_config)) next
      work_items <- c(work_items, list(list(metric = metric_key, strat = strat_key)))
    }
  }

  cli::cli_alert_info("Screening {length(work_items)} metric x stratification combinations...")

  ## Choose parallel or sequential map
  map_fn <- if (requireNamespace("furrr", quietly = TRUE) &&
                !inherits(future::plan(), "sequential")) {
    function(...) furrr::future_map(..., .options = furrr::furrr_options(seed = TRUE))
  } else {
    purrr::map
  }

  ## Process all combinations
  results_list <- map_fn(work_items, function(item) {
    screen_stratification(data, item$metric, item$strat, metric_config, strat_config)
  })

  ## Combine results
  all_results <- dplyr::bind_rows(purrr::map(results_list, "result_row"))
  all_pairwise <- dplyr::bind_rows(purrr::keep(purrr::map(results_list, "pairwise_df"), ~ nrow(.) > 0))

  plot_names <- purrr::map_chr(work_items, ~ paste0(.x$metric, "_", .x$strat))
  plot_list <- purrr::map(results_list, "plot")
  non_null <- !vapply(plot_list, is.null, logical(1))
  all_plots <- purrr::set_names(plot_list[non_null], plot_names[non_null])

  cli::cli_alert_success(
    "Screening complete: {nrow(all_results)} combinations, {length(all_plots)} plots"
  )

  list(
    results = all_results,
    pairwise = all_pairwise,
    plots = all_plots
  )
}
