suppressPackageStartupMessages({
  library(tibble)
})

source("R/00_plot_theme.R", local = TRUE)
source("R/10_reference_curves.R", local = TRUE)
source("app/helpers/summary_page.R", local = TRUE)

make_decision <- function(metric = "metric_a", selected_strat = NA_character_, decision_type = "none") {
  tibble(
    metric = metric,
    decision_type = decision_type,
    selected_strat = selected_strat,
    selected_p_value = NA_real_,
    selected_n_groups = NA_integer_,
    selected_min_n = NA_integer_,
    runner_up_strat = NA_character_,
    runner_up_p_value = NA_real_,
    needs_review = FALSE,
    review_reason = NA_character_,
    notes = NA_character_
  )
}

make_summary_rv <- function() {
  list2env(list(
    current_metric = "metric_a",
    curve_stratification = list(metric_a = "none", metric_b = "none", metric_c = "none"),
    metric_phase_cache = list(),
    completed_metrics = list(),
    stratum_results = list(),
    summary_available_overrides = list(),
    phase2_ranking = NULL,
    phase1_candidates = list(),
    phase2_metric_overrides = list(),
    phase3_verification = list(),
    phase3_patterns = NULL,
    phase3_feasibility = NULL,
    phase1_screening = NULL,
    phase1_effect_sizes = NULL,
    precheck_df = NULL,
    data_fingerprint = "fingerprint-1",
    config_version = 1L,
    metric_config = list(
      metric_a = list(metric_family = "continuous", display_name = "Metric A", allowed_stratifications = "strat_a"),
      metric_b = list(metric_family = "continuous", display_name = "Metric B", allowed_stratifications = character(0)),
      metric_c = list(metric_family = "continuous", display_name = "Metric C", allowed_stratifications = character(0))
    ),
    strat_config = list(
      strat_a = list(display_name = "Strat A")
    ),
    data = data.frame(value = 1:5)
  ), parent = emptyenv())
}

check_manual_curve_info_from_rows_for_unstratified_curve <- function() {
  rows <- tibble(
    metric = "metric_a",
    curve_source = "manual",
    curve_status = "complete",
    stratum = NA_character_
  )

  info <- manual_curve_info_from_rows(rows, "metric_a", "Metric A")

  stopifnot(isTRUE(info$has_manual_curve))
  stopifnot(identical(info$manual_curve_count, 1L))
  stopifnot(identical(info$manual_strata, character(0)))
  stopifnot(identical(info$summary_label, "Manual curve"))
  stopifnot(identical(info$selection_label, "Metric A (manual curve)"))
}

check_manual_curve_info_from_rows_for_stratified_curve <- function() {
  rows <- tibble(
    metric = "metric_a",
    curve_source = c("manual", "auto", "manual"),
    curve_status = "complete",
    stratum = c("Low", "High", "Mid")
  )

  info <- manual_curve_info_from_rows(rows, "metric_a", "Metric A")

  stopifnot(isTRUE(info$has_manual_curve))
  stopifnot(identical(info$manual_curve_count, 2L))
  stopifnot(identical(info$manual_strata, c("Low", "Mid")))
  stopifnot(identical(info$summary_label, "Manual strata: Low, Mid"))
  stopifnot(identical(info$selection_label, "Metric A (manual strata: Low, Mid)"))
}

check_summary_recompute_plan_only_flags_current_manual_curves <- function() {
  rv <- make_summary_rv()

  current_decision_a <- make_decision(metric = "metric_a")
  current_signature_a <- build_metric_phase4_signature(rv, "metric_a", current_decision_a)
  rv$completed_metrics[["metric_a"]] <- list(
    strat_decision = current_decision_a,
    reference_curve = list(curve_row = tibble(metric = "metric_a", curve_source = "manual", curve_status = "complete")),
    phase4_signature = current_signature_a
  )

  current_decision_b <- make_decision(metric = "metric_b")
  current_signature_b <- build_metric_phase4_signature(rv, "metric_b", current_decision_b)
  rv$completed_metrics[["metric_b"]] <- list(
    strat_decision = current_decision_b,
    reference_curve = list(curve_row = tibble(metric = "metric_b", curve_source = "auto", curve_status = "complete")),
    phase4_signature = current_signature_b
  )

  stale_decision_c <- make_decision(metric = "metric_c")
  stale_signature_c <- modifyList(
    build_metric_phase4_signature(rv, "metric_c", stale_decision_c),
    list(data_fingerprint = "stale-fingerprint")
  )
  rv$completed_metrics[["metric_c"]] <- list(
    strat_decision = stale_decision_c,
    reference_curve = list(curve_row = tibble(metric = "metric_c", curve_source = "manual", curve_status = "complete")),
    phase4_signature = stale_signature_c
  )

  plan <- build_summary_recompute_plan(rv, c("metric_a", "metric_b", "metric_c"))

  stopifnot(identical(plan$manual_metrics, "metric_a"))
  stopifnot(identical(sort(plan$auto_metrics), c("metric_b", "metric_c")))
  stopifnot(nrow(plan$manual_info) == 1)
  stopifnot(identical(plan$manual_info$summary_label[[1]], "Manual curve"))
}

check_summary_recompute_metric_resolution <- function() {
  targets <- resolve_summary_recompute_metrics(
    auto_metrics = c("metric_b", "metric_c"),
    manual_metrics = c("metric_a", "metric_d"),
    selected_manual_metrics = c("metric_d", "metric_x")
  )

  stopifnot(identical(targets, c("metric_b", "metric_c", "metric_d")))
}

check_summary_page_static_manual_recompute_wiring <- function() {
  summary_lines <- readLines("app/modules/mod_summary_page.R", warn = FALSE)
  summary_text <- paste(summary_lines, collapse = "\n")

  stopifnot(any(grepl("pending_bulk_recompute <- reactiveVal\\(NULL\\)", summary_lines)))
  stopifnot(any(grepl("pending_row_recompute <- reactiveVal\\(NULL\\)", summary_lines)))
  stopifnot(any(grepl("show_bulk_manual_recompute_modal <- function", summary_lines, fixed = TRUE)))
  stopifnot(any(grepl("show_row_manual_recompute_modal <- function", summary_lines, fixed = TRUE)))
  stopifnot(any(grepl("Manual Curves Will Be Overwritten", summary_lines, fixed = TRUE)))
  stopifnot(any(grepl("Overwrite Manual Curve\\?", summary_lines)))
  stopifnot(any(grepl("bulk_manual_recompute_metrics", summary_lines, fixed = TRUE)))
  stopifnot(any(grepl("confirm_bulk_recompute", summary_lines, fixed = TRUE)))
  stopifnot(any(grepl("confirm_row_recompute", summary_lines, fixed = TRUE)))
  stopifnot(any(grepl("Manual curves were preserved\\.", summary_lines)))
  stopifnot(any(grepl("row_data\\$manual_curve_label", summary_lines)))
  stopifnot(grepl("build_summary_recompute_plan\\(rv, summary_metrics\\(\\)\\)", summary_text))
  stopifnot(grepl("resolve_summary_recompute_metrics\\(", summary_text))
}

check_manual_curve_info_from_rows_for_unstratified_curve()
check_manual_curve_info_from_rows_for_stratified_curve()
check_summary_recompute_plan_only_flags_current_manual_curves()
check_summary_recompute_metric_resolution()
check_summary_page_static_manual_recompute_wiring()

cat("summary_page_manual_recompute_checks: OK\n")
