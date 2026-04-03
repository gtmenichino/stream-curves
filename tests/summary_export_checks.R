suppressPackageStartupMessages({
  library(shiny)
  library(tibble)
})

source("R/10_reference_curves.R", local = TRUE)
source("app/helpers/summary_page.R", local = TRUE)
source("app/modules/mod_summary_export.R", local = TRUE)

default_output_config <- function() {
  list(
    summary_outputs = list(
      thresholds = list(enabled = TRUE),
      metric_status = list(enabled = TRUE),
      stratification_decisions = list(enabled = TRUE),
      decision_history = list(enabled = TRUE),
      regional_curves = list(enabled = TRUE),
      results_bundle = list(enabled = TRUE)
    ),
    report_products = list(
      summary = list(enabled = TRUE, file = "reports/summary.qmd", format = "pdf", tier = 2),
      dashboard = list(enabled = TRUE, file = "reports/dashboard.qmd", format = "html", tier = 1),
      appendix = list(enabled = TRUE, file = "reports/appendix.qmd", format = "pdf", tier = 3)
    )
  )
}

make_summary_export_rv <- function(metric_config = list(),
                                   strat_config = list(),
                                   summary_available_overrides = list(),
                                   data = tibble(),
                                   precheck_df = tibble(),
                                   phase1_candidates = list(),
                                   all_layer1_results = list(),
                                   phase2_ranking = NULL,
                                   phase2_settings = empty_phase2_settings(),
                                   curve_stratification = list(),
                                   output_config = default_output_config()) {
  list2env(list(
    data = data,
    precheck_df = precheck_df,
    metric_config = metric_config,
    strat_config = strat_config,
    summary_available_overrides = summary_available_overrides,
    phase1_candidates = phase1_candidates,
    all_layer1_results = all_layer1_results,
    all_layer2_results = list(),
    phase2_ranking = phase2_ranking,
    cross_metric_consistency = NULL,
    phase2_settings = phase2_settings,
    phase2_metric_overrides = list(),
    curve_stratification = curve_stratification,
    summary_edit_notes = list(),
    phase3_verification = list(),
    metric_phase_cache = list(),
    completed_metrics = list(),
    decision_log = tibble(),
    stratum_results = list(),
    current_metric = NULL,
    strat_decision_user = NULL,
    output_config = output_config
  ), parent = emptyenv())
}

make_curve_row <- function(metric,
                           display_name,
                           stratum = NA_character_,
                           q25 = 5,
                           q75 = 10,
                           curve_source = "auto",
                           curve_status = "complete") {
  tibble(
    metric = metric,
    display_name = display_name,
    n_reference = 18L,
    median_val = (q25 + q75) / 2,
    mean_val = (q25 + q75) / 2,
    sd_val = 1.5,
    min_val = q25 - 3,
    max_val = q75 + 3,
    q25 = q25,
    q75 = q75,
    iqr = q75 - q25,
    functioning_min = q25,
    functioning_max = q75,
    at_risk_min = q25 - 2,
    at_risk_max = q75 + 2,
    not_functioning_min = q25 - 5,
    not_functioning_max = q75 + 5,
    higher_is_better = TRUE,
    curve_point1_x = q25 - 1,
    curve_point1_y = 0.3,
    curve_point2_x = (q25 + q75) / 2,
    curve_point2_y = 0.7,
    curve_point3_x = q75 + 1,
    curve_point3_y = 1,
    curve_n_points = 3L,
    curve_source = curve_source,
    score_30_metric = q25 - 1,
    score_70_metric = q75,
    score_100_metric = q75 + 1,
    score_30_crossings = list(c(q25 - 1)),
    score_70_crossings = list(c(q75)),
    score_100_crossings = list(c(q75 + 1)),
    score_30_crossing_count = 1L,
    score_70_crossing_count = 1L,
    score_100_crossing_count = 1L,
    functioning_ranges = list(tibble(min = q25, max = q75)),
    at_risk_ranges = list(tibble(min = q25 - 2, max = q25)),
    not_functioning_ranges = list(tibble(min = q25 - 5, max = q25 - 2)),
    curve_status = curve_status,
    stratum = stratum,
    curve_points = list(tibble(metric_value = c(q25 - 1, (q25 + q75) / 2, q75 + 1), index_score = c(0.3, 0.7, 1.0)))
  )
}

add_completed_metric <- function(rv, metric, curve_rows, selected_strat = "none", artifact_mode = "summary") {
  rv$curve_stratification[[metric]] <- selected_strat
  decision_tbl <- build_metric_strat_decision(rv, metric, selected_strat)
  rv$completed_metrics[[metric]] <- list(
    strat_decision = decision_tbl,
    phase4_signature = build_metric_phase4_signature(rv, metric, decision_tbl),
    phase4_artifact_mode = artifact_mode,
    phase4_curve_rows = curve_rows
  )
}

check_empty_metric_choices_are_safe <- function() {
  choices <- boxplot_metric_choices(list())
  stopifnot(identical(choices, stats::setNames(character(0), character(0))))
}

check_metric_choices_filter_categorical <- function() {
  metric_config <- list(
    metric_a = list(metric_family = "continuous", display_name = "Metric A"),
    metric_b = list(metric_family = "categorical", display_name = "Metric B"),
    metric_c = list(metric_family = "count", display_name = "Metric C")
  )

  choices <- boxplot_metric_choices(metric_config)
  stopifnot(identical(unname(choices), c("metric_a", "metric_c")))
  stopifnot(identical(names(choices), c("Metric A", "Metric C")))
}

check_empty_strat_choices_are_safe <- function() {
  rv <- make_summary_export_rv()
  choices <- boxplot_strat_choices(rv, "metric_a")
  stopifnot(identical(choices, stats::setNames(character(0), character(0))))
}

check_strat_choices_follow_allowed_metric_strats <- function() {
  rv <- make_summary_export_rv(
    metric_config = list(
      metric_a = list(
        allowed_stratifications = c("strat_a", "strat_b", "strat_missing"),
        metric_family = "continuous",
        display_name = "Metric A"
      )
    ),
    strat_config = list(
      strat_a = list(display_name = "Strat A"),
      strat_b = list(display_name = "Strat B")
    )
  )

  choices <- boxplot_strat_choices(rv, "metric_a")
  stopifnot(identical(unname(choices), c("strat_a", "strat_b")))
  stopifnot(identical(names(choices), c("Strat A", "Strat B")))
}

check_stratified_threshold_table_uses_interval_unions <- function() {
  curve_rows <- tibble(
    stratum = c("A", "B"),
    curve_status = c("complete", "insufficient_data"),
    functioning_ranges = list(
      tibble(min = c(7), max = c(13)),
      NULL
    ),
    at_risk_ranges = list(
      tibble(min = c(3, 13), max = c(7, 17)),
      NULL
    ),
    not_functioning_ranges = list(
      tibble(min = c(0, 17), max = c(3, 20)),
      NULL
    )
  )

  tbl <- build_stratified_threshold_table(curve_rows)

  stopifnot(identical(tbl$A, c(
    "7.00 - 13.00",
    "3.00 - 7.00, 13.00 - 17.00",
    "0.00 - 3.00, 17.00 - 20.00"
  )))
  stopifnot(identical(tbl$B, rep("N/A", 3)))
}

make_current_state_test_rv <- function() {
  metric_config <- list(
    metric_a = list(
      metric_family = "continuous",
      display_name = "Metric A",
      units = "units",
      higher_is_better = TRUE,
      allowed_stratifications = "eco",
      include_in_summary = TRUE,
      min_sample_size = 2L
    ),
    metric_b = list(
      metric_family = "continuous",
      display_name = "Metric B",
      units = "units",
      higher_is_better = TRUE,
      allowed_stratifications = character(0),
      include_in_summary = TRUE,
      min_sample_size = 2L
    )
  )

  strat_config <- list(
    eco = list(display_name = "Ecoregion", column_name = "eco")
  )

  rv <- make_summary_export_rv(
    metric_config = metric_config,
    strat_config = strat_config,
    data = tibble(eco = c("A", "A", "B", "B", "B")),
    precheck_df = tibble(metric = c("metric_a", "metric_b"), n_obs = c(5L, 5L)),
    phase1_candidates = list(
      metric_a = tibble(
        metric = "metric_a",
        stratification = "eco",
        p_value = 0.01,
        epsilon_squared = 0.2,
        effect_size_label = "medium",
        min_group_n = 2L,
        candidate_status = "promising",
        reviewer_note = ""
      ),
      metric_b = tibble()
    ),
    all_layer1_results = list(
      metric_a = tibble(
        metric = "metric_a",
        stratification = "eco",
        p_value = 0.01,
        n_groups = 2L,
        min_group_n = 2L
      )
    ),
    phase2_ranking = tibble(
      stratification = "eco",
      consistency_score = 0.8,
      n_promising = 1L,
      n_possible = 0L,
      n_not_promising = 0L,
      pct_promising_possible = 1,
      tier = "Broad-Use Candidate"
    ),
    phase2_settings = list(
      support_threshold = 0.6,
      sig_threshold = 0.05,
      metric_filter = c("metric_a", "metric_b"),
      strat_filter = "eco"
    )
  )

  add_completed_metric(
    rv,
    "metric_a",
    dplyr::bind_rows(
      make_curve_row("metric_a", "Metric A", stratum = "A", q25 = 4, q75 = 8, curve_source = "auto"),
      make_curve_row("metric_a", "Metric A", stratum = "B", q25 = 5, q75 = 9, curve_source = "manual")
    ),
    selected_strat = "eco"
  )
  add_completed_metric(
    rv,
    "metric_b",
    make_curve_row("metric_b", "Metric B", q25 = 10, q75 = 14, curve_source = "auto"),
    selected_strat = "none"
  )

  rv$completed_metrics$regional_bankfull <- list(
    type = "regional",
    response = "bankfull_width",
    predictor = "drainage_area",
    stratify = "None",
    model_summary = tibble(
      response = "bankfull_width",
      predictor = "drainage_area",
      group_var = NA_character_,
      group_level = NA_character_,
      n_obs = 12L,
      coefficient_a = 0.45,
      exponent_b = 0.52,
      r_squared = 0.81,
      fit_status = "ok"
    )
  )

  rv$decision_log <- tibble(
    timestamp = "2026-04-03 09:00:00",
    metric = "metric_a",
    decision_stage = "verification",
    phase = "phase3",
    selected_strat = "eco",
    rationale = "Analyst override retained."
  )

  rv
}

check_export_context_uses_current_phase4_rows <- function() {
  rv <- make_current_state_test_rv()
  context <- build_summary_export_context(rv)

  stopifnot(nrow(context$threshold_rows) == 3)
  stopifnot(sum(context$threshold_rows$curve_source == "manual", na.rm = TRUE) == 1)
  stopifnot(any(context$metric_status$metric == "metric_a"))
  stopifnot(isTRUE(context$metric_status$has_manual_curve[context$metric_status$metric == "metric_a"]))
  stopifnot(identical(
    context$metric_status$selected_curve_stratification_label[context$metric_status$metric == "metric_a"],
    "Ecoregion"
  ))
  stopifnot(identical(
    context$current_decisions$recommended_curve_stratification_label[context$current_decisions$metric == "metric_a"],
    "Ecoregion"
  ))
  stopifnot(nrow(context$regional_curves) == 1)
  stopifnot(nrow(context$decision_history) == 1)
  stopifnot(nrow(context$phase2_summary) == 1)
}

check_staged_bundle_contains_current_session_files <- function() {
  rv <- make_current_state_test_rv()
  bundle_dir <- tempfile(pattern = "summary_export_stage_")
  on.exit(unlink(bundle_dir, recursive = TRUE, force = TRUE), add = TRUE)

  staged <- write_summary_export_stage(rv, bundle_dir, include_appendix_plots = FALSE)

  stopifnot(file.exists(staged$files$metric_status))
  stopifnot(file.exists(staged$files$thresholds))
  stopifnot(file.exists(staged$files$stratification_decisions))
  stopifnot(file.exists(staged$files$decision_history))
  stopifnot(file.exists(staged$files$regional_curves))
  stopifnot(file.exists(staged$files$report_context))

  restored <- readRDS(staged$files$report_context)
  stopifnot(identical(restored$session_meta$metric_count, 2L))
  stopifnot(nrow(restored$threshold_rows) == 3)
}

check_export_config_helpers_follow_output_config <- function() {
  rv <- make_current_state_test_rv()
  stopifnot(isTRUE(summary_export_output_enabled(rv, "thresholds")))
  stopifnot(isTRUE(summary_export_report_enabled(rv, "summary")))

  rv$output_config$summary_outputs$decision_history$enabled <- FALSE
  rv$output_config$report_products$appendix$enabled <- FALSE

  stopifnot(!summary_export_output_enabled(rv, "decision_history"))
  stopifnot(!summary_export_report_enabled(rv, "appendix"))
}

check_export_module_and_reports_drop_legacy_sources <- function() {
  module_text <- paste(readLines("app/modules/mod_summary_export.R", warn = FALSE), collapse = "\n")
  stopifnot(!grepl('Summary / Export', module_text, fixed = TRUE))
  stopifnot(!grepl('nav_panel\\("Overview"', module_text))
  stopifnot(!grepl('nav_panel\\("Details"', module_text))
  stopifnot(grepl('downloadButton\\(ns\\("dl_summary_report"', module_text))
  stopifnot(grepl('downloadButton\\(ns\\("dl_dashboard_report"', module_text))
  stopifnot(grepl('downloadButton\\(ns\\("dl_appendix_report"', module_text))

  for (path in c("reports/summary.qmd", "reports/dashboard.qmd", "reports/appendix.qmd")) {
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    stopifnot(grepl("session_dir", text, fixed = TRUE))
    stopifnot(!grepl("run_latest", text, fixed = TRUE))
  }
}

check_empty_metric_choices_are_safe()
check_metric_choices_filter_categorical()
check_empty_strat_choices_are_safe()
check_strat_choices_follow_allowed_metric_strats()
check_stratified_threshold_table_uses_interval_unions()
check_export_context_uses_current_phase4_rows()
check_staged_bundle_contains_current_session_files()
check_export_config_helpers_follow_output_config()
check_export_module_and_reports_drop_legacy_sources()

cat("summary_export_checks: OK\n")
