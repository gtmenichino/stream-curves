suppressPackageStartupMessages({
  library(shiny)
  library(tibble)
})

source("R/00_plot_theme.R", local = TRUE)
source("R/10_reference_curves.R", local = TRUE)
source("app/helpers/phase_tracker.R", local = TRUE)
source("app/helpers/summary_page.R", local = TRUE)
source("app/helpers/badges.R", local = TRUE)

make_decision <- function(metric = "metric_a", selected_strat = "strat_a", decision_type = "single") {
  tibble(
    metric = metric,
    decision_type = decision_type,
    selected_strat = if (identical(decision_type, "single")) selected_strat else NA_character_,
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

make_rv <- function() {
  list2env(list(
    current_metric = "metric_a",
    phase1_screening = NULL,
    phase1_effect_sizes = NULL,
    phase3_patterns = NULL,
    phase3_feasibility = NULL,
    phase3_verification = list(),
    curve_stratification = list(),
    strat_decision_user = NULL,
    reference_curve = NULL,
    current_stratum_level = NULL,
    phase4_data = NULL,
    metric_phase_cache = list(),
    completed_metrics = list(),
    stratum_results = list(),
    phase1_candidates = list(),
    phase2_metric_overrides = list(),
    phase2_ranking = NULL,
    summary_available_overrides = list(),
    data_fingerprint = "fingerprint-1",
    config_version = 1L,
    metric_config = list(
      metric_a = list(
        allowed_stratifications = "strat_a",
        metric_family = "continuous",
        display_name = "Metric A"
      )
    ),
    strat_config = list(
      strat_a = list(display_name = "Strat A")
    ),
    data = data.frame(value = 1:5)
  ), parent = emptyenv())
}

check_phase1_refresh_flag <- function() {
  rv <- make_rv()
  rv$metric_phase_cache[["metric_a"]] <- list(
    phase1_screening = list(results = tibble(stratification = "strat_a")),
    phase1_artifact_mode = "summary"
  )

  stopifnot(isTRUE(metric_needs_phase1_artifact_refresh(rv, "metric_a")))

  rv$metric_phase_cache[["metric_a"]]$phase1_artifact_mode <- "full"
  stopifnot(!isTRUE(metric_needs_phase1_artifact_refresh(rv, "metric_a")))
}

check_phase3_refresh_flag <- function() {
  rv <- make_rv()
  rv$phase3_verification[["metric_a"]] <- list(selected_strat = "none")
  rv$metric_phase_cache[["metric_a"]] <- list(phase3_artifact_mode = "summary")

  stopifnot(isTRUE(metric_needs_phase3_artifact_refresh(rv, "metric_a")))

  rv$metric_phase_cache[["metric_a"]]$phase3_artifact_mode <- "full"
  stopifnot(!isTRUE(metric_needs_phase3_artifact_refresh(rv, "metric_a")))
}

check_phase4_cache_rules <- function() {
  rv <- make_rv()
  decision <- make_decision()
  signature <- build_metric_phase4_signature(rv, "metric_a", decision)

  rv$metric_phase_cache[["metric_a"]] <- list(
    strat_decision_user = decision,
    reference_curve = list(curve_row = tibble(metric = "metric_a")),
    phase4_signature = signature
  )

  stopifnot(isTRUE(metric_has_phase4_cache(rv, "metric_a", decision)))

  rv$metric_phase_cache[["metric_a"]]$reference_curve <- NULL
  rv$metric_phase_cache[["metric_a"]]$stratum_results <- list(
    low = list(reference_curve = list(curve_row = tibble(metric = "metric_a", stratum = "low"))),
    high = list(reference_curve = list(curve_row = tibble(metric = "metric_a", stratum = "high")))
  )

  stopifnot(isTRUE(metric_has_phase4_cache(rv, "metric_a", decision)))

  changed_decision <- make_decision(selected_strat = "strat_b")
  stopifnot(!isTRUE(metric_has_phase4_cache(rv, "metric_a", changed_decision)))
}

check_phase4_completed_fallback <- function() {
  rv <- make_rv()
  decision <- make_decision(decision_type = "none")
  signature <- build_metric_phase4_signature(rv, "metric_a", decision)

  rv$completed_metrics[["metric_a"]] <- list(
    strat_decision = decision,
    reference_curve = list(curve_row = tibble(metric = "metric_a")),
    phase4_signature = signature
  )

  stopifnot(isTRUE(metric_has_phase4_cache(rv, "metric_a", decision)))
}

check_phase4_display_state_prefers_current_cache <- function() {
  rv <- make_reactive_rv()
  decision <- shiny::isolate(get_metric_phase4_decision_state(rv, "metric_a"))
  signature <- shiny::isolate(build_metric_phase4_signature(rv, "metric_a", decision))

  shiny::isolate({
    rv$metric_phase_cache[["metric_a"]]$strat_decision_user <- decision
    rv$metric_phase_cache[["metric_a"]]$reference_curve <- list(
      curve_row = tibble(metric = "metric_a", display_source = "cache", curve_status = "complete")
    )
    rv$metric_phase_cache[["metric_a"]]$phase4_signature <- signature
  })

  phase4 <- shiny::isolate(get_metric_phase4_display_state(rv, "metric_a"))
  snapshot <- shiny::isolate(build_metric_summary_snapshot(rv, "metric_a"))

  stopifnot(identical(phase4$source, "cache"))
  stopifnot(nrow(phase4$curve_rows) == 1)
  stopifnot(identical(phase4$curve_rows$display_source[1], "cache"))
  stopifnot(identical(snapshot$phase4_source, "cache"))
  stopifnot(nrow(snapshot$curve_rows) == 1)
}

check_phase4_display_state_prefers_current_completed_when_cache_is_stale <- function() {
  rv <- make_reactive_rv()
  current_decision <- shiny::isolate(get_metric_phase4_decision_state(rv, "metric_a"))
  current_signature <- shiny::isolate(build_metric_phase4_signature(rv, "metric_a", current_decision))
  stale_decision <- make_decision(selected_strat = "strat_b")
  stale_signature <- shiny::isolate(build_metric_phase4_signature(rv, "metric_a", stale_decision))

  shiny::isolate({
    rv$metric_phase_cache[["metric_a"]]$strat_decision_user <- stale_decision
    rv$metric_phase_cache[["metric_a"]]$reference_curve <- list(
      curve_row = tibble(metric = "metric_a", display_source = "stale_cache", curve_status = "complete")
    )
    rv$metric_phase_cache[["metric_a"]]$phase4_signature <- stale_signature

    rv$completed_metrics[["metric_a"]] <- list(
      strat_decision = current_decision,
      reference_curve = list(
        curve_row = tibble(metric = "metric_a", display_source = "completed", curve_status = "complete")
      ),
      phase4_signature = current_signature
    )
  })

  phase4 <- shiny::isolate(get_metric_phase4_display_state(rv, "metric_a"))
  snapshot <- shiny::isolate(build_metric_summary_snapshot(rv, "metric_a"))

  stopifnot(identical(phase4$source, "completed"))
  stopifnot(nrow(phase4$curve_rows) == 1)
  stopifnot(identical(phase4$curve_rows$display_source[1], "completed"))
  stopifnot(identical(snapshot$phase4_source, "completed"))
  stopifnot(identical(snapshot$curve_rows$display_source[1], "completed"))
}

check_phase4_display_state_ignores_stale_results <- function() {
  rv <- make_reactive_rv()
  stale_decision <- make_decision(selected_strat = "strat_b")
  stale_signature <- shiny::isolate(build_metric_phase4_signature(rv, "metric_a", stale_decision))

  shiny::isolate({
    rv$metric_phase_cache[["metric_a"]]$strat_decision_user <- stale_decision
    rv$metric_phase_cache[["metric_a"]]$reference_curve <- list(
      curve_row = tibble(metric = "metric_a", display_source = "stale_cache", curve_status = "complete")
    )
    rv$metric_phase_cache[["metric_a"]]$phase4_signature <- stale_signature

    rv$completed_metrics[["metric_a"]] <- list(
      strat_decision = stale_decision,
      reference_curve = list(
        curve_row = tibble(metric = "metric_a", display_source = "stale_completed", curve_status = "complete")
      ),
      phase4_signature = stale_signature
    )
  })

  phase4 <- shiny::isolate(get_metric_phase4_display_state(rv, "metric_a"))
  snapshot <- shiny::isolate(build_metric_summary_snapshot(rv, "metric_a"))

  stopifnot(identical(phase4$source, "none"))
  stopifnot(nrow(phase4$curve_rows) == 0)
  stopifnot(identical(snapshot$phase4_source, "none"))
  stopifnot(nrow(snapshot$curve_rows) == 0)
  stopifnot(identical(snapshot$status$summary_label, "Not Run"))
}

check_deferred_modal_callbacks <- function() {
  files <- c(
    "app/modules/mod_analysis_workspace.R",
    "app/modules/mod_phase1_exploration.R",
    "app/modules/mod_phase2_consistency.R",
    "app/modules/mod_phase3_verification.R",
    "app/modules/mod_phase4_finalization.R"
  )

  forbidden_patterns <- c("rv\\$current_metric", "rv\\$workspace_modal_type", "rv\\$metric_config")

  for (path in files) {
    lines <- readLines(path, warn = FALSE)
    callback_starts <- grep("session\\$onFlushed\\(function\\(\\)", lines)

    for (start_idx in callback_starts) {
      callback_window <- lines[start_idx:min(length(lines), start_idx + 8L)]
      stopifnot(any(grepl("isolate\\(", callback_window)))
      for (pattern in forbidden_patterns) {
        stopifnot(!any(grepl(pattern, callback_window)))
      }
    }
  }
}

check_analysis_helper_defaults_are_safe <- function() {
  helper_lines <- readLines("app/helpers/phase_tracker.R", warn = FALSE)
  phase1_lines <- readLines("app/modules/mod_phase1_exploration.R", warn = FALSE)
  phase3_lines <- readLines("app/modules/mod_phase3_verification.R", warn = FALSE)

  forbidden_defaults <- c(
    "request_id = rv\\$analysis_tab_request_id",
    "request_id = rv\\$analysis_tab_request_id %\\|\\|% NULL",
    "status = rv\\$analysis_tab_status"
  )

  for (pattern in forbidden_defaults) {
    stopifnot(!any(grepl(pattern, helper_lines)))
    stopifnot(!any(grepl(pattern, phase1_lines)))
    stopifnot(!any(grepl(pattern, phase3_lines)))
  }

  stopifnot(any(grepl("request_id <- request_id %\\|\\|% shiny::isolate\\(rv\\$analysis_tab_request_id", helper_lines)))
  stopifnot(any(grepl("request_id <- request_id %\\|\\|% shiny::isolate\\(rv\\$analysis_tab_request_id", phase1_lines)))
  stopifnot(any(grepl("request_id <- request_id %\\|\\|% shiny::isolate\\(rv\\$analysis_tab_request_id", phase3_lines)))
}

check_phase2_shell_delay <- function() {
  app_lines <- readLines("app/app.R", warn = FALSE)

  stopifnot(any(grepl("workspace_modal_min_shell_seconds <- function", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("identical\\(modal_type, \"phase2\"\\)", app_lines)))
  stopifnot(any(grepl("later::later\\(function\\(", app_lines)))
}

check_modal_launch_is_deferred <- function() {
  rv <- shiny::reactiveValues(
    current_metric = "metric_a",
    workspace_modal_type = NULL,
    workspace_modal_metric = NULL,
    workspace_modal_stage = NULL,
    workspace_modal_error = "stale error",
    workspace_modal_loading_detail = "stale detail",
    workspace_modal_nonce = 0L,
    analysis_tab_request_id = NULL,
    analysis_tab_status = NULL,
    analysis_tab_status_nonce = 0L,
    analysis_tab_preload_tab = NULL,
    analysis_tab_preload_nonce = 0L,
    analysis_tab_preload_completed_tab = NULL,
    analysis_tab_preload_completed_status = NULL,
    analysis_tab_preload_completed_nonce = 0L
  )

  launch_workspace_modal(rv, "phase1", "metric_b")

  shiny::isolate({
    stopifnot(identical(rv$current_metric, "metric_a"))
    stopifnot(identical(rv$workspace_modal_type, "phase1"))
    stopifnot(identical(rv$workspace_modal_metric, "metric_b"))
    stopifnot(identical(rv$workspace_modal_stage, "loading"))
    stopifnot(is.null(rv$workspace_modal_error))
    stopifnot(is.null(rv$workspace_modal_loading_detail))
    stopifnot(identical(rv$workspace_modal_nonce, 1L))
  })

  launch_workspace_modal(rv, "analysis", "metric_c")

  shiny::isolate({
    stopifnot(identical(rv$current_metric, "metric_a"))
    stopifnot(identical(rv$workspace_modal_type, "analysis"))
    stopifnot(identical(rv$workspace_modal_metric, "metric_c"))
    stopifnot(identical(rv$workspace_modal_stage, "loading"))
    stopifnot(identical(rv$workspace_modal_nonce, 2L))
    stopifnot(identical(rv$analysis_tab_request_id, 2L))
    stopifnot(all(vapply(rv$analysis_tab_status, identical, logical(1), "pending")))
  })
}

check_analysis_tab_state_helpers <- function() {
  rv <- shiny::reactiveValues(
    analysis_tab_request_id = NULL,
    analysis_tab_status = NULL,
    analysis_tab_status_nonce = 0L,
    analysis_tab_preload_tab = NULL,
    analysis_tab_preload_nonce = 0L,
    analysis_tab_preload_completed_tab = NULL,
    analysis_tab_preload_completed_status = NULL,
    analysis_tab_preload_completed_nonce = 0L
  )

  reset_analysis_tab_state(rv, 7L)

  shiny::isolate({
    status <- get_analysis_tab_status(rv)
    stopifnot(identical(rv$analysis_tab_request_id, 7L))
    stopifnot(identical(status$reference_curves, "pending"))
    stopifnot(identical(status$exploratory, "pending"))
    stopifnot(identical(status$cross_metric, "pending"))
    stopifnot(identical(status$verification, "pending"))

    request_analysis_tab_preload(rv, "cross_metric", 7L)
    stopifnot(identical(rv$analysis_tab_preload_tab, "cross_metric"))

    stopifnot(isTRUE(set_analysis_tab_status(rv, "cross_metric", "ready", 7L)))
    stopifnot(identical(get_analysis_tab_status(rv, "cross_metric"), "ready"))

    stopifnot(isTRUE(complete_analysis_tab_preload(rv, "cross_metric", "ready", 7L)))
    stopifnot(identical(rv$analysis_tab_preload_completed_tab, "cross_metric"))
    stopifnot(identical(rv$analysis_tab_preload_completed_status, "ready"))

    stopifnot(!isTRUE(set_analysis_tab_status(rv, "verification", "ready", 8L)))
    stopifnot(identical(get_analysis_tab_status(rv, "verification"), "pending"))
  })
}

check_workspace_modal_ui_builders <- function() {
  source("app/modules/mod_phase1_exploration.R", local = TRUE)
  source("app/modules/mod_phase2_consistency.R", local = TRUE)
  source("app/modules/mod_phase3_verification.R", local = TRUE)
  source("app/modules/mod_phase4_finalization.R", local = TRUE)
  source("app/modules/mod_analysis_workspace.R", local = TRUE)

  invisible(htmltools::renderTags(mod_phase1_exploration_ui("phase1", dialog_mode = TRUE)))
  invisible(htmltools::renderTags(mod_phase3_verification_ui("phase3", dialog_mode = TRUE)))
  invisible(htmltools::renderTags(mod_phase4_finalization_ui("phase4", dialog_mode = TRUE)))
  invisible(htmltools::renderTags(mod_analysis_workspace_ui("analysis")))
}

make_reactive_rv <- function() {
  shiny::reactiveValues(
    current_metric = "metric_a",
    phase1_screening = list(
      results = tibble(stratification = "strat_a"),
      pairwise = tibble(),
      plots = list(),
      plot_specs = list()
    ),
    phase1_effect_sizes = tibble(stratification = "strat_a", epsilon_squared = 0.1),
    phase3_patterns = NULL,
    phase3_feasibility = NULL,
    phase3_verification = list(
      metric_a = list(
        finalists = "strat_a",
        pattern_results = NULL,
        feasibility_results = NULL,
        verification_status = list(strat_a = "verified"),
        selected_strat = "strat_a",
        justification = "test"
      )
    ),
    curve_stratification = list(metric_a = "strat_a"),
    strat_decision_user = tibble(
      metric = "metric_a",
      decision_type = "single",
      selected_strat = "strat_a"
    ),
    reference_curve = NULL,
    current_stratum_level = NULL,
    phase4_data = NULL,
    metric_phase_cache = list(
      metric_a = list(
        phase1_screening = list(
          results = tibble(stratification = "strat_a"),
          pairwise = tibble(),
          plots = list(),
          plot_specs = list()
        ),
        phase1_effect_sizes = tibble(stratification = "strat_a", epsilon_squared = 0.1),
        phase1_artifact_mode = "summary",
        phase3_patterns = list(results = tibble(stratification = "strat_a"), plots = list()),
        phase3_feasibility = tibble(stratification = "strat_a", feasibility_flag = "feasible"),
        phase3_artifact_mode = "summary"
      )
    ),
    completed_metrics = list(),
    stratum_results = list(),
    phase1_candidates = list(
      metric_a = tibble(
        metric = "metric_a",
        stratification = "strat_a",
        candidate_status = "promising"
      )
    ),
    phase2_metric_overrides = list(),
    phase2_ranking = NULL,
    summary_available_overrides = list(),
    summary_edit_notes = list(),
    all_layer1_results = list(),
    all_layer2_results = list(),
    metric_config = list(
      metric_a = list(
        allowed_stratifications = "strat_a",
        min_sample_size = 10,
        metric_family = "continuous",
        display_name = "Metric A"
      )
    ),
    strat_config = list(
      strat_a = list(display_name = "Strat A")
    ),
    data_fingerprint = "fingerprint-1",
    config_version = 1L,
    data = data.frame(value = 1:5)
  )
}

check_helper_isolation_works <- function() {
  rv <- make_reactive_rv()

  stopifnot(isTRUE(shiny::isolate(metric_needs_phase1_artifact_refresh(rv, "metric_a"))))

  phase1_state <- shiny::isolate(get_metric_phase1_display_state(rv, "metric_a"))
  stopifnot(!is.null(phase1_state), nrow(phase1_state$results) == 1)

  phase3_state <- shiny::isolate(get_metric_phase3_display_state(rv, "metric_a"))
  stopifnot(!is.null(phase3_state), identical(phase3_state$strats, "strat_a"))
}

check_curve_strat_flow <- function() {
  rv <- make_reactive_rv()

  stopifnot(identical(shiny::isolate(get_metric_curve_stratification(rv, "metric_a")), "strat_a"))

  shiny::isolate(set_metric_curve_stratification(rv, "metric_a", "none", clear_phase4 = FALSE))
  stopifnot(identical(shiny::isolate(get_metric_curve_stratification(rv, "metric_a", fallback_to_auto = FALSE)), "none"))

  decision <- shiny::isolate(get_metric_phase4_decision_state(rv, "metric_a"))
  stopifnot(identical(decision$decision_type[1], "none"))
}

check_summary_page_analysis_controls <- function() {
  lines <- readLines("app/modules/mod_summary_page.R", warn = FALSE)

  stopifnot(any(grepl("open_analysis_", lines, fixed = TRUE)))
  stopifnot(any(grepl("curve_", lines, fixed = TRUE)))
  stopifnot(!any(grepl("open_phase1_", lines, fixed = TRUE)))
  stopifnot(!any(grepl("p1_", lines, fixed = TRUE)))
  stopifnot(any(grepl("Stratification Used for Curves", lines, fixed = TRUE)))
  stopifnot(any(grepl("row_data\\$phase4", lines)))
  stopifnot(!any(grepl("row_data\\$completed", lines)))
}

check_analysis_modal_dropdown_overlays <- function() {
  phase1_lines <- readLines("app/modules/mod_phase1_exploration.R", warn = FALSE)
  phase2_lines <- readLines("app/modules/mod_phase2_consistency.R", warn = FALSE)
  phase3_lines <- readLines("app/modules/mod_phase3_verification.R", warn = FALSE)
  phase4_lines <- readLines("app/modules/mod_phase4_finalization.R", warn = FALSE)
  css_lines <- readLines("app/www/custom.css", warn = FALSE)
  js_lines <- readLines("app/www/custom.js", warn = FALSE)
  modal_container <- 'container = ".modal-dialog.workspace-modal-dialog"'

  stopifnot(any(grepl("picker_container <- function", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl(".modal-dialog.workspace-modal-dialog", phase1_lines, fixed = TRUE)))
  stopifnot(sum(grepl("container = picker_container()", phase1_lines, fixed = TRUE)) >= 1)
  stopifnot(sum(grepl("size = 8", phase1_lines, fixed = TRUE)) >= 1)
  stopifnot(sum(grepl(modal_container, phase2_lines, fixed = TRUE)) >= 2)
  stopifnot(sum(grepl("size = 8", phase2_lines, fixed = TRUE)) >= 2)
  stopifnot(!any(grepl('container = "body"', phase2_lines, fixed = TRUE)))
  stopifnot(any(grepl('shinyWidgets::pickerInput\\(', phase3_lines)))
  stopifnot(any(grepl(modal_container, phase3_lines, fixed = TRUE)))
  stopifnot(any(grepl("size = 8", phase3_lines, fixed = TRUE)))
  stopifnot(!any(grepl('container = "body"', phase3_lines, fixed = TRUE)))
  stopifnot(any(grepl('shinyWidgets::pickerInput\\(', phase4_lines)))
  stopifnot(any(grepl(modal_container, phase4_lines, fixed = TRUE)))
  stopifnot(any(grepl("size = 8", phase4_lines, fixed = TRUE)))
  stopifnot(!any(grepl('container = "body"', phase4_lines, fixed = TRUE)))
  stopifnot(any(grepl(".modal-dialog.workspace-modal-dialog .bs-container.dropdown.bootstrap-select", css_lines, fixed = TRUE)))
  stopifnot(any(grepl("workspace-modal-dialog .bootstrap-select > .dropdown-toggle::after", css_lines, fixed = TRUE)))
  stopifnot(any(grepl('show.bs.select', js_lines, fixed = TRUE)))
  stopifnot(any(grepl('shown.bs.select', js_lines, fixed = TRUE)))
  stopifnot(any(grepl('workspacePickerScrollTop', js_lines, fixed = TRUE)))
}

check_verification_boxplots_rebuild_from_specs <- function() {
  phase3_lines <- readLines("app/modules/mod_phase3_verification.R", warn = FALSE)
  phase3_text <- paste(phase3_lines, collapse = "\n")

  stopifnot(any(grepl("get_metric_phase1_display_state\\(rv, rv\\$current_metric\\)", phase3_lines)))
  stopifnot(any(grepl("phase1_state\\$plot_specs\\[\\[local_sk\\]\\]", phase3_lines)))
  stopifnot(any(grepl("build_screening_plot_from_spec\\(", phase3_lines)))
  stopifnot(any(grepl('font_profile = "large_analysis"', phase3_lines, fixed = TRUE)))
  stopifnot(!grepl(
    'output\\[\\[paste0\\("bp_", local_sk\\)\\]\\] <- renderPlot\\([\\s\\S]*rv\\$phase1_screening\\$plots\\[\\[local_sk\\]\\]',
    phase3_text,
    perl = TRUE
  ))
}

check_analysis_blocking_preload_hooks <- function() {
  app_lines <- readLines("app/app.R", warn = FALSE)
  app_text <- paste(app_lines, collapse = "\n")
  workspace_lines <- readLines("app/modules/mod_analysis_workspace.R", warn = FALSE)
  phase4_lines <- readLines("app/modules/mod_phase4_finalization.R", warn = FALSE)
  ref_curve_lines <- readLines("app/modules/mod_ref_curve.R", warn = FALSE)
  summary_lines <- readLines("app/helpers/summary_page.R", warn = FALSE)
  js_lines <- readLines("app/www/custom.js", warn = FALSE)
  css_lines <- readLines("app/www/custom.css", warn = FALSE)

  stopifnot(any(grepl("analysis_modal_progress_value <- function", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("preload_analysis_modal_tab <- function", app_lines, fixed = TRUE)))
  stopifnot(any(grepl('paste0\\(ready_tabs, \" of \", total_tabs, \" analysis tabs ready\"\\)', app_lines)))
  stopifnot(any(grepl("analysis_launch_total_steps <- function", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("make_analysis_launch_progress_adapter <- function", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("analysis = NULL", app_lines, fixed = TRUE)))
  stopifnot(!any(grepl("show_analysis_launch_progress(request_id, metric)", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("request_analysis_tab_preload", workspace_lines, fixed = TRUE)))
  stopifnot(any(grepl('observeEvent\\(input\\$analysis_tabs, \\{', workspace_lines)))
  stopifnot(any(grepl('set_analysis_tab_status\\(rv, selected_tab, "loading", request_id\\)', workspace_lines)))
  stopifnot(!any(grepl("analysis_tab_preload_completed_nonce", workspace_lines, fixed = TRUE)))
  stopifnot(!any(grepl("analysisTabState", js_lines, fixed = TRUE)))
  stopifnot(!any(grepl("data-analysis-tab-disabled", js_lines, fixed = TRUE)))
  stopifnot(any(grepl("workspace-analysis-loading-status", css_lines, fixed = TRUE)))
  stopifnot(any(grepl("phase4_artifact_mode", summary_lines, fixed = TRUE)))
  stopifnot(any(grepl("phase4_curve_rows", summary_lines, fixed = TRUE)))
  stopifnot(any(grepl('artifact_mode = "summary"', phase4_lines, fixed = TRUE)))
  stopifnot(any(grepl('artifact_mode = "full"', phase4_lines, fixed = TRUE)))
  stopifnot(any(grepl('artifact_mode = "full"', ref_curve_lines, fixed = TRUE)))
  stopifnot(grepl(
    'if \\(identical\\(modal_type, "analysis"\\)\\) \\{[\\s\\S]*ensure_analysis_workspace_server\\(\\)[\\s\\S]*close_analysis_launch_progress\\(force = TRUE\\)[\\s\\S]*prepare_workspace_modal\\(request_id, modal_type, metric\\)[\\s\\S]*show_workspace_modal_dialog\\(title, body_content = body_content\\)[\\s\\S]*remove_analysis_launch_spinner_notification\\(session, request_id\\)',
    app_text,
    perl = TRUE
  ))
}

check_analysis_launch_feedback <- function() {
  app_lines <- readLines("app/app.R", warn = FALSE)
  app_text <- paste(app_lines, collapse = "\n")

  stopifnot(any(grepl("show_analysis_launch_progress <- function", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("update_analysis_launch_progress <- function", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("advance_analysis_launch_progress <- function", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("analysis_launch_total_steps <- function", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("analysis_launch_progress_message <- function", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("show_workspace_modal_dialog <- function", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("notify_workspace_modal_ready <- function", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("close_analysis_launch_progress <- function", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("analysis_launch_progress_message(current$metric)", app_lines, fixed = TRUE)))
  stopifnot(any(grepl('Loading page, please wait\\.', app_lines)))
  stopifnot(any(grepl("launch steps complete", app_lines, fixed = TRUE)))
  stopifnot(!any(grepl("show_analysis_launch_progress(request_id, metric)", app_lines, fixed = TRUE)))
  stopifnot(any(grepl('notify = !identical\\(modal_type, "analysis"\\)', app_lines)))
  stopifnot(any(grepl("close_analysis_launch_progress(request_id)", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("close_analysis_launch_progress(force = TRUE)", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("advance_analysis_launch_progress(request_id, metric = metric, detail = detail)", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("body_content <- shiny::isolate(", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("workspace_modal_body_content(modal_type, stage, title_metric)", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("show_workspace_modal_dialog(title, body_content = body_content)", app_lines, fixed = TRUE)))
  stopifnot(any(grepl("observeEvent(input$workspace_modal_client_ready, {", app_lines, fixed = TRUE)))
  stopifnot(grepl(
    'observeEvent\\(input\\$workspace_modal_client_ready, \\{[\\s\\S]*request_id <- isolate\\(rv\\$workspace_modal_nonce %\\|\\|% 0L\\)[\\s\\S]*modal_type <- rv\\$workspace_modal_type %\\|\\|% NULL[\\s\\S]*if \\(!identical\\(modal_type, "analysis"\\) \\|\\| !modal_request_is_current\\(request_id\\)\\) \\{[\\s\\S]*if \\(identical\\(shiny::isolate\\(rv\\$workspace_modal_stage %\\|\\|% "loading"\\), "ready"\\)\\) \\{[\\s\\S]*notify_workspace_modal_ready\\(request_id\\)[\\s\\S]*remove_analysis_launch_spinner_notification\\(session, request_id\\)[\\s\\S]*close_analysis_launch_progress\\(request_id\\)',
    app_text,
    perl = TRUE
  ))
  stopifnot(grepl(
    'if \\(identical\\(modal_type, "analysis"\\)\\) \\{[\\s\\S]*ensure_analysis_workspace_server\\(\\)[\\s\\S]*close_analysis_launch_progress\\(force = TRUE\\)[\\s\\S]*removeModal\\(\\)[\\s\\S]*prepare_workspace_modal\\(request_id, modal_type, metric\\)[\\s\\S]*title_metric <- shiny::isolate\\(rv\\$workspace_modal_metric %\\|\\|% metric\\)[\\s\\S]*title <- workspace_modal_title\\(modal_type, title_metric\\)[\\s\\S]*stage <- shiny::isolate\\(rv\\$workspace_modal_stage %\\|\\|% "loading"\\)[\\s\\S]*body_content <- shiny::isolate\\([\\s\\S]*workspace_modal_body_content\\(modal_type, stage, title_metric\\)[\\s\\S]*show_workspace_modal_dialog\\(title, body_content = body_content\\)[\\s\\S]*session\\$sendCustomMessage\\("bindWorkspaceModalContent", list\\(\\)\\)[\\s\\S]*remove_analysis_launch_spinner_notification\\(session, request_id\\)[\\s\\S]*invisible\\(NULL\\)[\\s\\S]*return\\(\\)',
    app_text,
    perl = TRUE
  ))
}

check_phase4_summary_artifact_modes <- function() {
  summary_lines <- readLines("app/helpers/summary_page.R", warn = FALSE)
  ref_curve_lines <- readLines("app/modules/mod_ref_curve.R", warn = FALSE)
  phase4_lines <- readLines("app/modules/mod_phase4_finalization.R", warn = FALSE)
  curve_lines <- readLines("R/10_reference_curves.R", warn = FALSE)
  phase4_text <- paste(phase4_lines, collapse = "\n")

  stopifnot(any(grepl("phase4_artifact_mode_satisfies <- function", summary_lines, fixed = TRUE)))
  stopifnot(any(grepl("get_metric_phase4_artifact_mode <- function", summary_lines, fixed = TRUE)))
  stopifnot(any(grepl("metric_needs_phase4_artifact_refresh <- function", summary_lines, fixed = TRUE)))
  stopifnot(any(grepl('artifact_mode = c\\("full", "summary"\\)', summary_lines)))
  stopifnot(any(grepl("phase4_curve_rows", summary_lines, fixed = TRUE)))
  stopifnot(any(grepl('artifact_mode = cached\\$artifact_mode %\\|\\|% "full"', ref_curve_lines)))
  stopifnot(any(grepl('metric_has_phase4_cache\\(rv, metric, decision_tbl, artifact_mode = "summary"\\)', ref_curve_lines)))
  stopifnot(any(grepl('metric_has_phase4_cache\\(rv, metric, decision_tbl, artifact_mode = "summary"\\)', phase4_lines)))
  stopifnot(grepl(
    'preload_metric_phase4_workspace\\([\\s\\S]*artifact_mode = "full"',
    phase4_text,
    perl = TRUE
  ))
  stopifnot(any(grepl("strip_reference_curve_result <- function", curve_lines, fixed = TRUE)))
  stopifnot(any(grepl('artifact_mode = c\\("full", "summary"\\)', curve_lines)))
  stopifnot(any(grepl("build_plots = TRUE", curve_lines, fixed = TRUE)))
}

check_phase1_scatterplot_controls <- function() {
  global_lines <- readLines("app/global.R", warn = FALSE)
  plot_theme_lines <- readLines("R/00_plot_theme.R", warn = FALSE)
  phase1_lines <- readLines("app/modules/mod_phase1_exploration.R", warn = FALSE)
  css_lines <- readLines("app/www/custom.css", warn = FALSE)
  screening_lines <- readLines("R/05_stratification_screening.R", warn = FALSE)
  summary_lines <- readLines("app/helpers/summary_page.R", warn = FALSE)

  stopifnot(any(grepl("library\\(plotly\\)", global_lines)))
  stopifnot(any(grepl("plotly::plotlyOutput\\(", phase1_lines)))
  stopifnot(any(grepl("plotly::renderPlotly\\(", phase1_lines)))
  stopifnot(any(grepl("scatterplot-comparison-card", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("scatter_panel_list_ui", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("phase1_scatter_panels", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("scatter_context_metric <- function", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("rv\\$workspace_modal_metric %\\|\\|% rv\\$current_metric", phase1_lines)))
  stopifnot(any(grepl("scatter_panel_store <- reactiveVal\\(", phase1_lines)))
  stopifnot(any(grepl("scatter_panel_shells <- reactiveVal\\(", phase1_lines)))
  stopifnot(any(grepl("scatter_panel_shell_version <- reactiveVal\\(", phase1_lines)))
  stopifnot(any(grepl("scatter_panel_active_tab <- reactiveVal\\(", phase1_lines)))
  stopifnot(any(grepl("scatter_panel_loaded_context <- reactiveVal\\(", phase1_lines)))
  stopifnot(any(grepl("scatter_panel_context_key <- function", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("apply_scatter_panel_edit <- function", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("build_scatter_panel_messages <- function", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("prune_scatter_panel_input_observers <- function", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("register_scatter_panel_input_observers <- function", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("scatter_panel_shells_from_panels <- function", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("resolve_scatter_panel_active_tab <- function", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("set_scatter_panel_active_tab <- function", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("get_scatter_panel_ui_panel <- function", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("freezeReactiveValue\\(input, strat_input_id\\)", phase1_lines)))
  stopifnot(any(grepl("updateSelectInput\\(", phase1_lines)))
  stopifnot(!any(grepl("scatter_panel_seed <- reactiveVal\\(", phase1_lines)))
  stopifnot(!any(grepl("scatter_panel_picker_sync <- reactiveVal\\(", phase1_lines)))
  stopifnot(!any(grepl("sync_scatter_panel_picker <- function", phase1_lines, fixed = TRUE)))
  stopifnot(!any(grepl("scatter_panel_seed_shells <- function", phase1_lines, fixed = TRUE)))
  stopifnot(!any(grepl("scatter_panel_ui_state <- reactive\\(", phase1_lines)))
  stopifnot(!any(grepl("scatter_panels_state <- reactive\\(", phase1_lines)))
  stopifnot(any(grepl("set_scatter_panel_state <- function", phase1_lines, fixed = TRUE)))
  stopifnot(!any(grepl("picker_key = \"compare\"", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("get_scatter_panel_live_panel <- function", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("add_scatter_panel", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("phase1_quick_mask_sites", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("Temporarily hide sites in exploratory plots:", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("Affects exploratory scatterplots, histograms, and boxplots only\\.", phase1_lines)))
  stopifnot(any(grepl("It does not recompute screening statistics or effect sizes\\.", phase1_lines)))
  stopifnot(any(grepl("phase1_quick_mask_site_ids", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("class = \"phase1-quick-mask-card\"", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("class = \"py-2 phase1-quick-mask-card-body\"", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("selected = character\\(0\\)", phase1_lines)))
  stopifnot(!any(grepl("selected = cached_phase1_quick_mask_ids\\(scatter_context_metric\\(\\)\\)", phase1_lines)))
  stopifnot(any(grepl("Scatter comparison controls previously got stuck in reset loops", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("keep compare_metric/strat_key/current_on_x in scatter_panel_store\\(\\) only", phase1_lines)))
  stopifnot(any(grepl("scatter_panel_messages_output_id <- function", phase1_lines, fixed = TRUE)))
  stopifnot(!any(grepl("scatter_panel_plot_shell_output_id <- function", phase1_lines, fixed = TRUE)))
  stopifnot(!any(grepl("scatter_panel_plot_fingerprint <- function", phase1_lines, fixed = TRUE)))
  stopifnot(!any(grepl("phase1_scatter_active_panel", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("Scatterplot Comparison Panels", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("Comparison metric:", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("Stratify by comparison metric:", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("Current metric on X axis", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("Site ID:", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("Site Label:", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("scatter_panel_plot_height_px <- function", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("380L", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("430L", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("width = \"100%\"", phase1_lines, fixed = TRUE)))
  stopifnot(!any(grepl("scatter_current_on_x", phase1_lines, fixed = TRUE)))
  stopifnot(!any(grepl("scatter_metrics", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("selectInput\\(", phase1_lines)))
  stopifnot(any(grepl("checkboxInput\\(", phase1_lines)))
  stopifnot(sum(grepl("ignoreNULL = TRUE, ignoreInit = TRUE", phase1_lines, fixed = TRUE)) >= 3)
  stopifnot(any(grepl("scatter_panel_tabs_input_id <- \"scatter_panel_tabs\"", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("bslib::navset_tab", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("bslib::nav_panel", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("remove_scatter_panel", phase1_lines, fixed = TRUE)))
  stopifnot(!any(grepl("shinyWidgets::pickerInput\\(\\s*ns\\(scatter_panel_compare_input_id", phase1_lines)))
  stopifnot(!any(grepl("shinyWidgets::pickerInput\\(\\s*ns\\(scatter_panel_strat_key_input_id", phase1_lines)))
  stopifnot(!any(grepl("shinyWidgets::prettySwitch\\(\\s*ns\\(scatter_panel_toggle_input_id", phase1_lines)))
  stopifnot(any(grepl("streamcurves_plotly_layout\\(", phase1_lines)))
  stopifnot(any(grepl("streamcurves_plotly_axis_defaults\\(", phase1_lines)))
  stopifnot(any(grepl("streamcurves_plotly_legend_defaults\\(", phase1_lines)))
  stopifnot(any(grepl("standoff = 18", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("x = 0.5", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("xanchor = \"center\"", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("automargin = TRUE", plot_theme_lines, fixed = TRUE)))
  stopifnot(any(grepl("scatterplot-panel-tab", css_lines, fixed = TRUE)))
  stopifnot(any(grepl("scatterplot-panel-controls", css_lines, fixed = TRUE)))
  stopifnot(any(grepl("scatterplot-panel-controls \\.form-control", css_lines)))
  stopifnot(any(grepl("scatterplot-panel-toggle \\.checkbox", css_lines)))
  stopifnot(any(grepl("scatterplot-comparison-card \\.nav-tabs", css_lines)))
  stopifnot(any(grepl("scatterplot-comparison-card \\.tab-content", css_lines)))
  stopifnot(any(grepl("\\.phase1-quick-mask-card,\\s*$", css_lines)))
  stopifnot(any(grepl("\\.phase1-quick-mask-card \\.card-body \\{", css_lines)))
  stopifnot(any(grepl("overflow: visible;", css_lines, fixed = TRUE)))
  stopifnot(any(grepl("\\.phase1-quick-mask-card \\+ \\.scatterplot-comparison-card \\{", css_lines)))
  stopifnot(!any(grepl("scatterplot-comparison-plot-shell", css_lines, fixed = TRUE)))
  stopifnot(!any(grepl("data-plot-fingerprint", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("plotly::plotly_build\\(", phase1_lines)))
  stopifnot(any(grepl("uiOutput\\(ns\\(scatter_panel_messages_output_id\\(panel\\$id\\)\\)\\)", phase1_lines)))
  stopifnot(any(grepl("output\\[\\[scatter_panel_messages_output_id\\(current_panel_id\\)\\]\\] <- renderUI\\(", phase1_lines)))
  stopifnot(!any(grepl("Secondary comparison metric:", phase1_lines, fixed = TRUE)))
  stopifnot(!any(grepl("\"Stratify:\"", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("y = 1.08", phase1_lines, fixed = TRUE)))
  stopifnot(!any(grepl("y = -0.18", phase1_lines, fixed = TRUE)))
  stopifnot(!any(grepl("y = -0.24", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("workspace-modal-dialog \\.scatterplot-comparison-card \\.card-body", css_lines)))
  stopifnot(any(grepl("Force scatter panel state to invalidate when the temporary site mask changes", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("quick_mask_ids <- phase1_quick_mask_ids\\(\\)", phase1_lines)))
  stopifnot(any(grepl("cache_entry <- shiny::isolate\\(rv\\$metric_phase_cache\\[\\[current_metric\\]\\] %\\|\\|% list\\(\\)\\)", phase1_lines)))
  stopifnot(any(grepl("if \\(identical\\(cached_ids, quick_mask_ids\\)\\)", phase1_lines)))
  stopifnot(any(grepl("rv\\$metric_phase_cache\\[\\[current_metric\\]\\] <- cache_entry", phase1_lines)))
  stopifnot(any(grepl("boxplot_points", phase1_lines, fixed = TRUE)))
  stopifnot(any(grepl("masked_site_ids = phase1_quick_mask_ids\\(\\)", phase1_lines)))
  stopifnot(any(grepl("visible_rows <- \\!\\(site_identity_frame\\(data\\)\\$site_id %in% quick_mask_ids\\)", phase1_lines)))
  stopifnot(any(grepl("build_screening_plot_from_spec <- function", screening_lines, fixed = TRUE)))
  stopifnot(any(grepl("masked_site_ids = integer\\(0\\)", screening_lines)))
  stopifnot(any(grepl("streamcurves_site_id_column", screening_lines, fixed = TRUE)))
  stopifnot(any(grepl("plot_spec =", screening_lines, fixed = TRUE)))
  stopifnot(any(grepl("plot_specs = screening\\$plot_specs", summary_lines)))
  stopifnot(any(grepl("plot_specs = plot_specs", summary_lines, fixed = TRUE)))
  stopifnot(any(grepl("panel_defs <- scatter_panel_shells\\(\\)", phase1_lines)))
  stopifnot(!any(grepl("scatter_panel_remove_input_id <- function", phase1_lines, fixed = TRUE)))
}

check_phase1_refresh_flag()
check_phase3_refresh_flag()
check_phase4_cache_rules()
check_phase4_completed_fallback()
check_phase4_display_state_prefers_current_cache()
check_phase4_display_state_prefers_current_completed_when_cache_is_stale()
check_phase4_display_state_ignores_stale_results()
check_deferred_modal_callbacks()
check_analysis_helper_defaults_are_safe()
check_phase2_shell_delay()
check_modal_launch_is_deferred()
check_analysis_tab_state_helpers()
check_workspace_modal_ui_builders()
check_helper_isolation_works()
check_curve_strat_flow()
check_summary_page_analysis_controls()
check_analysis_modal_dropdown_overlays()
check_verification_boxplots_rebuild_from_specs()
check_analysis_blocking_preload_hooks()
check_analysis_launch_feedback()
check_phase4_summary_artifact_modes()
check_phase1_scatterplot_controls()

cat("Phase cache checks passed.\n")
