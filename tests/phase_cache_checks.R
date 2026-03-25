suppressPackageStartupMessages({
  library(shiny)
  library(tibble)
})

source("app/helpers/phase_tracker.R", local = TRUE)
source("app/helpers/summary_page.R", local = TRUE)

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

check_deferred_modal_callbacks <- function() {
  files <- c(
    "app/modules/mod_phase1_exploration.R",
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
    workspace_modal_nonce = 0L
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
}

check_workspace_modal_ui_builders <- function() {
  source("app/modules/mod_phase1_exploration.R", local = TRUE)
  source("app/modules/mod_phase3_verification.R", local = TRUE)
  source("app/modules/mod_phase4_finalization.R", local = TRUE)

  invisible(htmltools::renderTags(mod_phase1_exploration_ui("phase1", dialog_mode = TRUE)))
  invisible(htmltools::renderTags(mod_phase3_verification_ui("phase3", dialog_mode = TRUE)))
  invisible(htmltools::renderTags(mod_phase4_finalization_ui("phase4", dialog_mode = TRUE)))
}

make_reactive_rv <- function() {
  shiny::reactiveValues(
    current_metric = "metric_a",
    phase1_screening = list(
      results = tibble(stratification = "strat_a"),
      pairwise = tibble(),
      plots = list()
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
          plots = list()
        ),
        phase1_effect_sizes = tibble(stratification = "strat_a", epsilon_squared = 0.1),
        phase1_artifact_mode = "summary",
        phase3_patterns = list(results = tibble(stratification = "strat_a"), plots = list()),
        phase3_feasibility = tibble(stratification = "strat_a"),
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

check_phase1_refresh_flag()
check_phase3_refresh_flag()
check_phase4_cache_rules()
check_phase4_completed_fallback()
check_deferred_modal_callbacks()
check_phase2_shell_delay()
check_modal_launch_is_deferred()
check_workspace_modal_ui_builders()
check_helper_isolation_works()

cat("Phase cache checks passed.\n")
