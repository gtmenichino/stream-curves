## ── Phase Tracker ────────────────────────────────────────────────────────────────
## Visual progress indicator for the 4-phase workflow.
## Replaces helpers/step_tracker.R from the 11-step wizard.

library(shiny)

PHASE_LABELS <- c(
  "Initial Exploration",       # Phase 1
  "Cross-Metric Consistency",  # Phase 2
  "Verification",              # Phase 3
  "Reference Curve"            # Phase 4
)

## ── Per-phase state fields for save/restore across metric switches ────────────
PHASE_STATE_FIELDS <- c(
  "phase1_screening",
  "phase1_effect_sizes",
  "phase3_patterns",
  "phase3_feasibility",
  "strat_decision_user",
  "reference_curve",
  "current_stratum_level",
  "phase4_data"
)

deep_copy_value <- function(x) {
  if (is.null(x)) return(NULL)
  unserialize(serialize(x, NULL))
}

#' Render the phase tracker horizontal progress bar
#' @param current_phase Integer 1-4
#' @param completed_phases Integer vector of completed phase numbers (1-4)
#' @param ns Shiny namespace function
#' @return HTML tag list
phase_tracker_ui <- function(current_phase, completed_phases = integer(0), ns = identity) {
  dots <- lapply(seq_along(PHASE_LABELS), function(i) {
    if (i %in% completed_phases) {
      icon_class <- "phase-dot completed"
      icon <- "\u2713"
    } else if (i == current_phase) {
      icon_class <- "phase-dot current"
      icon <- "\u25cf"
    } else {
      icon_class <- "phase-dot pending"
      icon <- "\u25cb"
    }

    tags$div(
      class = "d-flex flex-column align-items-center",
      style = "flex: 1; min-width: 0;",
      tags$div(
        class = icon_class,
        style = "width: 32px; height: 32px; border-radius: 50%; display: flex;
                 align-items: center; justify-content: center; font-size: 1rem;
                 font-weight: 700; border: 2px solid currentColor;",
        icon
      ),
      tags$span(
        style = "font-size: 0.72rem; text-align: center; margin-top: 4px;
                 line-height: 1.1;",
        PHASE_LABELS[i]
      )
    )
  })

  tags$div(class = "phase-tracker d-flex justify-content-between gap-2 mb-3", dots)
}


#' Save metric state to cache on metric switch
#' @param rv reactiveValues object
#' @param metric Metric key string
save_metric_phase_state <- function(rv, metric) {
  if (is.null(metric) || metric == "") return(invisible(NULL))
  snapshot <- rv$metric_phase_cache[[metric]] %||% list()
  for (field in PHASE_STATE_FIELDS) {
    snapshot[[field]] <- rv[[field]]
  }
  snapshot$config_version <- rv$config_version
  ## Save per-stratum results for this metric
  snapshot$stratum_results <- rv$stratum_results[[metric]]
  rv$metric_phase_cache[[metric]] <- snapshot
  invisible(NULL)
}


#' Restore metric state from cache on metric switch
#' @param rv reactiveValues object
#' @param metric Metric key string
restore_metric_phase_state <- function(rv, metric) {
  snapshot <- rv$metric_phase_cache[[metric]]
  if (!is.null(snapshot)) {
    for (field in PHASE_STATE_FIELDS) {
      rv[[field]] <- snapshot[[field]]
    }
    ## Restore per-stratum results for this metric
    if (!is.null(snapshot$stratum_results)) {
      rv$stratum_results[[metric]] <- snapshot$stratum_results
    }
    if (!is.null(snapshot$config_version) &&
        snapshot$config_version != rv$config_version) {
      cli::cli_alert_warning("Config changed since {metric}'s analysis was run")
    }
  } else {
    for (field in PHASE_STATE_FIELDS) {
      rv[[field]] <- NULL
    }
  }
  invisible(NULL)
}


#' Save per-stratum Phase 4 state
#' @param rv reactiveValues object
#' @param metric Metric key string
#' @param level Stratum level string
save_stratum_state <- function(rv, metric, level) {
  if (is.null(metric) || is.null(level)) return(invisible(NULL))
  if (is.null(rv$stratum_results[[metric]])) {
    rv$stratum_results[[metric]] <- list()
  }
  rv$stratum_results[[metric]][[level]] <- list(
    reference_curve = rv$reference_curve
  )
  invisible(NULL)
}


#' Restore per-stratum Phase 4 state
#' @param rv reactiveValues object
#' @param metric Metric key string
#' @param level Stratum level string
restore_stratum_state <- function(rv, metric, level) {
  saved <- rv$stratum_results[[metric]][[level]]
  if (is.null(saved)) {
    rv$reference_curve <- NULL
  } else {
    rv$reference_curve <- saved$reference_curve
  }
  invisible(NULL)
}


#' Reset all analysis state across every metric
#' @param rv reactiveValues object
reset_all_analysis <- function(rv) {
  ## ── Clean up custom groupings first ───────────────────────────────
  if (length(rv$custom_groupings) > 0) {
    for (cg_key in names(rv$custom_groupings)) {
      cg <- rv$custom_groupings[[cg_key]]
      ## Remove data column
      if (!is.null(cg$column_name) && cg$column_name %in% names(rv$data)) {
        rv$data[[cg$column_name]] <- NULL
      }
      ## Remove from strat_config
      rv$strat_config[[cg_key]] <- NULL
      ## Remove from allowed_stratifications in metric_config
      for (mk in names(rv$metric_config)) {
        allowed <- rv$metric_config[[mk]]$allowed_stratifications
        if (cg_key %in% allowed) {
          rv$metric_config[[mk]]$allowed_stratifications <-
            setdiff(allowed, cg_key)
        }
      }
    }
    rv$custom_groupings <- list()
    rv$custom_grouping_counter <- list()
  }

  ## ── Reset phase state ─────────────────────────────────────────────
  for (field in PHASE_STATE_FIELDS) {
    rv[[field]] <- NULL
  }

  rv$metric_phase_cache         <- list()
  rv$completed_metrics          <- list()
  rv$all_layer1_results         <- list()
  rv$all_layer2_results         <- list()
  rv$phase1_candidates          <- list()
  rv$phase2_ranking             <- NULL
  rv$cross_metric_consistency   <- NULL
  rv$phase2_settings            <- empty_phase2_settings()
  rv$phase2_metric_overrides    <- list()
  rv$summary_available_overrides <- list()
  rv$summary_edit_notes         <- list()
  rv$phase3_verification        <- list()
  rv$decision_log               <- tibble::tibble()
  rv$stratum_results            <- list()
  invisible(NULL)
}


#' Reset the app to its original startup state
#' @param rv reactiveValues object
reset_app_to_startup <- function(rv) {
  reset_all_analysis(rv)

  rv$metric_config        <- deep_copy_value(rv$startup_metric_config)
  rv$strat_config         <- deep_copy_value(rv$startup_strat_config)
  rv$predictor_config     <- deep_copy_value(rv$startup_predictor_config)
  rv$factor_recode_config <- deep_copy_value(rv$startup_factor_recode_config)
  rv$output_config        <- deep_copy_value(rv$startup_output_config)
  rv$config_version       <- rv$startup_config_version %||% 0L

  rv$data                 <- NULL
  rv$qa_log               <- NULL
  rv$precheck_df          <- NULL
  rv$data_source          <- NULL
  rv$data_fingerprint     <- NULL
  rv$upload_filename      <- NULL
  rv$session_name         <- NULL
  rv$current_metric       <- rv$startup_current_metric %||% "perRiffle"
  rv$app_data_loaded      <- FALSE
  rv$app_reset_nonce      <- isolate(rv$app_reset_nonce %||% 0L) + 1L

  invisible(NULL)
}


launch_workspace_modal <- function(rv, phase, metric = NULL) {
  rv$workspace_modal_type <- phase
  rv$workspace_modal_metric <- metric %||% rv$current_metric %||% NULL
  rv$workspace_modal_stage <- "loading"
  rv$workspace_modal_error <- NULL
  rv$workspace_modal_loading_detail <- NULL
  rv$workspace_modal_nonce <- isolate(rv$workspace_modal_nonce %||% 0L) + 1L
  invisible(NULL)
}


notify_workspace_refresh <- function(rv) {
  rv$workspace_refresh_nonce <- isolate(rv$workspace_refresh_nonce %||% 0L) + 1L
  invisible(NULL)
}
