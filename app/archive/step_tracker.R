## ── Step Tracker ──────────────────────────────────────────────────────────────
## Visual progress indicator for the 11-step (0-10) metric analysis wizard.

library(shiny)

STEP_LABELS <- c(
  "Precheck",                    # Step 0
  "Statistical Significance",    # Step 1
  "Effect Size",                 # Step 2
  "Pattern Stability",           # Step 3
  "Practical Relevance",         # Step 4
  "Operational Feasibility",     # Step 5
  "Stratification Decision",     # Step 6
  "Model Building",              # Step 7
  "Model Selection",             # Step 8
  "Diagnostics",                 # Step 9
  "Reference Curve"              # Step 10
)

#' Render the step tracker sidebar UI
#' @param current_step Integer 0-10
#' @param completed_steps Integer vector of completed step numbers
#' @param ns Shiny namespace function
#' @return HTML tag list
step_tracker_ui <- function(current_step, completed_steps = integer(0), ns = identity) {
  steps <- lapply(seq_along(STEP_LABELS), function(i) {
    ## Map 1-based index to 0-based step number
    step_num <- i - 1

    if (step_num %in% completed_steps) {
      icon_class <- "text-success"
      icon <- "\u2713"
      text_class <- "text-success"
    } else if (step_num == current_step) {
      icon_class <- "text-primary"
      icon <- "\u25cf"
      text_class <- "fw-bold text-primary"
    } else {
      icon_class <- "text-muted"
      icon <- "\u25cb"
      text_class <- "text-muted"
    }

    clickable <- step_num %in% completed_steps || step_num == current_step
    step_id <- ns(paste0("goto_step_", step_num))

    label_text <- paste0(step_num, ". ", STEP_LABELS[i])

    if (clickable) {
      tags$div(
        class = "d-flex align-items-center mb-2",
        style = "cursor: pointer;",
        onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", step_id, step_num),
        tags$span(class = paste("me-2", icon_class), style = "font-size: 1.1rem;", icon),
        tags$span(class = text_class, style = "font-size: 0.82rem;", label_text)
      )
    } else {
      tags$div(
        class = "d-flex align-items-center mb-2",
        tags$span(class = paste("me-2", icon_class), style = "font-size: 1.1rem;", icon),
        tags$span(class = text_class, style = "font-size: 0.82rem;", label_text)
      )
    }
  })

  tags$div(class = "mt-3", steps)
}

## ── Per-step state fields for save/restore across metric switches ───────────
STEP_STATE_FIELDS <- c(
  "layer1_screening",
  "layer1_reviewer",
  "layer2_effect_sizes",
  "layer3_patterns",
  "layer4_relevance",
  "layer5_feasibility",
  "layer5_reviewer",
  "strat_decision_auto",
  "strat_decision_user",
  "model_candidates",
  "model_selection_auto",
  "model_selection_user",
  "diagnostics",
  "reference_curve"
)

save_metric_state <- function(rv, metric) {
  if (is.null(metric) || metric == "") return(invisible(NULL))
  snapshot <- list(current_step = rv$current_step %||% 0)
  for (field in STEP_STATE_FIELDS) {
    snapshot[[field]] <- rv[[field]]
  }
  snapshot$config_version <- rv$config_version
  rv$metric_step_cache[[metric]] <- snapshot
  invisible(NULL)
}

#' Reset all analysis state across every metric
#' @param rv reactiveValues object
reset_all_analysis <- function(rv) {
  rv$current_step <- 0
  for (field in STEP_STATE_FIELDS) {
    rv[[field]] <- NULL
  }

  rv$metric_step_cache        <- list()
  rv$completed_metrics        <- list()
  rv$all_layer1_results       <- list()
  rv$all_layer2_results       <- list()
  rv$all_layer4_results       <- list()
  rv$cross_metric_consistency <- NULL
  rv$decision_log             <- tibble::tibble()
  invisible(NULL)
}

restore_metric_state <- function(rv, metric) {
  snapshot <- rv$metric_step_cache[[metric]]
  if (!is.null(snapshot)) {
    rv$current_step <- snapshot$current_step %||% 0
    for (field in STEP_STATE_FIELDS) {
      rv[[field]] <- snapshot[[field]]
    }
    if (!is.null(snapshot$config_version) &&
        snapshot$config_version != rv$config_version) {
      cli::cli_alert_warning("Config changed since {metric}'s analysis was run")
    }
  } else {
    rv$current_step <- 0
    for (field in STEP_STATE_FIELDS) {
      rv[[field]] <- NULL
    }
  }
  invisible(NULL)
}
