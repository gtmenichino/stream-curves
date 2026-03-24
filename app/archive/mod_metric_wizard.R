## ── Module: Metric Analysis Wizard ────────────────────────────────────────────
## Main Page 3: sidebar + step-based content for single-metric analysis.
## 11-step flow (Steps 0-10): Precheck -> 5 layers -> Decision -> Model -> Diagnostics -> Curve

library(shiny)
library(bslib)

mod_metric_wizard_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    ## ── Sidebar ─────────────────────────────────────────────────────────────
    sidebar = sidebar(
      width = 300,
      title = "Metric Analysis",

      ## Metric picker
      uiOutput(ns("metric_picker")),

      ## Metric info card
      uiOutput(ns("metric_info")),

      ## Step tracker
      tags$hr(),
      tags$strong("Analysis Steps"),
      uiOutput(ns("step_tracker")),
      actionButton(ns("reset_steps"), "Reset This Metric",
                   class = "btn btn-outline-danger btn-sm w-100 mt-2",
                   icon = icon("arrow-rotate-left")),
      actionButton(ns("reset_all_metrics"), "Reset All Metrics",
                   class = "btn btn-outline-secondary btn-sm w-100 mt-1",
                   icon = icon("arrows-rotate")),

      ## Advanced settings
      tags$hr(),
      bslib::accordion(
        id = ns("advanced_accordion"),
        open = FALSE,
        bslib::accordion_panel(
          "Advanced Settings",
          numericInput(ns("min_sample"), "Min sample size:", value = 10, min = 3, max = 30),
          radioButtons(ns("strat_mode"), "Stratification mode:",
                       choices = c("Covariate" = "covariate", "Subset" = "subset"),
                       selected = "covariate"),
          radioButtons(ns("transform"), "Transform:",
                       choices = c("None" = "none", "Log" = "log"),
                       selected = "none")
        )
      )
    ),

    ## ── Main content ────────────────────────────────────────────────────────
    uiOutput(ns("step_content"))
  )
}

mod_metric_wizard_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    prev_metric <- reactiveVal(NULL)

    ## ── Build metric picker choices (grouped by family) ───────────────────────
    output$metric_picker <- renderUI({
      mc <- rv$metric_config

      ## Exclude categorical and build grouped choices
      families <- list()
      for (mk in names(mc)) {
        fam <- mc[[mk]]$metric_family
        if (fam == "categorical") next
        fam_label <- switch(fam,
          continuous  = "Continuous",
          proportion  = "Proportion",
          count       = "Count",
          fam
        )
        families[[fam_label]] <- c(families[[fam_label]], setNames(mk, mc[[mk]]$display_name))
      }

      selectInput(
        ns("metric_select"), "Select Metric:",
        choices = families,
        selected = "perRiffle"
      )
    })

    ## ── Metric change observer ────────────────────────────────────────────────
    observeEvent(input$metric_select, {
      old_metric <- prev_metric()
      new_metric <- input$metric_select

      ## Same metric re-selected — no-op
      if (!is.null(old_metric) && identical(old_metric, new_metric)) {
        return(invisible(NULL))
      }

      ## SAVE outgoing metric's state
      if (!is.null(old_metric) && old_metric != "") {
        save_metric_state(rv, old_metric)
      }

      ## Switch
      rv$current_metric <- new_metric
      prev_metric(new_metric)

      ## RESTORE incoming metric's state (or NULL if first visit)
      restore_metric_state(rv, new_metric)

      ## Update advanced settings from config
      mc <- rv$metric_config[[new_metric]]
      if (!is.null(mc)) {
        updateNumericInput(session, "min_sample", value = mc$min_sample_size %||% 10)
        updateRadioButtons(session, "strat_mode",
                            selected = mc$stratification_mode %||% "covariate")
        updateRadioButtons(session, "transform",
                            selected = mc$preferred_transform %||% "none")
      }
    }, ignoreInit = TRUE)

    ## ── Advanced settings write-back to config ────────────────────────────────
    observeEvent(input$min_sample, {
      req(rv$current_metric)
      mc <- isolate(rv$metric_config)
      if (!is.null(mc[[rv$current_metric]]) &&
          !identical(mc[[rv$current_metric]]$min_sample_size, input$min_sample)) {
        mc[[rv$current_metric]]$min_sample_size <- input$min_sample
        rv$metric_config <- mc
      }
    }, ignoreInit = TRUE)

    observeEvent(input$strat_mode, {
      req(rv$current_metric)
      mc <- isolate(rv$metric_config)
      if (!is.null(mc[[rv$current_metric]]) &&
          !identical(mc[[rv$current_metric]]$stratification_mode, input$strat_mode)) {
        mc[[rv$current_metric]]$stratification_mode <- input$strat_mode
        rv$metric_config <- mc
      }
    }, ignoreInit = TRUE)

    observeEvent(input$transform, {
      req(rv$current_metric)
      mc <- isolate(rv$metric_config)
      if (!is.null(mc[[rv$current_metric]]) &&
          !identical(mc[[rv$current_metric]]$preferred_transform, input$transform)) {
        mc[[rv$current_metric]]$preferred_transform <- input$transform
        rv$metric_config <- mc
      }
    }, ignoreInit = TRUE)

    ## ── Reset this metric observer ─────────────────────────────────────────────
    observeEvent(input$reset_steps, {
      req(rv$current_metric)
      metric <- rv$current_metric

      rv$current_step <- 0
      for (field in STEP_STATE_FIELDS) {
        rv[[field]] <- NULL
      }

      ## Clear cache entry for this metric
      rv$metric_step_cache[[metric]] <- NULL

      ## Also remove from cross-metric caches
      rv$completed_metrics[[metric]]  <- NULL
      rv$all_layer1_results[[metric]] <- NULL
      rv$all_layer2_results[[metric]] <- NULL
      rv$all_layer4_results[[metric]] <- NULL

      ## Remove from decision log
      if (!is.null(rv$decision_log) && nrow(rv$decision_log) > 0) {
        rv$decision_log <- rv$decision_log |> dplyr::filter(metric != !!metric)
      }

      showNotification(
        paste0("Steps reset for ", metric, " \u2014 starting from Step 0."),
        type = "message", duration = 3
      )
    })

    ## ── Reset all metrics observer ────────────────────────────────────────────
    observeEvent(input$reset_all_metrics, {
      showModal(modalDialog(
        title = "Reset All Metrics",
        "This will clear all completed work for every metric, including cached results and the decision log. This cannot be undone.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset_all"), "Reset Everything",
                       class = "btn btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_reset_all, {
      removeModal()

      rv$current_step <- 0
      for (field in STEP_STATE_FIELDS) {
        rv[[field]] <- NULL
      }

      ## Clear all caches
      rv$metric_step_cache        <- list()
      rv$completed_metrics        <- list()
      rv$all_layer1_results       <- list()
      rv$all_layer2_results       <- list()
      rv$all_layer4_results       <- list()
      rv$cross_metric_consistency <- NULL
      rv$decision_log             <- tibble::tibble()

      showNotification("All metrics reset \u2014 starting fresh.",
                       type = "message", duration = 3)
    })

    ## ── Metric info card ──────────────────────────────────────────────────────
    output$metric_info <- renderUI({
      req(rv$current_metric)
      mc <- rv$metric_config[[rv$current_metric]]
      req(mc)

      precheck <- rv$precheck_df |>
        dplyr::filter(metric == rv$current_metric)

      completed <- rv$current_metric %in% names(rv$completed_metrics)

      div(
        class = "metric-info-card",
        tags$table(
          class = "table table-sm mb-0",
          tags$tbody(
            tags$tr(
              tags$td(class = "info-label", "Family:"),
              tags$td(class = "info-value", mc$metric_family)
            ),
            tags$tr(
              tags$td(class = "info-label", "Units:"),
              tags$td(class = "info-value", mc$units)
            ),
            tags$tr(
              tags$td(class = "info-label", "Direction:"),
              tags$td(class = "info-value",
                       if (isTRUE(mc$higher_is_better)) "Higher is better"
                       else if (isFALSE(mc$higher_is_better)) "Lower is better"
                       else "Neutral")
            ),
            if (nrow(precheck) > 0) {
              tagList(
                tags$tr(
                  tags$td(class = "info-label", "n obs:"),
                  tags$td(class = "info-value", precheck$n_obs)
                ),
                tags$tr(
                  tags$td(class = "info-label", "Status:"),
                  tags$td(status_badge(precheck$precheck_status))
                )
              )
            }
          )
        ),
        if (completed) {
          div(class = "mt-2", status_badge("pass", "COMPLETED"))
        }
      )
    })

    ## ── Step tracker ──────────────────────────────────────────────────────────
    output$step_tracker <- renderUI({
      current <- rv$current_step %||% 0
      completed <- get_completed_steps(rv)
      step_tracker_ui(current, completed, ns)
    })

    ## ── Step navigation via tracker clicks ────────────────────────────────────
    lapply(0:10, function(i) {
      observeEvent(input[[paste0("goto_step_", i)]], {
        completed <- get_completed_steps(rv)
        if (i %in% completed || i == rv$current_step) {
          rv$current_step <- i
        }
      }, ignoreInit = TRUE)
    })

    ## ── Step content rendering ────────────────────────────────────────────────
    output$step_content <- renderUI({
      req(rv$current_metric)
      step <- rv$current_step %||% 0

      switch(as.character(step),
        "0"  = mod_precheck_ui(ns("precheck")),
        "1"  = mod_layer1_significance_ui(ns("layer1")),
        "2"  = mod_layer2_effect_size_ui(ns("layer2")),
        "3"  = mod_layer3_pattern_ui(ns("layer3")),
        "4"  = mod_layer4_relevance_ui(ns("layer4")),
        "5"  = mod_layer5_feasibility_ui(ns("layer5")),
        "6"  = mod_strat_decision_ui(ns("strat_decision")),
        "7"  = mod_model_build_ui(ns("model_build")),
        "8"  = mod_model_select_ui(ns("model_select")),
        "9"  = mod_diagnostics_ui(ns("diagnostics")),
        "10" = mod_ref_curve_ui(ns("ref_curve")),
        div("Unknown step")
      )
    })

    ## ── Sub-module servers ────────────────────────────────────────────────────
    precheck_events       <- mod_precheck_server("precheck", rv)
    layer1_events         <- mod_layer1_significance_server("layer1", rv)
    layer2_events         <- mod_layer2_effect_size_server("layer2", rv)
    layer3_events         <- mod_layer3_pattern_server("layer3", rv)
    layer4_events         <- mod_layer4_relevance_server("layer4", rv)
    layer5_events         <- mod_layer5_feasibility_server("layer5", rv)
    strat_decision_events <- mod_strat_decision_server("strat_decision", rv)
    model_build_events    <- mod_model_build_server("model_build", rv)
    model_select_events   <- mod_model_select_server("model_select", rv)
    diag_events           <- mod_diagnostics_server("diagnostics", rv)
    ref_curve_events      <- mod_ref_curve_server("ref_curve", rv)

    ## ── Step navigation: proceed buttons ──────────────────────────────────────
    ## Step 0 -> 1: Precheck -> Significance
    observeEvent(precheck_events(), {
      rv$current_step <- 1
    }, ignoreInit = TRUE)

    ## Step 1 -> 2: Significance -> Effect Size
    observeEvent(layer1_events$proceed(), {
      rv$current_step <- 2
    }, ignoreInit = TRUE)
    observeEvent(layer1_events$back(), {
      rv$current_step <- 0
    }, ignoreInit = TRUE)

    ## Step 2 -> 3: Effect Size -> Pattern
    observeEvent(layer2_events$proceed(), {
      rv$current_step <- 3
    }, ignoreInit = TRUE)
    observeEvent(layer2_events$back(), {
      rv$current_step <- 1
    }, ignoreInit = TRUE)

    ## Step 3 -> 4: Pattern -> Relevance
    observeEvent(layer3_events$proceed(), {
      rv$current_step <- 4
    }, ignoreInit = TRUE)
    observeEvent(layer3_events$back(), {
      rv$current_step <- 2
    }, ignoreInit = TRUE)

    ## Step 4 -> 5: Relevance -> Feasibility
    observeEvent(layer4_events$proceed(), {
      rv$current_step <- 5
    }, ignoreInit = TRUE)
    observeEvent(layer4_events$back(), {
      rv$current_step <- 3
    }, ignoreInit = TRUE)

    ## Step 5 -> 6: Feasibility -> Decision
    observeEvent(layer5_events$proceed(), {
      rv$current_step <- 6
    }, ignoreInit = TRUE)
    observeEvent(layer5_events$back(), {
      rv$current_step <- 4
    }, ignoreInit = TRUE)

    ## Step 6 -> 7: Decision -> Model Building
    observeEvent(strat_decision_events$proceed(), {
      rv$current_step <- 7
    }, ignoreInit = TRUE)
    observeEvent(strat_decision_events$back(), {
      rv$current_step <- 5
    }, ignoreInit = TRUE)

    ## Step 7 -> 8: Model Building -> Model Selection
    observeEvent(model_build_events$proceed(), {
      rv$current_step <- 8
    }, ignoreInit = TRUE)
    observeEvent(model_build_events$back(), {
      rv$current_step <- 6
    }, ignoreInit = TRUE)

    ## Step 8 -> 9: Model Selection -> Diagnostics
    observeEvent(model_select_events$proceed(), {
      rv$current_step <- 9
    }, ignoreInit = TRUE)
    observeEvent(model_select_events$back(), {
      rv$current_step <- 7
    }, ignoreInit = TRUE)

    ## Step 9 -> 10: Diagnostics -> Reference Curve
    observeEvent(diag_events$proceed(), {
      rv$current_step <- 10
    }, ignoreInit = TRUE)
    observeEvent(diag_events$back(), {
      rv$current_step <- 8
    }, ignoreInit = TRUE)

    ## Step 10: Reference Curve back
    observeEvent(ref_curve_events$back(), {
      rv$current_step <- 9
    }, ignoreInit = TRUE)
  })
}


## Helper: determine which steps are completed based on reactive state
get_completed_steps <- function(rv) {
  steps <- integer(0)

  ## Step 0: Precheck (always done when metric is selected)
  if (!is.null(rv$current_metric)) steps <- c(steps, 0L)

  ## Step 1: Significance
  if (!is.null(rv$layer1_screening)) steps <- c(steps, 1L)

  ## Step 2: Effect Size
  if (!is.null(rv$layer2_effect_sizes) && nrow(rv$layer2_effect_sizes) > 0) steps <- c(steps, 2L)

  ## Step 3: Pattern Stability
  if (!is.null(rv$layer3_patterns)) steps <- c(steps, 3L)

  ## Step 4: Practical Relevance
  if (!is.null(rv$layer4_relevance) && nrow(rv$layer4_relevance) > 0) steps <- c(steps, 4L)

  ## Step 5: Operational Feasibility
  if (!is.null(rv$layer5_feasibility) && nrow(rv$layer5_feasibility) > 0) steps <- c(steps, 5L)

  ## Step 6: Stratification Decision
  if (!is.null(rv$strat_decision_user)) steps <- c(steps, 6L)

  ## Step 7: Model Building
  if (!is.null(rv$model_candidates) && nrow(rv$model_candidates$candidates_df) > 0) steps <- c(steps, 7L)

  ## Step 8: Model Selection
  if (!is.null(rv$model_selection_user)) steps <- c(steps, 8L)

  ## Step 9: Diagnostics
  if (!is.null(rv$diagnostics)) steps <- c(steps, 9L)

  ## Step 10: Reference Curve
  if (!is.null(rv$reference_curve)) steps <- c(steps, 10L)

  steps
}
