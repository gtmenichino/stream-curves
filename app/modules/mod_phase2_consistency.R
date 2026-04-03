## ── Module: Phase 2 — Cross-Metric Consistency ──────────────────────────────────
## Evaluates stratification performance across multiple metrics.
## Enhanced with support score heatmap, tier classification, threshold controls.

library(shiny)
library(bslib)
library(DT)

mod_phase2_consistency_ui <- function(id) {
  ns <- NS(id)

  tagList(
    ## Instructional card
    explanation_card(
      "Cross-Metric Analysis",
      p("Which stratifications perform consistently across multiple metrics?"),
      p("After completing exploratory screening for at least 2 metrics, compare stratification
         performance using a support score heatmap. Identify broad-use candidates
         that work across most metrics vs. metric-specific ones."),
      p(tags$strong("Requires:"), " Exploratory screening complete for \u2265 2 metrics.")
    ),

    uiOutput(ns("consistency_ui"), class = "phase2-consistency-shell-output")
  )
}

mod_phase2_consistency_server <- function(id, rv, workspace_scope = "standalone") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    workspace_scope <- match.arg(workspace_scope, c("standalone", "analysis"))

    phase2_workspace_active <- function(isolate_state = FALSE) {
      workspace_scope_is_active(
        rv,
        workspace_scope = workspace_scope,
        standalone_modal_type = "phase2",
        isolate_state = isolate_state
      )
    }

    phase2_settings <- reactive({
      normalize_phase2_settings(rv)
    })

    observeEvent(rv$analysis_tab_preload_nonce, {
      if (!identical(workspace_scope, "analysis") ||
          !isTRUE(phase2_workspace_active()) ||
          !identical(rv$analysis_tab_preload_tab %||% NULL, "cross_metric")) {
        return(invisible(NULL))
      }

      request_id <- rv$analysis_tab_request_id %||% NULL
      if (!analysis_tab_request_is_current(rv, request_id)) {
        return(invisible(NULL))
      }

      available_metrics <- get_phase2_metric_choices(rv)
      if (length(available_metrics) >= 2L &&
          (is.null(rv$cross_metric_consistency) || is.null(rv$phase2_ranking))) {
        request_consistency_refresh(
          mode = "auto_full",
          settings = normalize_phase2_settings(rv)
        )
        return(invisible(NULL))
      }

      session$onFlushed(function() {
        shiny::isolate({
          if (!analysis_tab_request_is_current(rv, request_id)) {
            return(invisible(NULL))
          }

          set_analysis_tab_status(rv, "cross_metric", "ready", request_id)
          complete_analysis_tab_preload(rv, "cross_metric", "ready", request_id)
          invisible(NULL)
        })
        invisible(NULL)
      }, once = TRUE)
    }, ignoreInit = TRUE)

    output$current_metric_context <- renderUI({
      if (!isTRUE(phase2_workspace_active())) {
        return(NULL)
      }

      req(rv$current_metric)

      metric <- rv$current_metric
      metric_label <- rv$metric_config[[metric]]$display_name %||% metric
      highlighted <- get_metric_phase2_passed(rv, metric)
      highlighted_label <- if (length(highlighted) == 0) {
        "None highlighted yet"
      } else {
        paste(vapply(highlighted, function(sk) get_strat_display_name(rv, sk), character(1)), collapse = ", ")
      }

      div(
        class = "alert alert-info mb-3",
        tags$strong("Current metric context: "),
        metric_label,
        tags$br(),
        tags$span("Broad-use candidates available to this metric: "),
        highlighted_label,
        tags$br(),
        tags$span("Current curve recommendation: "),
        get_metric_curve_strat_label(rv, metric)
      )
    })

    output$consistency_ui <- renderUI({
      if (!isTRUE(phase2_workspace_active())) {
        return(NULL)
      }

      if (is.null(rv$data)) return(no_data_alert())
      settings <- phase2_settings()

      ## Gate: need at least 2 metrics with Phase 1 results
      n_metrics <- length(get_phase2_metric_choices(rv))

      if (n_metrics < 2) {
        return(div(
          class = "alert alert-info mt-3",
          icon("info-circle"),
          paste0(" Complete exploratory screening for at least 2 metrics to enable this view. ",
                 "Currently completed: ", n_metrics, " metric(s).")
        ))
      }

      tagList(
        uiOutput(ns("current_metric_context")),
        ## Controls
        card(
          card_header("Analysis Controls"),
          card_body(
            layout_column_wrap(
              width = 1 / 2,
              ## Metric filter — pickerInput dropdown
              shinyWidgets::pickerInput(
                ns("metric_filter"), "Metrics to Include:",
                choices = setNames(
                  get_phase2_metric_choices(rv),
                  sapply(get_phase2_metric_choices(rv), function(mk) {
                    rv$metric_config[[mk]]$display_name %||% mk
                  })
                ),
                selected = settings$metric_filter,
                multiple = TRUE,
                options = shinyWidgets::pickerOptions(
                  container = ".modal-dialog.workspace-modal-dialog",
                  size = 8,
                  actionsBox = TRUE, liveSearch = TRUE,
                  selectedTextFormat = "count > 3",
                  countSelectedText = "{0} of {1} selected",
                  noneSelectedText = "None selected"
                )
              ),
              ## Stratification filter — pickerInput dropdown
              shinyWidgets::pickerInput(
                ns("strat_filter"), "Stratifications to Include:",
                choices = {
                  all_strats <- get_phase2_strat_choices(rv, settings$metric_filter)
                  setNames(all_strats, sapply(all_strats, function(sk) {
                    rv$strat_config[[sk]]$display_name %||% sk
                  }))
                },
                selected = settings$strat_filter,
                multiple = TRUE,
                options = shinyWidgets::pickerOptions(
                  container = ".modal-dialog.workspace-modal-dialog",
                  size = 8,
                  actionsBox = TRUE, liveSearch = TRUE,
                  selectedTextFormat = "count > 3",
                  countSelectedText = "{0} of {1} selected",
                  noneSelectedText = "None selected"
                )
              ),
              ## Significance threshold slider
              sliderInput(ns("sig_threshold"), "Significance Level (p-value):",
                          min = 0.01, max = 0.10, value = settings$sig_threshold, step = 0.01),
              ## Support cutoff slider
              sliderInput(ns("support_threshold"), "Strong Support Cutoff:",
                          min = 0.1, max = 0.9, value = settings$support_threshold, step = 0.05)
            ),
            actionButton(ns("compute_matrix"), "Compute Consistency Matrix",
                         class = "btn btn-primary", icon = icon("table"))
          )
        ),

        uiOutput(ns("results_ui"), class = "phase2-consistency-results-output")
      )
    })

    ## ── Compute consistency ───────────────────────────────────────────────────
    consistency_results <- reactiveVal(NULL)
    matrix_loading <- reactiveVal(FALSE)
    loading_mode <- reactiveVal("idle")
    pending_refresh <- reactiveVal(NULL)
    active_refresh_request_id <- reactiveVal(NULL)
    phase2_controls_ready <- reactiveVal(FALSE)
    phase2_applied_settings <- reactiveVal(NULL)
    phase2_programmatic_strat_update <- reactiveVal(FALSE)
    phase2_control_reset_nonce <- reactiveVal(0L)
    loading_notification_id <- ns("consistency_matrix_loading")
    phase2_results_ready_input_id <- ns("phase2_results_ready")
    phase2_refresh_request_nonce <- 0L
    phase2_refresh_timeout_seconds <- 12

    next_phase2_refresh_request_id <- function() {
      phase2_refresh_request_nonce <<- phase2_refresh_request_nonce + 1L
      paste0("phase2-refresh-", phase2_refresh_request_nonce)
    }

    get_matrix_loading <- function() {
      shiny::isolate(isTRUE(matrix_loading()))
    }

    set_matrix_loading <- function(value = FALSE) {
      matrix_loading(isTRUE(value))
      invisible(isTRUE(value))
    }

    get_loading_mode <- function() {
      shiny::isolate(loading_mode() %||% "idle")
    }

    set_loading_mode <- function(value = "idle") {
      loading_mode(value %||% "idle")
      invisible(value %||% "idle")
    }

    get_pending_refresh <- function() {
      shiny::isolate(pending_refresh())
    }

    set_pending_refresh <- function(value = NULL) {
      pending_refresh(value)
      invisible(value)
    }

    get_active_refresh_request_id <- function() {
      shiny::isolate(active_refresh_request_id() %||% NULL)
    }

    set_active_refresh_request_id <- function(value = NULL) {
      active_refresh_request_id(value %||% NULL)
      invisible(value %||% NULL)
    }

    get_phase2_controls_ready <- function() {
      shiny::isolate(isTRUE(phase2_controls_ready()))
    }

    set_phase2_controls_ready <- function(value = FALSE) {
      phase2_controls_ready(isTRUE(value))
      invisible(isTRUE(value))
    }

    get_phase2_applied_settings <- function() {
      shiny::isolate(phase2_applied_settings() %||% NULL)
    }

    set_phase2_applied_settings <- function(value = NULL) {
      phase2_applied_settings(value %||% NULL)
      invisible(value %||% NULL)
    }

    get_phase2_programmatic_strat_update <- function() {
      shiny::isolate(isTRUE(phase2_programmatic_strat_update()))
    }

    set_phase2_programmatic_strat_update <- function(value = FALSE) {
      phase2_programmatic_strat_update(isTRUE(value))
      invisible(isTRUE(value))
    }

    bump_phase2_control_reset_nonce <- function() {
      phase2_control_reset_nonce(shiny::isolate(phase2_control_reset_nonce() %||% 0L) + 1L)
      invisible(shiny::isolate(phase2_control_reset_nonce()))
    }

    resolve_phase2_settings <- function(settings = NULL) {
      normalize_phase2_settings(
        rv,
        settings %||% shiny::isolate(rv$phase2_settings %||% empty_phase2_settings())
      )
    }

    phase2_settings_match <- function(left, right) {
      if (is.null(left) || is.null(right)) {
        return(FALSE)
      }

      identical(resolve_phase2_settings(left), resolve_phase2_settings(right))
    }

    phase2_input_settings <- function() {
      list(
        metric_filter = input$metric_filter,
        strat_filter = input$strat_filter,
        sig_threshold = input$sig_threshold,
        support_threshold = input$support_threshold
      )
    }

    phase2_inputs_bound <- function() {
      !is.null(input$metric_filter) &&
        !is.null(input$strat_filter) &&
        !is.null(input$sig_threshold) &&
        !is.null(input$support_threshold)
    }

    reset_phase2_controls_state <- function() {
      set_phase2_controls_ready(FALSE)
      set_phase2_programmatic_strat_update(FALSE)
      set_phase2_applied_settings(NULL)
      bump_phase2_control_reset_nonce()
      invisible(NULL)
    }

    phase2_results_are_renderable <- function() {
      shiny::isolate({
        if (!isTRUE(phase2_workspace_active(isolate_state = TRUE))) {
          return(FALSE)
        }

        res <- rv$cross_metric_consistency %||% NULL
        ranking <- rv$phase2_ranking %||% NULL

        !is.null(res) &&
          is.data.frame(res$summary %||% NULL) &&
          nrow(res$summary) > 0 &&
          !is.null(ranking) &&
          is.data.frame(ranking) &&
          nrow(ranking) > 0
      })
    }

    phase2_refresh_detail <- function(mode = "manual_full") {
      switch(
        mode,
        manual_full = paste(
          "Loading page, please wait. Computing the consistency matrix and updating",
          "the support score heatmap, ranked summary, tier classification, and current",
          "metric highlights. No second click is needed."
        ),
        auto_full = "Updating the consistency matrix and cross-metric results for the current filters.",
        tier_only = "Updating the tier classification and current metric highlights from the current matrix.",
        "Updating cross-metric analysis."
      )
    }

    phase2_refresh_error_prefix <- function(mode = "manual_full") {
      switch(
        mode,
        manual_full = "Consistency computation failed",
        auto_full = "Cross-metric analysis update failed",
        tier_only = "Tier classification update failed",
        "Cross-metric analysis update failed"
      )
    }

    finish_consistency_refresh <- function(request_id = NULL,
                                          notify_workspace = TRUE) {
      current_request_id <- get_active_refresh_request_id()
      if (is.null(current_request_id)) {
        return(invisible(FALSE))
      }

      request_id <- request_id %||% current_request_id
      if (!is.null(request_id) &&
          !identical(as.character(current_request_id), as.character(request_id))) {
        return(invisible(FALSE))
      }

      set_active_refresh_request_id(NULL)
      set_matrix_loading(FALSE)
      set_loading_mode("idle")
      remove_final_loading_notification(session, loading_notification_id)

      if (identical(workspace_scope, "analysis") &&
          isTRUE(phase2_workspace_active(isolate_state = TRUE)) &&
          analysis_tab_request_is_current(rv, rv$analysis_tab_request_id %||% NULL) &&
          identical(get_analysis_tab_status(rv, "cross_metric"), "loading")) {
        complete_analysis_tab_preload(
          rv,
          "cross_metric",
          "ready",
          rv$analysis_tab_request_id %||% NULL
        )
      }

      if (isTRUE(notify_workspace)) {
        notify_workspace_refresh(rv)
      }

      next_request <- get_pending_refresh()
      set_pending_refresh(NULL)

      if (!is.null(next_request)) {
        request_consistency_refresh(
          mode = next_request$mode,
          settings = next_request$settings,
          error_prefix = next_request$error_prefix
        )
      }

      invisible(TRUE)
    }

    ## ── Shared recompute helper ─────────────────────────────────────────────
    request_consistency_refresh <- function(mode = c("manual_full", "auto_full", "tier_only"),
                                            settings = NULL,
                                            error_prefix = NULL) {
      if (!isTRUE(phase2_workspace_active(isolate_state = TRUE))) {
        return(invisible(FALSE))
      }

      mode <- match.arg(mode)
      resolved_settings <- resolve_phase2_settings(settings)
      error_prefix <- error_prefix %||% phase2_refresh_error_prefix(mode)
      refresh_request <- list(
        mode = mode,
        settings = resolved_settings,
        error_prefix = error_prefix
      )

      if (!identical(mode, "manual_full") &&
          isTRUE(phase2_settings_match(resolved_settings, get_phase2_applied_settings()))) {
        return(invisible(FALSE))
      }

      if (isTRUE(get_matrix_loading())) {
        if (identical(mode, "manual_full")) {
          return(invisible(FALSE))
        }

        set_phase2_applied_settings(resolved_settings)
        set_pending_refresh(refresh_request)
        return(invisible(FALSE))
      }

      set_phase2_applied_settings(resolved_settings)
      set_matrix_loading(TRUE)
      set_loading_mode(mode)
      request_id <- next_phase2_refresh_request_id()
      set_active_refresh_request_id(request_id)

      show_final_loading_notification(
        session,
        loading_notification_id,
        "Loading cross-metric analysis",
        phase2_refresh_detail(mode),
        close_button = FALSE
      )

      session$onFlushed(function() {
        shiny::isolate({
          later::later(function() {
            tryCatch(
              {
                if (identical(mode, "tier_only")) {
                  refresh_phase2_ranking_shared(rv, resolved_settings, persist_settings = FALSE)
                } else {
                  recompute_phase2_shared(rv, resolved_settings, persist_settings = FALSE)
                }

                consistency_results(rv$cross_metric_consistency %||% NULL)

                session$onFlushed(function() {
                  shiny::isolate({
                    if (!identical(get_active_refresh_request_id(), request_id)) {
                      return(invisible(NULL))
                    }

                    if (isTRUE(phase2_results_are_renderable())) {
                      session$sendCustomMessage(
                        "watchPhase2ResultsReady",
                        list(
                          rootId = ns("phase2_results_root"),
                          heatmapId = ns("heatmap"),
                          rankingTableId = ns("ranking_table"),
                          tierSectionId = ns("phase2_tier_classification"),
                          highlightsSectionId = ns("phase2_current_metric_highlights"),
                          readyInputId = phase2_results_ready_input_id,
                          requestId = request_id,
                          expectRanking = TRUE,
                          expectTier = TRUE,
                          expectHighlights = TRUE,
                          timeoutMs = phase2_refresh_timeout_seconds * 1000
                        )
                      )
                    } else {
                      later::later(function() {
                        finish_consistency_refresh(request_id = request_id)
                        invisible(NULL)
                      }, 0.15)
                    }

                    later::later(function() {
                      finish_consistency_refresh(request_id = request_id)
                      invisible(NULL)
                    }, phase2_refresh_timeout_seconds)
                    invisible(NULL)
                  })
                  invisible(NULL)
                }, once = TRUE)
              },
              error = function(e) {
                set_active_refresh_request_id(NULL)
                set_matrix_loading(FALSE)
                set_loading_mode("idle")
                remove_final_loading_notification(session, loading_notification_id)

                next_request <- get_pending_refresh()
                set_pending_refresh(NULL)

                showNotification(
                  paste0(error_prefix, ": ", conditionMessage(e)),
                  type = "error", duration = 8
                )

                if (identical(workspace_scope, "analysis") &&
                    isTRUE(phase2_workspace_active(isolate_state = TRUE)) &&
                    analysis_tab_request_is_current(rv, rv$analysis_tab_request_id %||% NULL) &&
                    identical(get_analysis_tab_status(rv, "cross_metric"), "loading")) {
                  complete_analysis_tab_preload(
                    rv,
                    "cross_metric",
                    "error",
                    rv$analysis_tab_request_id %||% NULL
                  )
                }

                if (!is.null(next_request)) {
                  request_consistency_refresh(
                    mode = next_request$mode,
                    settings = next_request$settings,
                    error_prefix = next_request$error_prefix
                  )
                }
              }
            )
            invisible(NULL)
          }, 0)
          invisible(NULL)
        })
        invisible(NULL)
      }, once = TRUE)

      invisible(TRUE)
    }

    observeEvent(input$phase2_results_ready, {
      finish_consistency_refresh(request_id = input$phase2_results_ready %||% NULL)
    }, ignoreInit = TRUE)

    observeEvent(rv$workspace_modal_nonce, {
      reset_phase2_controls_state()
    }, ignoreInit = FALSE)

    observeEvent(rv$phase2_settings, {
      reset_phase2_controls_state()
    }, ignoreInit = TRUE)

    observeEvent(rv$data, {
      reset_phase2_controls_state()
    }, ignoreInit = TRUE)

    observeEvent(rv$all_layer1_results, {
      reset_phase2_controls_state()
    }, ignoreInit = TRUE)

    observeEvent(rv$config_version, {
      reset_phase2_controls_state()
    }, ignoreInit = TRUE)

    observe({
      phase2_control_reset_nonce()

      if (!phase2_inputs_bound()) {
        return(invisible(NULL))
      }

      if (isTRUE(get_phase2_controls_ready())) {
        return(invisible(NULL))
      }

      set_phase2_applied_settings(resolve_phase2_settings(phase2_input_settings()))
      set_phase2_controls_ready(TRUE)
      set_phase2_programmatic_strat_update(FALSE)
      invisible(NULL)
    })

    observeEvent(rv$cross_metric_consistency, {
      consistency_results(rv$cross_metric_consistency %||% NULL)
    }, ignoreInit = FALSE)

    observeEvent(input$compute_matrix, {
      req(length(get_phase2_metric_choices(rv)) >= 2, input$metric_filter, input$strat_filter)
      request_consistency_refresh(
        mode = "manual_full",
        settings = resolve_phase2_settings(phase2_input_settings())
      )
    })

    ## ── Re-classify tiers when slider changes ─────────────────────────────────
    observeEvent(input$support_threshold, {
      if (!isTRUE(get_phase2_controls_ready())) {
        return(invisible(NULL))
      }

      req(rv$cross_metric_consistency, rv$cross_metric_consistency$summary)
      resolved_settings <- resolve_phase2_settings(list(
        metric_filter = input$metric_filter %||% phase2_settings()$metric_filter,
        strat_filter = input$strat_filter %||% phase2_settings()$strat_filter,
        sig_threshold = input$sig_threshold %||% phase2_settings()$sig_threshold,
        support_threshold = input$support_threshold
      ))
      request_consistency_refresh(
        mode = "tier_only",
        settings = resolved_settings
      )
    }, ignoreInit = TRUE)

    ## ── Re-run consistency when significance threshold changes ───────────────
    observeEvent(input$sig_threshold, {
      if (!isTRUE(get_phase2_controls_ready())) {
        return(invisible(NULL))
      }

      req(consistency_results())
      resolved_settings <- resolve_phase2_settings(list(
        metric_filter = input$metric_filter %||% phase2_settings()$metric_filter,
        strat_filter = input$strat_filter %||% phase2_settings()$strat_filter,
        sig_threshold = input$sig_threshold,
        support_threshold = input$support_threshold %||% phase2_settings()$support_threshold
      ))
      request_consistency_refresh(
        mode = "auto_full",
        settings = resolved_settings
      )
    }, ignoreInit = TRUE)

    ## ── Re-run consistency when stratification filter changes ────────────────
    observeEvent(input$metric_filter, {
      if (!isTRUE(get_phase2_controls_ready())) {
        return(invisible(NULL))
      }

      resolved_settings <- resolve_phase2_settings(list(
        metric_filter = input$metric_filter,
        strat_filter = input$strat_filter %||% phase2_settings()$strat_filter,
        sig_threshold = input$sig_threshold %||% phase2_settings()$sig_threshold,
        support_threshold = input$support_threshold %||% phase2_settings()$support_threshold
      ))

      if (isTRUE(phase2_settings_match(resolved_settings, get_phase2_applied_settings()))) {
        return(invisible(NULL))
      }

      strat_choices <- get_phase2_strat_choices(rv, resolved_settings$metric_filter)
      strat_choice_labels <- setNames(strat_choices, sapply(strat_choices, function(sk) {
        rv$strat_config[[sk]]$display_name %||% sk
      }))
      strat_selection_changed <- !identical(
        sort(input$strat_filter %||% character(0)),
        sort(resolved_settings$strat_filter %||% character(0))
      )

      set_phase2_programmatic_strat_update(strat_selection_changed)
      freezeReactiveValue(input, "strat_filter")
      shinyWidgets::updatePickerInput(
        session,
        "strat_filter",
        choices = strat_choice_labels,
        selected = resolved_settings$strat_filter
      )

      req(consistency_results())
      request_consistency_refresh(
        mode = "auto_full",
        settings = resolved_settings
      )
    }, ignoreInit = TRUE)

    observeEvent(input$strat_filter, {
      if (!isTRUE(get_phase2_controls_ready())) {
        return(invisible(NULL))
      }

      if (isTRUE(get_phase2_programmatic_strat_update())) {
        set_phase2_programmatic_strat_update(FALSE)
        return(invisible(NULL))
      }

      req(consistency_results())
      resolved_settings <- resolve_phase2_settings(list(
        metric_filter = input$metric_filter %||% phase2_settings()$metric_filter,
        strat_filter = input$strat_filter,
        sig_threshold = input$sig_threshold %||% phase2_settings()$sig_threshold,
        support_threshold = input$support_threshold %||% phase2_settings()$support_threshold
      ))
      request_consistency_refresh(
        mode = "auto_full",
        settings = resolved_settings
      )
    }, ignoreInit = TRUE)

    ## ── Save carry-forward overrides ──────────────────────────────────────────
    observeEvent(input$save_carry_forward, {
      ranking <- rv$phase2_ranking
      req(ranking, nrow(ranking) > 0)

      ## Read radio button values and map to tier labels
      updated_tiers <- sapply(ranking$stratification, function(sk) {
        carry_val <- input[[paste0("carry_", sk)]]
        if (is.null(carry_val)) return(ranking$tier[ranking$stratification == sk])
        switch(carry_val,
          "broad_use"       = "Broad-Use Candidate",
          "metric_specific" = "Metric-Specific Candidate",
          "do_not_carry"    = "Weak Candidate",
          ranking$tier[ranking$stratification == sk]
        )
      })

      rv$phase2_ranking <- ranking |>
        dplyr::mutate(tier = updated_tiers)

      showNotification("Carry-forward selections saved.", type = "message", duration = 3)
      notify_workspace_refresh(rv)
    })

    ## ── Results UI ────────────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      if (!isTRUE(phase2_workspace_active())) {
        return(NULL)
      }

      res <- consistency_results()
      req(res)

      div(
        id = ns("phase2_results_root"),
        class = "phase2-consistency-results",
        ## Support score heatmap
        card(
          card_header("Support Score Heatmap"),
          card_body(
            class = "heatmap-container",
            p(class = "text-muted small",
              textOutput(ns("sig_threshold_label"), inline = TRUE)),
            plotOutput(ns("heatmap"), height = "450px")
          )
        ),

        ## Ranked summary table
        card(
          card_header("Ranked Stratification Summary"),
          card_body(DT::DTOutput(ns("ranking_table")))
        ),

        ## Tier classification cards
        if (!is.null(rv$phase2_ranking) && nrow(rv$phase2_ranking) > 0) {
          div(
            id = ns("phase2_tier_classification"),
            card(
            card_header("Tier Classification"),
            card_body(
              layout_column_wrap(
                width = 1 / 3,
                tier_card("Broad-Use Candidate", "success",
                          rv$phase2_ranking |> dplyr::filter(tier == "Broad-Use Candidate")),
                tier_card("Metric-Specific Candidate", "warning",
                          rv$phase2_ranking |> dplyr::filter(tier == "Metric-Specific Candidate")),
                tier_card("Weak Candidate", "danger",
                          rv$phase2_ranking |> dplyr::filter(tier == "Weak Candidate"))
              )
            )
            )
          )
        },

        if (!is.null(rv$phase2_ranking) && nrow(rv$phase2_ranking) > 0) {
          div(
            id = ns("phase2_current_metric_highlights"),
            card(
            card_header("Current Metric Highlights"),
            card_body(
              p(class = "text-muted", "Broad-use candidates are surfaced automatically here for the current metric. Choose the final curve stratification on the Reference Curves tab."),
              {
                highlighted <- get_metric_phase2_passed(rv, rv$current_metric)
                if (length(highlighted) == 0) {
                  div(class = "text-muted", "No broad-use candidates are currently highlighted for this metric.")
                } else {
                  tagList(lapply(highlighted, function(sk) {
                    div(
                      class = "d-flex align-items-center gap-3 mb-2",
                      tags$strong(get_strat_display_name(rv, sk), style = "min-width: 220px;"),
                      status_badge("pass", "Broad-Use Candidate")
                    )
                  }))
                }
              }
            )
            )
          )
        },

        ## Downloads
        div(
          class = "mt-3",
          downloadButton(ns("dl_matrix"), "Download Consistency Matrix CSV",
                         class = "btn btn-outline-primary"),
          downloadButton(ns("dl_heatmap"), "Download Heatmap PNG",
                         class = "btn btn-outline-primary ms-2")
        )
      )
    })

    outputOptions(output, "consistency_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "results_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "current_metric_context", suspendWhenHidden = FALSE)

    ## ── Significance threshold label ─────────────────────────────────────────
    output$sig_threshold_label <- renderText({
      req(isTRUE(phase2_workspace_active()))
      settings <- phase2_settings()
      paste0("Cells colored by p-value significance at \u03b1 = ",
             input$sig_threshold %||% settings$sig_threshold)
    })
    outputOptions(output, "sig_threshold_label", suspendWhenHidden = FALSE)

    ## ── Heatmap ───────────────────────────────────────────────────────────────
    output$heatmap <- renderPlot({
      req(isTRUE(phase2_workspace_active()))
      res <- consistency_results()
      req(res, !is.null(res$heatmap_plot))
      res$heatmap_plot
    })
    outputOptions(output, "heatmap", suspendWhenHidden = FALSE)

    ## ── Ranking table ─────────────────────────────────────────────────────────
    output$ranking_table <- DT::renderDT({
      req(isTRUE(phase2_workspace_active()))
      ranking <- rv$phase2_ranking
      req(ranking, nrow(ranking) > 0)

      display_df <- ranking |>
        dplyr::mutate(
          strat_display = sapply(stratification, function(sk) {
            rv$strat_config[[sk]]$display_name %||% sk
          }),
          mean_effect_size = round(mean_effect_size, 4)
        ) |>
        dplyr::select(
          Stratification = strat_display,
          `Metrics Tested` = n_metrics_tested,
          `# Promising` = n_promising,
          `# Possible` = n_possible,
          `Mean Effect Size` = mean_effect_size,
          `Consistency Score` = consistency_score,
          Tier = tier
        )

      DT::datatable(
        display_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE,
                       order = list(list(5, "desc"))),
        rownames = FALSE,
        class = "compact stripe"
      ) |>
        DT::formatStyle(
          "Tier",
          backgroundColor = DT::styleEqual(
            c("Broad-Use Candidate", "Metric-Specific Candidate", "Weak Candidate"),
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)", "rgba(231,76,60,0.15)")
          )
        )
    })
    outputOptions(output, "ranking_table", suspendWhenHidden = FALSE)

    ## ── Downloads ─────────────────────────────────────────────────────────────
    output$dl_matrix <- downloadHandler(
      filename = function() paste0("strat_consistency_matrix_", format(Sys.time(), "%Y%m%d"), ".csv"),
      content = function(file) {
        res <- consistency_results()
        if (!is.null(res) && nrow(res$consistency_matrix) > 0) {
          write.csv(res$consistency_matrix, file, row.names = FALSE)
        } else {
          write.csv(data.frame(message = "No data"), file, row.names = FALSE)
        }
      }
    )

    output$dl_heatmap <- downloadHandler(
      filename = function() paste0("strat_consistency_heatmap_", format(Sys.time(), "%Y%m%d"), ".png"),
      content = function(file) {
        res <- consistency_results()
        if (!is.null(res) && !is.null(res$heatmap_plot)) {
          ggplot2::ggsave(file, res$heatmap_plot, width = 10, height = 6, dpi = 300)
        }
      }
    )
  })
}


## ── Helper for tier classification cards ──────────────────────────────────────
tier_card <- function(tier_name, color, tier_data) {
  n <- nrow(tier_data)
  strats <- if (n > 0) {
    paste(tier_data$stratification, collapse = ", ")
  } else {
    "None"
  }

  card(
    class = paste0("border-", color),
    card_header(
      class = paste0("bg-", color, if (color != "warning") " text-white" else " text-dark"),
      tier_name
    ),
    card_body(
      p(paste0(n, " stratification(s)")),
      p(class = "text-muted", style = "font-size: 0.85rem;", strats)
    )
  )
}
