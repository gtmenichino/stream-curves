## -- Module: Phase 4 -- Reference Curve Finalization ------------------------------
## Empirical scoring curve workflow: compute descriptive statistics and build
## piecewise-linear scoring curves directly from reference-site distributions.
## Stratified metrics show ALL strata simultaneously (no switching).
## Non-stratified metrics delegate to mod_ref_curve sub-module.

library(shiny)
library(bslib)

mod_phase4_finalization_ui <- function(id, dialog_mode = FALSE) {
  ns <- NS(id)
  uiOutput(ns("phase4_page"))
}

mod_phase4_finalization_server <- function(id, rv, dialog_mode = FALSE, workspace_scope = "standalone") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    prev_metric <- reactiveVal(NULL)
    registered_stratum_editors <- reactiveVal(character(0))
    stratum_editor_ids <- reactiveVal(stats::setNames(character(0), character(0)))
    next_stratum_editor_seq <- reactiveVal(0L)
    artifacts_loading <- reactiveVal(FALSE)
    artifacts_error <- reactiveVal(NULL)
    workspace_scope <- match.arg(workspace_scope, c("standalone", "analysis"))

    phase4_workspace_active <- function(isolate_state = FALSE) {
      if (!isTRUE(dialog_mode)) {
        return(TRUE)
      }

      workspace_scope_is_active(
        rv,
        workspace_scope = workspace_scope,
        standalone_modal_type = "phase4",
        isolate_state = isolate_state
      )
    }

    allocate_stratum_editor_ids <- function(levels) {
      levels <- as.character(levels %||% character(0))
      if (length(levels) == 0) {
        return(stats::setNames(character(0), character(0)))
      }

      start <- next_stratum_editor_seq()
      ids <- paste0(
        "curve_editor_",
        seq.int(from = start + 1L, length.out = length(levels))
      )
      next_stratum_editor_seq(start + length(levels))
      stats::setNames(ids, levels)
    }

    sync_phase4_inputs <- function(metric = rv$current_metric) {
      mc <- rv$metric_config[[metric]] %||% NULL
      if (is.null(mc)) return(invisible(NULL))
      updateNumericInput(session, "min_sample", value = mc$min_sample_size %||% 10)
      updateRadioButtons(session, "transform", selected = mc$preferred_transform %||% "none")
      invisible(NULL)
    }

    ## -- Data gate: show alert or full page ------------------------------------
    output$phase4_page <- renderUI({
      if (!isTRUE(phase4_workspace_active())) {
        return(NULL)
      }

      if (is.null(rv$data)) return(no_data_alert())

      if (isTRUE(dialog_mode)) {
        return(div(
          class = "workspace-phase-body",
          card(
            class = "mb-3",
            card_header("Metric Settings"),
            card_body(
              layout_column_wrap(
                width = 1 / 2,
                uiOutput(ns("metric_info")),
                div(
                  class = "workspace-phase-settings",
                  numericInput(ns("min_sample"), "Min sample size:", value = 10, min = 3, max = 30),
                  radioButtons(ns("transform"), "Transform:",
                               choices = c("None" = "none", "Log" = "log"),
                               selected = "none")
                )
              )
            )
          ),
          uiOutput(ns("artifact_status")),
          uiOutput(ns("strat_confirm_card")),
          uiOutput(ns("stratified_display")),
          uiOutput(ns("unstratified_display"))
        ))
      }

      layout_sidebar(
        ## -- Sidebar ---------------------------------------------------------------
        sidebar = sidebar(
          width = 300,
          title = "Phase 4: Finalize",

          ## Metric picker
          uiOutput(ns("metric_picker")),

          ## Metric info card
          uiOutput(ns("metric_info")),

          ## Advanced settings
          tags$hr(),
          bslib::accordion(
            id = ns("advanced_accordion"),
            open = FALSE,
            bslib::accordion_panel(
              "Advanced Settings",
              numericInput(ns("min_sample"), "Min sample size:", value = 10, min = 3, max = 30),
              radioButtons(ns("transform"), "Transform:",
                           choices = c("None" = "none", "Log" = "log"),
                           selected = "none")
            )
          )
        ),

        ## -- Main content ----------------------------------------------------------
        tagList(
          ## Stratification confirmation card
          uiOutput(ns("strat_confirm_card")),

          ## All-strata display (returns NULL if non-stratified)
          uiOutput(ns("stratified_display")),

          ## Single-stratum mod_ref_curve (returns NULL if stratified)
          uiOutput(ns("unstratified_display"))
        )
      )
    })
    outputOptions(output, "phase4_page", suspendWhenHidden = FALSE)

    observe({
      if (!isTRUE(phase4_workspace_active())) {
        return(invisible(NULL))
      }

      req(rv$current_metric)
      metric <- rv$current_metric
      sync_metric_decision_state(
        rv,
        metric,
        build_metric_strat_decision(rv, metric, get_metric_curve_stratification(rv, metric))
      )
    })

    ## -- Build metric picker choices -------------------------------------------
    output$metric_picker <- renderUI({
      mc <- rv$metric_config
      families <- list()
      for (mk in names(mc)) {
        fam <- mc[[mk]]$metric_family
        if (fam == "categorical") next
        fam_label <- switch(fam,
          continuous = "Continuous", proportion = "Proportion",
          count = "Count", fam)
        families[[fam_label]] <- c(families[[fam_label]], setNames(mk, mc[[mk]]$display_name))
      }

      selectInput(ns("metric_select"), "Select Metric:",
                  choices = families,
                  selected = rv$current_metric %||% "perRiffle")
    })

    ## -- Stratum info reactive -------------------------------------------------
    stratum_info <- reactive({
      req(isTRUE(phase4_workspace_active()))
      strat <- rv$strat_decision_user

      if (is.null(strat) || is.na(strat$selected_strat) || strat$decision_type != "single") {
        return(list(has_strata = FALSE, levels = NULL, strat_var = NULL))
      }

      strat_var <- strat$selected_strat
      levels <- sort(unique(get_stratification_values(rv$data, strat_var, rv$strat_config)))
      levels <- levels[!is.na(levels)]
      list(has_strata = TRUE, levels = levels, strat_var = strat_var)
    })

    strat_values <- reactive({
      info <- stratum_info()
      req(info$has_strata)
      get_stratification_values(rv$data, info$strat_var, rv$strat_config)
    })

    observe({
      if (!isTRUE(phase4_workspace_active())) {
        return(invisible(NULL))
      }

      info <- stratum_info()

      if (!isTRUE(info$has_strata) || length(info$levels %||% character(0)) == 0) {
        stratum_editor_ids(stats::setNames(character(0), character(0)))
        registered_stratum_editors(character(0))
        return(invisible(NULL))
      }

      current_map <- stratum_editor_ids()
      if (!identical(names(current_map), info$levels)) {
        stratum_editor_ids(allocate_stratum_editor_ids(info$levels))
        registered_stratum_editors(character(0))
      }

      invisible(NULL)
    })

    current_phase4_decision <- reactive({
      req(rv$current_metric)
      get_metric_phase4_decision_state(rv, rv$current_metric)
    })

    sync_analysis_tab_state <- function(status = NULL, request_id = NULL, complete = FALSE) {
      request_id <- request_id %||% shiny::isolate(rv$analysis_tab_request_id %||% NULL)
      if (!isTRUE(dialog_mode) ||
          !identical(workspace_scope, "analysis") ||
          !isTRUE(phase4_workspace_active(isolate_state = TRUE)) ||
          !analysis_tab_request_is_current(rv, request_id)) {
        return(invisible(NULL))
      }

      resolved_status <- status %||% if (is.null(artifacts_error()) || !nzchar(artifacts_error() %||% "")) {
        "ready"
      } else {
        "error"
      }

      set_analysis_tab_status(rv, "reference_curves", resolved_status, request_id)
      if (isTRUE(complete)) {
        complete_analysis_tab_preload(rv, "reference_curves", resolved_status, request_id)
      }

      invisible(resolved_status)
    }

    refresh_phase4_artifacts <- function(metric = rv$current_metric,
                                         request_id = NULL,
                                         complete = FALSE,
                                         defer = FALSE) {
      if (!isTRUE(phase4_workspace_active(isolate_state = TRUE))) {
        return(invisible(FALSE))
      }

      target_metric <- metric %||% NULL
      if (is.null(target_metric) || identical(target_metric, "")) {
        return(invisible(FALSE))
      }

      request_id <- request_id %||% shiny::isolate(rv$analysis_tab_request_id %||% NULL)
      if (!metric_needs_phase4_artifact_refresh(rv, target_metric, artifact_mode = "full")) {
        artifacts_loading(FALSE)
        artifacts_error(NULL)
        sync_analysis_tab_state(request_id = request_id, complete = complete)
        return(invisible(FALSE))
      }

      artifacts_loading(TRUE)
      artifacts_error(NULL)

      run_refresh <- function() {
        if (isTRUE(dialog_mode) &&
            identical(workspace_scope, "analysis") &&
            !analysis_tab_request_is_current(rv, request_id)) {
          artifacts_loading(FALSE)
          return(invisible(NULL))
        }

        tryCatch(
          {
            preload_metric_phase4_workspace(
              rv,
              target_metric,
              artifact_mode = "full"
            )
            artifacts_error(NULL)
          },
          error = function(e) {
            artifacts_error(conditionMessage(e))
          },
          finally = {
            artifacts_loading(FALSE)
            sync_analysis_tab_state(request_id = request_id, complete = complete)
          }
        )

        invisible(NULL)
      }

      if (isTRUE(defer)) {
        session$onFlushed(function() {
          shiny::isolate(run_refresh())
          invisible(NULL)
        }, once = TRUE)
      } else {
        run_refresh()
      }

      invisible(TRUE)
    }

    persist_stratified_curve_results <- function(results) {
      metric <- shiny::isolate(rv$current_metric)
      if (is.null(metric) || identical(metric, "")) {
        return(invisible(NULL))
      }
      decision_tbl <- shiny::isolate(current_phase4_decision())
      info <- shiny::isolate(stratum_info())

      stratum_results <- purrr::imap(results, function(result, lvl) {
        list(reference_curve = hydrate_reference_curve_result(
          result,
          shiny::isolate(rv$data),
          metric,
          shiny::isolate(rv$metric_config),
          stratum_label = lvl,
          artifact_mode = "full"
        ))
      })

      curve_rows <- extract_metric_phase4_curve_rows(list(stratum_results = stratum_results))
      phase4_signature <- cache_metric_phase4_results(
        rv,
        metric,
        decision_tbl = decision_tbl,
        stratum_results = stratum_results,
        artifact_mode = "full"
      )

      if (metric %in% names(rv$completed_metrics)) {
        update_metric_phase4_completed_entry(rv, metric, list(
          stratified = TRUE,
          strat_var = info$strat_var,
          strat_decision = decision_tbl,
          stratum_results = stratum_results,
          phase4_signature = phase4_signature,
          phase4_artifact_mode = "full",
          phase4_curve_rows = curve_rows
        ))
      }

      invisible(stratum_results)
    }

    ## -- Metric change observer ------------------------------------------------
    observeEvent(input$metric_select, {
      old_metric <- prev_metric()
      new_metric <- input$metric_select
      if (!is.null(old_metric) && identical(old_metric, new_metric)) return()

      ## Save current metric state before switch (no per-stratum state cycling)
      if (!is.null(old_metric) && old_metric != "") {
        save_metric_phase_state(rv, old_metric)
      }
      rv$current_metric <- new_metric
      prev_metric(new_metric)
      restore_metric_phase_state(rv, new_metric)

      ## Update advanced settings from config
      sync_phase4_inputs(new_metric)
    }, ignoreInit = TRUE)

    observeEvent(rv$workspace_modal_ready_nonce, {
      if (isTRUE(dialog_mode) && isTRUE(phase4_workspace_active())) {
        modal_metric <- rv$workspace_modal_metric %||% rv$current_metric
        artifacts_loading(FALSE)
        artifacts_error(NULL)
        session$onFlushed(function() {
          if (is.null(modal_metric) || identical(modal_metric, "")) {
            return(invisible(NULL))
          }
          shiny::isolate({
            sync_phase4_inputs(modal_metric)
            invisible(NULL)
          })
          invisible(NULL)
        }, once = TRUE)
      }
    }, ignoreInit = TRUE)

    observeEvent(rv$analysis_tab_preload_nonce, {
      if (!isTRUE(dialog_mode) ||
          !identical(workspace_scope, "analysis") ||
          !isTRUE(phase4_workspace_active()) ||
          !identical(rv$analysis_tab_preload_tab %||% NULL, "reference_curves")) {
        return(invisible(NULL))
      }

      request_id <- rv$analysis_tab_request_id %||% NULL
      modal_metric <- rv$workspace_modal_metric %||% rv$current_metric
      if (is.null(modal_metric) || identical(modal_metric, "") ||
          !analysis_tab_request_is_current(rv, request_id)) {
        return(invisible(NULL))
      }

      refresh_phase4_artifacts(
        modal_metric,
        request_id = request_id,
        complete = TRUE,
        defer = TRUE
      )
    }, ignoreInit = TRUE)

    output$artifact_status <- renderUI({
      if (!isTRUE(dialog_mode) ||
          !identical(workspace_scope, "analysis") ||
          !isTRUE(phase4_workspace_active())) {
        return(NULL)
      }

      if (isTRUE(artifacts_loading())) {
        return(div(
          class = "alert alert-info d-flex align-items-center gap-2",
          icon("spinner", class = "fa-spin"),
          tags$span("Loading full reference curve visuals. Summary results stay available while plots regenerate.")
        ))
      }

      error_text <- artifacts_error()
      if (!is.null(error_text) && nzchar(error_text)) {
        return(div(
          class = "alert alert-danger d-flex justify-content-between align-items-center flex-wrap gap-2",
          tags$span(paste0("Could not load full reference curve visuals: ", error_text)),
          actionButton(
            ns("retry_artifacts"),
            "Retry details",
            class = "btn btn-outline-danger btn-sm"
          )
        ))
      }

      NULL
    })

    observeEvent(input$retry_artifacts, {
      refresh_phase4_artifacts(
        rv$current_metric,
        request_id = rv$analysis_tab_request_id %||% NULL,
        complete = TRUE,
        defer = TRUE
      )
    }, ignoreInit = TRUE)

    ## -- Initialize phase4_data when metric/strat changes ----------------------
    observe({
      if (!isTRUE(phase4_workspace_active())) {
        return(invisible(NULL))
      }

      req(rv$current_metric)
      ## Always set full dataset; stratified display handles its own filtering
      rv$phase4_data <- rv$data
      rv$current_stratum_level <- NULL
    })

    ## -- Advanced settings write-back ------------------------------------------
    observeEvent(input$min_sample, {
      req(rv$current_metric)
      mc <- isolate(rv$metric_config)
      if (!is.null(mc[[rv$current_metric]]) &&
          !identical(mc[[rv$current_metric]]$min_sample_size, input$min_sample)) {
        mc[[rv$current_metric]]$min_sample_size <- input$min_sample
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

    ## -- Metric info card ------------------------------------------------------
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
                )
              )
            }
          )
        ),
        if (completed) div(class = "mt-2", status_badge("pass", "COMPLETED"))
      )
    })

    ## -- Stratification confirmation card --------------------------------------
    output$strat_confirm_card <- renderUI({
      req(rv$current_metric)
      metric <- rv$current_metric
      strat <- current_phase4_decision()
      current_choice <- get_metric_curve_stratification(rv, metric)
      recommended_choice <- get_metric_curve_strat_recommendation(rv, metric)
      choice_choices <- get_metric_curve_strat_choices(rv, metric)

      info <- stratum_info()
      strat_name <- if (strat$decision_type == "single" && !is.na(strat$selected_strat)) {
        sc <- rv$strat_config[[strat$selected_strat]]
        sc$display_name %||% strat$selected_strat
      } else {
        "None"
      }

      selector_card <- card(
        class = "mb-3",
        card_header("Curve Stratification"),
        card_body(
          shinyWidgets::pickerInput(
            ns("curve_strat_choice"),
            "Stratification used for curves:",
            choices = choice_choices,
            selected = current_choice,
            multiple = FALSE,
            width = "100%",
            options = shinyWidgets::pickerOptions(
              container = ".modal-dialog.workspace-modal-dialog",
              size = 8,
              liveSearch = FALSE
            )
          ),
          div(
            class = if (identical(current_choice, recommended_choice)) "alert alert-info mb-0 py-2" else "alert alert-warning mb-0 py-2",
            icon("wand-magic-sparkles"),
            tags$strong(" Recommended stratification: "),
            get_metric_curve_strat_label(rv, metric, recommended_choice)
          )
        )
      )

      if (info$has_strata) {
        ## Build level listing with sample sizes
        level_labels <- sapply(info$levels, function(lvl) {
          n <- sum(strat_values() == lvl, na.rm = TRUE)
          paste0(lvl, " (n=", n, ")")
        })

        tagList(
          selector_card,
          div(
            class = "alert alert-info mb-3",
            tags$strong("Stratification: "), strat_name,
            " | Mode: Subset (separate curve per level)",
            tags$br(),
            "Levels: ", paste(level_labels, collapse = ", ")
          )
        )
      } else {
        tagList(
          selector_card,
          div(
            class = "alert alert-info mb-3",
            tags$strong("Stratification: "), strat_name,
          if (strat$decision_type == "single" && !is.na(strat$selected_strat)) {
            tagList(
              " | ",
              sprintf("p = %.4f | groups = %d | min n = %d",
                       strat$selected_p_value %||% NA_real_,
                       strat$selected_n_groups %||% NA_integer_,
                       strat$selected_min_n %||% NA_integer_)
            )
          }
          )
        )
      }
    })

    observeEvent(input$curve_strat_choice, {
      req(rv$current_metric)
      metric <- rv$current_metric
      old_choice <- get_metric_curve_stratification(rv, metric)
      new_choice <- set_metric_curve_stratification(rv, metric, input$curve_strat_choice %||% "none")

      if (!identical(old_choice, new_choice)) {
        notify_workspace_refresh(rv)
      }
    }, ignoreInit = TRUE)

    ## -- Sub-module server (always runs; UI conditionally shown) ----------------
    ref_curve_events <- mod_ref_curve_server("ref_curve", rv, workspace_scope = workspace_scope)

    ## =========================================================================
    ## NON-STRATIFIED DISPLAY
    ## =========================================================================

    output$unstratified_display <- renderUI({
      if (!isTRUE(phase4_workspace_active())) {
        return(NULL)
      }

      info <- stratum_info()
      if (info$has_strata) return(NULL)
      mod_ref_curve_ui(ns("ref_curve"))
    })

    ## =========================================================================
    ## ALL-STRATA SIMULTANEOUS DISPLAY
    ## =========================================================================

    ## -- Compute all strata curves eagerly -------------------------------------
    all_strata_results <- reactive({
      req(isTRUE(phase4_workspace_active()))
      info <- stratum_info()
      req(info$has_strata)
      metric <- rv$current_metric
      req(metric)
      decision_tbl <- current_phase4_decision()
      strat_vals <- strat_values()

      if (metric_has_phase4_cache(rv, metric, decision_tbl, artifact_mode = "summary")) {
        cached <- get_metric_phase4_cached_result(rv, metric)
        cached_results <- lapply(info$levels, function(lvl) {
          entry <- cached$stratum_results[[lvl]] %||% NULL
          if (is.null(entry)) {
            return(NULL)
          }

          hydrate_reference_curve_result(
            entry$reference_curve %||% entry,
            rv$data[strat_vals == lvl, , drop = FALSE],
            metric,
            rv$metric_config,
            stratum_label = lvl,
            artifact_mode = cached$artifact_mode %||% "full"
          )
        })
        names(cached_results) <- info$levels

        if (all(vapply(cached_results, Negate(is.null), logical(1)))) {
          return(cached_results)
        }
      }

      results <- list()
      for (lvl in info$levels) {
        stratum_data <- rv$data[strat_vals == lvl, , drop = FALSE]
        results[[lvl]] <- build_reference_curve(
          stratum_data,
          metric,
          rv$metric_config,
          stratum_label = lvl
        )
      }
      results
    })

    ## -- Bind curve_row tibbles across strata ----------------------------------
    all_strata_curve_rows <- reactive({
      req(isTRUE(phase4_workspace_active()))
      results <- all_strata_results()
      req(results)
      metric <- rv$current_metric
      dplyr::bind_rows(purrr::imap(results, function(result, lvl) {
        normalized <- normalize_reference_curve_result(
          result,
          metric_config = rv$metric_config,
          metric_key = metric,
          stratum_label = lvl
        )
        normalized$curve_row %||% tibble::tibble()
      }))
    })

    observe({
      if (!isTRUE(phase4_workspace_active())) {
        return(invisible(NULL))
      }

      info <- stratum_info()
      req(info$has_strata)
      editor_id_map <- stratum_editor_ids()
      req(length(editor_id_map) == length(info$levels))

      new_editor_ids <- setdiff(unname(editor_id_map), registered_stratum_editors())
      if (length(new_editor_ids) == 0) {
        return(invisible(NULL))
      }

      for (lvl in info$levels) {
        editor_id <- editor_id_map[[lvl]]
        if (!(editor_id %in% new_editor_ids)) {
          next
        }

        local({
          stratum_level <- lvl
          child_editor_id <- editor_id

          mod_reference_curve_editor_server(
            child_editor_id,
            current_result = reactive({
              results <- all_strata_results()
              results[[stratum_level]] %||% NULL
            }),
            higher_is_better = reactive({
              req(rv$current_metric)
              rv$metric_config[[rv$current_metric]]$higher_is_better
            }),
            on_apply = function(points) {
              metric <- shiny::isolate(rv$current_metric)
              strat_vals <- shiny::isolate(strat_values())
              metric_config <- shiny::isolate(rv$metric_config)
              results <- shiny::isolate(all_strata_results())
              stratum_data <- shiny::isolate(rv$data[strat_vals == stratum_level, , drop = FALSE])

              results[[stratum_level]] <- build_reference_curve_from_points(
                stratum_data,
                metric,
                metric_config,
                curve_points = points,
                stratum_label = stratum_level
              )

              persist_stratified_curve_results(results)
              notify_workspace_refresh(rv)
              showNotification(
                paste0("Applied manual curve edits for stratum '", stratum_level, "'."),
                type = "message",
                duration = 4
              )
            },
            on_reset = function() {
              metric <- shiny::isolate(rv$current_metric)
              strat_vals <- shiny::isolate(strat_values())
              metric_config <- shiny::isolate(rv$metric_config)
              results <- shiny::isolate(all_strata_results())
              stratum_data <- shiny::isolate(rv$data[strat_vals == stratum_level, , drop = FALSE])

              results[[stratum_level]] <- build_reference_curve(
                stratum_data,
                metric,
                metric_config,
                stratum_label = stratum_level
              )

              persist_stratified_curve_results(results)
              notify_workspace_refresh(rv)
              showNotification(
                paste0("Reset stratum '", stratum_level, "' to the auto-generated curve."),
                type = "message",
                duration = 4
              )
            }
          )
        })
      }

      registered_stratum_editors(union(registered_stratum_editors(), new_editor_ids))
      invisible(NULL)
    })

    ## -- Stratified display UI -------------------------------------------------
    output$stratified_display <- renderUI({
      if (!isTRUE(phase4_workspace_active())) {
        return(NULL)
      }

      info <- stratum_info()
      if (!info$has_strata) return(NULL)

      results <- all_strata_results()
      req(results)
      editor_id_map <- stratum_editor_ids()
      req(length(editor_id_map) == length(info$levels))
      curve_rows <- all_strata_curve_rows()
      req(curve_rows)

      metric <- rv$current_metric
      mc <- rv$metric_config[[metric]]
      min_n <- mc$min_sample_size %||% 10

      ## Per-stratum sample size warnings
      warnings <- list()
      strat_vals <- strat_values()
      for (lvl in info$levels) {
        n <- sum(strat_vals == lvl, na.rm = TRUE)
        if (n < min_n) {
          warnings[[lvl]] <- div(
            class = "alert alert-warning py-2 mb-2",
            icon("exclamation-triangle"),
            sprintf(" Stratum '%s' has only %d observations (minimum: %d). Results may be unreliable.",
                    lvl, n, min_n)
          )
        }
      }

      ## Check how many strata have valid (complete) curves
      valid_rows <- curve_rows |> dplyr::filter(curve_status == "complete")
      n_valid <- nrow(valid_rows)

      tagList(
        ## Warnings
        if (length(warnings) > 0) tagList(warnings),

        ## Plots: two-column layout
        layout_column_wrap(
          width = 1 / 2,

          ## Left: overlay distributions
          bslib::card(
            class = "mb-3",
            bslib::card_header("Distributions by Stratum"),
            bslib::card_body(
              if (n_valid >= 1) {
                plotOutput(ns("strat_dist_plot"), height = "420px")
              } else {
                div(class = "text-muted", "No valid strata for distribution plot.")
              }
            )
          ),

          ## Right: overlay scoring curves
          bslib::card(
            class = "mb-3",
            bslib::card_header("Scoring Curves by Stratum"),
            bslib::card_body(
              if (n_valid >= 2) {
                plotOutput(ns("strat_curve_plot"), height = "420px")
              } else if (n_valid == 1) {
                tagList(
                  plotOutput(ns("strat_curve_plot_single"), height = "420px"),
                  div(class = "text-muted mt-1",
                      "Only 1 stratum has a valid curve. Overlay requires 2+.")
                )
              } else {
                div(class = "text-muted", "No valid strata for scoring curve plot.")
              }
            )
          )
        ),

        ## Descriptive statistics table (strata as columns)
        bslib::card(
          class = "mb-3",
          bslib::card_header("Descriptive Statistics"),
          bslib::card_body(DT::DTOutput(ns("strat_desc_table")))
        ),

        ## Scoring thresholds table (strata as columns)
        bslib::card(
          class = "mb-3",
          bslib::card_header("Scoring Thresholds"),
          bslib::card_body(DT::DTOutput(ns("strat_threshold_table")))
        ),

        bslib::card(
          class = "mb-3",
          bslib::card_header("Manual Curve Editors"),
          bslib::card_body(
            bslib::accordion(
              id = ns("strat_curve_editors"),
              open = FALSE,
              !!!lapply(info$levels, function(lvl) {
                editor_result <- results[[lvl]]
                label <- if (identical(editor_result$curve_source %||% "auto", "manual")) {
                  paste0(lvl, " (Manual)")
                } else {
                  lvl
                }

                bslib::accordion_panel(
                  title = label,
                  mod_reference_curve_editor_ui(ns(editor_id_map[[lvl]]), title = paste0("Edit ", lvl, " Curve"))
                )
              })
            )
          )
        ),

        ## Download buttons
        div(
          class = "d-flex gap-2 mb-3",
          downloadButton(ns("dl_strat_curve_png"), "Download Overlay Curve PNG",
                         class = "btn btn-outline-secondary btn-sm"),
          downloadButton(ns("dl_strat_dist_png"), "Download Overlay Distribution PNG",
                         class = "btn btn-outline-secondary btn-sm"),
          downloadButton(ns("dl_strat_csv"), "Download Thresholds CSV",
                         class = "btn btn-outline-secondary btn-sm")
        ),

        ## Mark Complete button (single for all strata)
        div(
          class = "d-flex justify-content-end mt-3",
          actionButton(ns("mark_all_complete"), "Mark All Strata Complete \u2713",
                       class = "btn btn-success btn-proceed",
                       icon = icon("check"))
        )
      )
    })

    ## -- Overlay scoring curves plot -------------------------------------------
    output$strat_curve_plot <- renderPlot({
      req(isTRUE(phase4_workspace_active()))
      curve_rows <- all_strata_curve_rows()
      req(curve_rows)
      p <- build_overlay_curve_plot(curve_rows, rv$metric_config)
      req(p)
      p
    })

    ## -- Single-stratum fallback curve plot ------------------------------------
    output$strat_curve_plot_single <- renderPlot({
      req(isTRUE(phase4_workspace_active()))
      results <- all_strata_results()
      req(results)
      ## Find the first stratum with a valid curve_plot
      for (lvl in names(results)) {
        if (!is.null(results[[lvl]]$curve_plot)) {
          return(results[[lvl]]$curve_plot)
        }
      }
      NULL
    })

    ## -- Overlay distribution plot ---------------------------------------------
    output$strat_dist_plot <- renderPlot({
      req(isTRUE(phase4_workspace_active()))
      info <- stratum_info()
      req(info$has_strata)
      curve_rows <- all_strata_curve_rows()
      req(curve_rows)
      valid_levels <- curve_rows |>
        dplyr::filter(curve_status == "complete") |>
        dplyr::pull(stratum)
      ## Use all levels for distribution even if curve is degenerate
      all_levels <- unique(curve_rows$stratum)
      levels_to_show <- if (length(valid_levels) > 0) all_levels else info$levels
      plot_data <- rv$data
      plot_data$.summary_stratum <- strat_values()
      build_overlay_bar_chart(plot_data, rv$current_metric, rv$metric_config,
                               ".summary_stratum", levels_to_show)
    })

    ## -- Descriptive statistics table (strata as columns) ----------------------
    output$strat_desc_table <- DT::renderDT({
      req(isTRUE(phase4_workspace_active()))
      curve_rows <- all_strata_curve_rows()
      req(curve_rows)

      stats <- c("n", "Min", "Q25", "Median", "Q75", "Max", "IQR", "SD")
      tbl <- data.frame(Statistic = stats, stringsAsFactors = FALSE)

      for (i in seq_len(nrow(curve_rows))) {
        row <- curve_rows[i, ]
        lvl <- row$stratum
        if (row$curve_status == "insufficient_data") {
          tbl[[lvl]] <- rep("N/A", length(stats))
        } else {
          tbl[[lvl]] <- c(
            as.character(row$n_reference),
            round(row$min_val, 2),
            round(row$q25, 2),
            round(row$median_val, 2),
            round(row$q75, 2),
            round(row$max_val, 2),
            round(row$iqr, 2),
            round(row$sd_val, 2)
          )
        }
      }

      DT::datatable(tbl, rownames = FALSE,
                     options = list(dom = "t", paging = FALSE, ordering = FALSE),
                     class = "table table-sm table-striped")
    })

    ## -- Scoring thresholds table (strata as columns) --------------------------
    output$strat_threshold_table <- DT::renderDT({
      req(isTRUE(phase4_workspace_active()))
      curve_rows <- all_strata_curve_rows()
      req(curve_rows)

      categories <- c("Functioning", "At-Risk", "Not Functioning")
      score_ranges <- c("0.70 - 1.00", "0.30 - 0.69", "0.00 - 0.29")

      tbl <- data.frame(
        Category = categories,
        `Score Range` = score_ranges,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      for (i in seq_len(nrow(curve_rows))) {
        row <- curve_rows[i, ]
        lvl <- row$stratum
        if (row$curve_status == "insufficient_data") {
          tbl[[lvl]] <- rep("N/A", 3)
        } else {
          tbl[[lvl]] <- c(
            reference_curve_row_range_display(row, "functioning", digits = 1),
            reference_curve_row_range_display(row, "at_risk", digits = 1),
            reference_curve_row_range_display(row, "not_functioning", digits = 1)
          )
        }
      }

      DT::datatable(tbl, rownames = FALSE,
                     options = list(dom = "t", paging = FALSE, ordering = FALSE),
                     class = "table table-sm table-striped") |>
        DT::formatStyle(
          "Category",
          backgroundColor = DT::styleEqual(
            categories,
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)", "rgba(231,76,60,0.15)")
          )
        )
    })

    ## -- Mark All Complete handler ---------------------------------------------
    observeEvent(input$mark_all_complete, {
      info <- stratum_info()
      req(info$has_strata)
      metric <- rv$current_metric
      req(metric)

      results <- all_strata_results()
      req(results)
      decision_tbl <- current_phase4_decision()

      ## Save all strata to rv$stratum_results
      for (lvl in names(results)) {
        if (is.null(rv$stratum_results[[metric]])) rv$stratum_results[[metric]] <- list()
        rv$stratum_results[[metric]][[lvl]] <- list(reference_curve = results[[lvl]])
      }

      ## Mark metric complete with all strata
      phase4_signature <- cache_metric_phase4_results(
        rv,
        metric,
        decision_tbl = decision_tbl,
        stratum_results = rv$stratum_results[[metric]],
        artifact_mode = "full"
      )
      curve_rows <- extract_metric_phase4_curve_rows(list(
        stratum_results = rv$stratum_results[[metric]]
      ))
      update_metric_phase4_completed_entry(rv, metric, list(
        stratified = TRUE,
        strat_var = info$strat_var,
        strat_decision = decision_tbl,
        stratum_results = rv$stratum_results[[metric]],
        phase4_signature = phase4_signature,
        phase4_artifact_mode = "full",
        phase4_curve_rows = curve_rows
      ))

      showNotification(
        paste0(rv$metric_config[[metric]]$display_name,
               " \u2014 all ", length(info$levels), " strata marked complete!"),
        type = "message", duration = 5
      )
      notify_workspace_refresh(rv)
    })

    ## -- Download: overlay curve PNG -------------------------------------------
    output$dl_strat_curve_png <- downloadHandler(
      filename = function() {
        paste0(rv$current_metric, "_overlay_scoring_curves.png")
      },
      content = function(file) {
        curve_rows <- all_strata_curve_rows()
        req(curve_rows)
        p <- build_overlay_curve_plot(curve_rows, rv$metric_config)
        if (!is.null(p)) {
          ggplot2::ggsave(file, p, width = 10, height = 6, dpi = 300)
        }
      }
    )

    ## -- Download: overlay distribution PNG ------------------------------------
    output$dl_strat_dist_png <- downloadHandler(
      filename = function() {
        paste0(rv$current_metric, "_overlay_distributions.png")
      },
      content = function(file) {
        info <- stratum_info()
        req(info$has_strata)
        plot_data <- rv$data
        plot_data$.summary_stratum <- strat_values()
        p <- build_overlay_bar_chart(plot_data, rv$current_metric, rv$metric_config,
                                      ".summary_stratum", info$levels)
        if (!is.null(p)) {
          ggplot2::ggsave(file, p, width = 10, height = 6, dpi = 300)
        }
      }
    )

    ## -- Download: comparison CSV ----------------------------------------------
    output$dl_strat_csv <- downloadHandler(
      filename = function() {
        paste0(rv$current_metric, "_strata_thresholds.csv")
      },
      content = function(file) {
        curve_rows <- all_strata_curve_rows()
        req(curve_rows)
        readr::write_csv(reference_curve_rows_for_export(curve_rows), file)
      }
    )
  })
}
