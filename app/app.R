## Reference & Regional Curve Development - Interactive Shiny App
## Entry point. Sources global.R, defines UI + server.
## 4-phase workflow: Explore -> Compare -> Verify -> Finalize

source("global.R", local = TRUE)

ui <- page_navbar(
  id = "main_navbar",
  title = "StreamCurves",
  window_title = "StreamCurves - Reference & Regional Curve Development",
  theme = app_theme,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$script(src = "custom.js")
  ),
  fillable = FALSE,

  nav_panel(
    title = "Data & Setup",
    icon = bsicons::bs_icon("database"),
    mod_data_overview_ui("data_overview")
  ),

  nav_panel(
    title = "Reference Curves",
    icon = bsicons::bs_icon("table"),
    mod_summary_page_ui("summary")
  ),

  nav_panel(
    title = "Regional Curves",
    icon = bsicons::bs_icon("bezier2"),
    mod_regional_curve_ui("regional")
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    data = NULL,
    qa_log = NULL,
    precheck_df = NULL,

    data_source = NULL,
    data_fingerprint = NULL,
    upload_filename = NULL,

    metric_config = metric_config,
    strat_config = strat_config,
    predictor_config = predictor_config,
    factor_recode_config = factor_recode_config,
    output_config = output_config,
    startup_metric_config = unserialize(serialize(metric_config, NULL)),
    startup_strat_config = unserialize(serialize(strat_config, NULL)),
    startup_predictor_config = unserialize(serialize(predictor_config, NULL)),
    startup_factor_recode_config = unserialize(serialize(factor_recode_config, NULL)),
    startup_output_config = unserialize(serialize(output_config, NULL)),
    startup_config_version = 0L,
    startup_current_metric = "perRiffle",
    config_version = 0L,

    current_metric = "perRiffle",

    phase1_screening = NULL,
    phase1_effect_sizes = NULL,
    phase1_candidates = list(),
    all_layer1_results = list(),
    all_layer2_results = list(),

    phase2_ranking = NULL,
    cross_metric_consistency = NULL,
    phase2_settings = empty_phase2_settings(),
    phase2_metric_overrides = list(),
    summary_available_overrides = list(),
    summary_edit_notes = list(),

    phase3_patterns = NULL,
    phase3_feasibility = NULL,
    phase3_verification = list(),
    strat_decision_user = NULL,

    reference_curve = NULL,

    phase4_data = NULL,
    current_stratum_level = NULL,
    stratum_results = list(),

    completed_metrics = list(),
    decision_log = tibble::tibble(),

    metric_phase_cache = list(),

    custom_groupings = list(),
    custom_grouping_counter = list(),

    session_name = NULL,
    app_data_loaded = FALSE,
    app_reset_nonce = 0L,
    workspace_modal_type = NULL,
    workspace_modal_metric = NULL,
    workspace_modal_stage = NULL,
    workspace_modal_error = NULL,
    workspace_modal_loading_detail = NULL,
    workspace_modal_nonce = 0L,
    workspace_modal_ready_nonce = 0L,
    workspace_refresh_nonce = 0L
  )

  mod_data_overview_server("data_overview", rv)
  mod_summary_page_server("summary", rv)
  mod_phase1_exploration_server("phase1", rv, dialog_mode = TRUE)
  mod_phase2_consistency_server("phase2", rv)
  mod_phase3_verification_server("phase3", rv, parent_session = session, dialog_mode = TRUE)
  mod_phase4_finalization_server("phase4", rv, dialog_mode = TRUE)
  mod_summary_export_server("summary_export", rv)
  mod_regional_curve_server("regional", rv)

  modal_request_is_current <- function(request_id) {
    identical(shiny::isolate(rv$workspace_modal_nonce %||% 0L), request_id)
  }

  workspace_modal_metric_label <- function(metric) {
    if (is.null(metric) || identical(metric, "")) {
      return(NULL)
    }
    rv$metric_config[[metric]]$display_name %||% metric
  }

  workspace_modal_title <- function(modal_type, metric) {
    metric_label <- workspace_modal_metric_label(metric)

    switch(
      modal_type,
      phase1 = paste0("Phase 1: Explore", if (!is.null(metric_label)) paste0(" - ", metric_label) else ""),
      phase2 = "Phase 2: Compare",
      phase3 = paste0("Phase 3: Verify", if (!is.null(metric_label)) paste0(" - ", metric_label) else ""),
      phase4 = paste0("Phase 4: Finalize", if (!is.null(metric_label)) paste0(" - ", metric_label) else ""),
      summary_export = "Summary / Export",
      "Workspace"
    )
  }

  workspace_modal_loading_label <- function(modal_type) {
    switch(
      modal_type,
      phase1 = "Loading Phase 1 workspace...",
      phase2 = "Loading Phase 2 workspace...",
      phase3 = "Loading Phase 3 workspace...",
      phase4 = "Loading Phase 4 workspace...",
      summary_export = "Loading Summary / Export workspace...",
      "Loading workspace..."
    )
  }

  workspace_modal_loading_default_detail <- function(modal_type, metric) {
    metric_label <- workspace_modal_metric_label(metric)

    switch(
      modal_type,
      phase1 = paste0("Preparing screening results", if (!is.null(metric_label)) paste0(" for ", metric_label) else "", "."),
      phase2 = "Preparing cross-metric consistency results.",
      phase3 = paste0("Preparing verification results", if (!is.null(metric_label)) paste0(" for ", metric_label) else "", "."),
      phase4 = paste0("Preparing reference curve results", if (!is.null(metric_label)) paste0(" for ", metric_label) else "", "."),
      summary_export = "Preparing summary and export outputs.",
      "Preparing workspace."
    )
  }

  workspace_modal_progress_steps <- function(modal_type, metric) {
    if (is.null(metric) && modal_type %in% c("phase1", "phase3", "phase4")) {
      return(0L)
    }

    switch(
      modal_type,
      phase1 = if (metric_needs_phase1_artifact_refresh(rv, metric)) {
        count_metric_phase1_backfill_steps(rv, metric, mode = "full")
      } else {
        0L
      },
      phase3 = {
        steps <- 0L
        if (metric_needs_phase1_artifact_refresh(rv, metric)) {
          steps <- steps + count_metric_phase1_backfill_steps(rv, metric, mode = "full")
        }
        if (metric_needs_phase3_artifact_refresh(rv, metric)) {
          steps <- steps + count_metric_phase3_backfill_steps(rv, metric, mode = "full")
        }
        steps
      },
      phase4 = count_metric_phase4_preload_steps(rv, metric),
      0L
    )
  }

  make_workspace_progress_notifier <- function(total_steps, message, detail = NULL) {
    progress <- shiny::Progress$new(session, min = 0, max = total_steps)
    current_step <- 0L
    progress$set(message = message, detail = detail, value = current_step)

    list(
      set_detail = function(next_detail) {
        rv$workspace_modal_loading_detail <- next_detail
        progress$set(message = message, detail = next_detail, value = current_step)
        invisible(NULL)
      },
      advance = function(next_detail = rv$workspace_modal_loading_detail %||% NULL) {
        current_step <<- min(current_step + 1L, total_steps)
        rv$workspace_modal_loading_detail <- next_detail
        progress$set(message = message, detail = next_detail, value = current_step)
        invisible(NULL)
      },
      close = function() {
        progress$close()
        invisible(NULL)
      }
    )
  }

  workspace_modal_min_shell_seconds <- function(modal_type, total_steps) {
    if (identical(modal_type, "phase2") && identical(as.integer(total_steps %||% 0L), 0L)) {
      return(0.2)
    }

    0
  }

  set_workspace_modal_ready <- function(request_id) {
    shiny::isolate({
      if (!modal_request_is_current(request_id)) {
        return(invisible(NULL))
      }

      rv$workspace_modal_ready_nonce <- isolate(rv$workspace_modal_ready_nonce %||% 0L) + 1L
      rv$workspace_modal_stage <- "ready"
      rv$workspace_modal_error <- NULL
    })

    invisible(NULL)
  }

  schedule_workspace_modal_ready <- function(request_id, modal_type, total_steps) {
    delay_seconds <- workspace_modal_min_shell_seconds(modal_type, total_steps)

    if (isTRUE(delay_seconds > 0)) {
      later::later(function() {
        set_workspace_modal_ready(request_id)
        invisible(NULL)
      }, delay_seconds)
      return(invisible(NULL))
    }

    set_workspace_modal_ready(request_id)
    invisible(NULL)
  }

  prepare_workspace_modal <- function(request_id, modal_type, metric) {
    loading_message <- workspace_modal_loading_label(modal_type)

    tryCatch(
      {
        target_metric <- shiny::isolate({
          rv$workspace_modal_stage <- "loading"
          rv$workspace_modal_error <- NULL
          rv$workspace_modal_loading_detail <- workspace_modal_loading_default_detail(modal_type, metric)

          metric %||% rv$current_metric %||% NULL
        })

        shiny::isolate({
          old_metric <- rv$current_metric %||% NULL

          if (!is.null(target_metric) && !identical(target_metric, old_metric)) {
            if (!is.null(old_metric) && nzchar(old_metric)) {
              save_metric_phase_state(rv, old_metric)
            }
            rv$current_metric <- target_metric
            restore_metric_phase_state(rv, target_metric)
          }

          rv$workspace_modal_metric <- target_metric
        })

        total_steps <- shiny::isolate(workspace_modal_progress_steps(modal_type, target_metric))
        progress <- NULL
        if (isTRUE(total_steps > 0L)) {
          progress <- make_workspace_progress_notifier(
            total_steps = total_steps,
            message = loading_message,
            detail = shiny::isolate(rv$workspace_modal_loading_detail %||% NULL)
          )
        }
        on.exit(if (!is.null(progress)) progress$close(), add = TRUE)

        shiny::isolate({
          if (!modal_request_is_current(request_id)) {
            return(invisible(NULL))
          }

          switch(
            modal_type,
            phase1 = {
              if (!is.null(target_metric) && metric_needs_phase1_artifact_refresh(rv, target_metric)) {
                ensure_metric_phase1_artifacts(rv, target_metric, progress = progress)
              }
            },
            phase3 = {
              if (!is.null(target_metric) && metric_needs_phase1_artifact_refresh(rv, target_metric)) {
                ensure_metric_phase1_artifacts(rv, target_metric, progress = progress)
              }
              if (!is.null(target_metric) && metric_needs_phase3_artifact_refresh(rv, target_metric)) {
                ensure_metric_phase3_artifacts(rv, target_metric, progress = progress)
              }
            },
            phase4 = {
              if (!is.null(target_metric)) {
                preload_metric_phase4_workspace(rv, target_metric, progress = progress)
              }
            },
            phase2 = NULL,
            summary_export = NULL,
            NULL
          )

          rv$workspace_modal_loading_detail <- "Rendering workspace..."

          if (!modal_request_is_current(request_id)) {
            return(invisible(NULL))
          }

          schedule_workspace_modal_ready(request_id, modal_type, total_steps)
        })
      },
      error = function(e) {
        shiny::isolate({
          if (!modal_request_is_current(request_id)) {
            return(invisible(NULL))
          }

          rv$workspace_modal_error <- conditionMessage(e)
          rv$workspace_modal_loading_detail <- NULL
          rv$workspace_modal_stage <- "error"
        })
      }
    )
  }

  output$workspace_modal_body <- renderUI({
    modal_type <- rv$workspace_modal_type %||% NULL
    req(!is.null(modal_type))

    stage <- rv$workspace_modal_stage %||% "loading"
    metric <- rv$workspace_modal_metric %||% rv$current_metric %||% NULL

    if (identical(stage, "error")) {
      return(div(
        class = "workspace-modal-error",
        div(
          class = "alert alert-danger mb-0",
          tags$strong("Could not open this workspace."),
          tags$br(),
          rv$workspace_modal_error %||% "Unknown error.",
          div(
            class = "mt-3",
            actionButton("workspace_modal_retry", "Retry", class = "btn btn-danger btn-sm")
          )
        )
      ))
    }

    if (!identical(stage, "ready")) {
      return(div(
        class = "workspace-modal-loading-shell",
        div(
          class = "progress workspace-modal-loading-progress",
          div(
            class = "progress-bar progress-bar-striped progress-bar-animated w-100 d-flex align-items-center justify-content-start",
            icon("spinner", class = "fa-spin me-2"),
            tags$span(workspace_modal_loading_label(modal_type))
          )
        ),
        div(
          class = "workspace-modal-loading-detail",
          rv$workspace_modal_loading_detail %||% workspace_modal_loading_default_detail(modal_type, metric)
        )
      ))
    }

    switch(
      modal_type,
      phase1 = mod_phase1_exploration_ui("phase1", dialog_mode = TRUE),
      phase2 = mod_phase2_consistency_ui("phase2"),
      phase3 = mod_phase3_verification_ui("phase3", dialog_mode = TRUE),
      phase4 = mod_phase4_finalization_ui("phase4", dialog_mode = TRUE),
      summary_export = mod_summary_export_ui("summary_export"),
      div(class = "alert alert-warning", "Unknown workspace requested.")
    )
  })

  observeEvent(input$workspace_modal_retry, {
    req(rv$workspace_modal_type)
    launch_workspace_modal(rv, rv$workspace_modal_type, rv$workspace_modal_metric %||% NULL)
  }, ignoreInit = TRUE)

  observeEvent(rv$workspace_modal_nonce, {
    modal_type <- rv$workspace_modal_type %||% NULL
    req(!is.null(modal_type))

    metric <- rv$workspace_modal_metric %||% rv$current_metric
    request_id <- isolate(rv$workspace_modal_nonce %||% 0L)
    title <- workspace_modal_title(modal_type, metric)

    removeModal()
    showModal(
      modalDialog(
        title = title,
        uiOutput("workspace_modal_body"),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "xl",
        class = "workspace-modal-dialog"
      )
    )

    session$onFlushed(function() {
      if (!modal_request_is_current(request_id)) {
        return(invisible(NULL))
      }
      prepare_workspace_modal(request_id, modal_type, metric)
      invisible(NULL)
    }, once = TRUE)
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
