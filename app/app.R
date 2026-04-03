## Reference & Regional Curve Development - Interactive Shiny App
## Entry point. Sources global.R, defines UI + server.
## 4-phase workflow: Explore -> Compare -> Verify -> Finalize

source("global.R", local = TRUE)

versioned_www_asset <- function(asset_name) {
  asset_path <- file.path("www", asset_name)
  info <- file.info(asset_path)

  if (is.na(info$mtime)) {
    return(asset_name)
  }

  asset_size <- if (is.na(info$size)) 0 else as.integer(info$size)
  version <- paste0(
    format(info$mtime, "%Y%m%d%H%M%S"),
    "-",
    asset_size
  )

  paste0(asset_name, "?v=", version)
}

ui <- page_navbar(
  id = "main_navbar",
  title = "StreamCurves",
  window_title = "StreamCurves - Reference & Regional Curve Development",
  theme = app_theme,
  header = tags$head(
    tags$link(rel = "stylesheet", href = versioned_www_asset("custom.css")),
    tags$script(src = versioned_www_asset("custom.js"))
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
    input_metadata = NULL,
    site_mask_config = NULL,

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
    curve_stratification = list(),
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
    workspace_refresh_nonce = 0L,
    analysis_tab_request_id = NULL,
    analysis_tab_status = empty_analysis_tab_status("pending"),
    analysis_tab_status_nonce = 0L,
    analysis_tab_preload_tab = NULL,
    analysis_tab_preload_nonce = 0L,
    analysis_tab_preload_completed_tab = NULL,
    analysis_tab_preload_completed_status = NULL,
    analysis_tab_preload_completed_nonce = 0L
  )

  mod_data_overview_server("data_overview", rv)
  mod_summary_page_server("summary", rv)
  mod_phase1_exploration_server("phase1", rv, dialog_mode = TRUE, workspace_scope = "standalone")
  mod_phase2_consistency_server("phase2", rv, workspace_scope = "standalone")
  mod_phase3_verification_server("phase3", rv, parent_session = session, dialog_mode = TRUE, workspace_scope = "standalone")
  mod_phase4_finalization_server("phase4", rv, dialog_mode = TRUE, workspace_scope = "standalone")
  mod_summary_export_server("summary_export", rv)
  mod_regional_curve_server("regional", rv)

  analysis_workspace_server_initialized <- FALSE

  ensure_analysis_workspace_server <- function() {
    if (isTRUE(analysis_workspace_server_initialized)) {
      return(invisible(NULL))
    }

    mod_analysis_workspace_server("analysis", rv)
    analysis_workspace_server_initialized <<- TRUE
    invisible(NULL)
  }

  modal_request_is_current <- function(request_id) {
    identical(shiny::isolate(rv$workspace_modal_nonce %||% 0L), request_id)
  }

  workspace_modal_metric_label <- function(metric) {
    if (is.null(metric) || identical(metric, "")) {
      return(NULL)
    }

    metric_config_snapshot <- shiny::isolate(rv$metric_config %||% list())
    metric_entry <- metric_config_snapshot[[metric]] %||% list()
    metric_entry$display_name %||% metric
  }

  workspace_modal_title <- function(modal_type, metric) {
    metric_label <- workspace_modal_metric_label(metric)

    switch(
      modal_type,
      analysis = paste0("Analysis", if (!is.null(metric_label)) paste0(" - ", metric_label) else ""),
      phase1 = paste0("Phase 1: Explore", if (!is.null(metric_label)) paste0(" - ", metric_label) else ""),
      phase2 = "Phase 2: Compare",
      phase3 = paste0("Phase 3: Verify", if (!is.null(metric_label)) paste0(" - ", metric_label) else ""),
      phase4 = paste0("Phase 4: Finalize", if (!is.null(metric_label)) paste0(" - ", metric_label) else ""),
      summary_export = "Export",
      "Workspace"
    )
  }

  workspace_modal_loading_label <- function(modal_type) {
    switch(
      modal_type,
      analysis = "Loading analysis workspace...",
      phase1 = "Loading Phase 1 workspace...",
      phase2 = "Loading Phase 2 workspace...",
      phase3 = "Loading Phase 3 workspace...",
      phase4 = "Loading Phase 4 workspace...",
      summary_export = "Loading Export workspace...",
      "Loading workspace..."
    )
  }

  workspace_modal_loading_default_detail <- function(modal_type, metric) {
    metric_label <- workspace_modal_metric_label(metric)

    switch(
      modal_type,
      analysis = paste0("Preparing analysis workspace", if (!is.null(metric_label)) paste0(" for ", metric_label) else "", "."),
      phase1 = paste0("Preparing screening results", if (!is.null(metric_label)) paste0(" for ", metric_label) else "", "."),
      phase2 = "Preparing cross-metric consistency results.",
      phase3 = paste0("Preparing verification results", if (!is.null(metric_label)) paste0(" for ", metric_label) else "", "."),
      phase4 = paste0("Preparing reference curve results", if (!is.null(metric_label)) paste0(" for ", metric_label) else "", "."),
      summary_export = "Preparing export outputs.",
      "Preparing workspace."
    )
  }

  workspace_modal_progress_steps <- function(modal_type, metric) {
    if (is.null(metric) && modal_type %in% c("analysis", "phase1", "phase3", "phase4")) {
      return(0L)
    }

    switch(
      modal_type,
      analysis = 0L,
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

  analysis_launch_progress <- NULL

  analysis_launch_progress_message <- function(metric = NULL) {
    metric_label <- workspace_modal_metric_label(metric)
    paste0("Opening Analysis", if (!is.null(metric_label)) paste0(" - ", metric_label) else "")
  }

  analysis_launch_total_steps <- function(metric = NULL) {
    target_metric <- metric %||% shiny::isolate(rv$workspace_modal_metric %||% rv$current_metric %||% NULL)
    if (is.null(target_metric) || identical(target_metric, "")) {
      return(1L)
    }

    exploratory_steps <- if (metric_needs_phase1_artifact_refresh(rv, target_metric)) {
      count_metric_phase1_backfill_steps(rv, target_metric, mode = "full")
    } else {
      1L
    }

    verification_steps <- if (metric_needs_phase3_artifact_refresh(rv, target_metric)) {
      count_metric_phase3_backfill_steps(rv, target_metric, mode = "full")
    } else {
      1L
    }

    phase4_steps <- count_metric_phase4_preload_steps(rv, target_metric)
    if (!isTRUE(phase4_steps > 0L)) {
      phase4_steps <- 1L
    }

    as.integer(1L + exploratory_steps + verification_steps + phase4_steps + 1L)
  }

  analysis_launch_progress_value <- function(request_id = NULL) {
    current <- analysis_launch_progress
    if (is.null(current)) {
      return(0L)
    }

    request_id <- request_id %||% current$request_id %||% NULL
    if (!identical(current$request_id %||% NULL, request_id)) {
      return(0L)
    }

    as.integer(current$completed_steps %||% 0L)
  }

  analysis_launch_progress_detail <- function(request_id = NULL, detail = NULL, metric = NULL) {
    target_metric <- metric %||% shiny::isolate(rv$workspace_modal_metric %||% rv$current_metric %||% NULL)
    completed_steps <- analysis_launch_progress_value(request_id)
    current <- analysis_launch_progress
    total_steps <- if (!is.null(current) &&
                       identical(current$request_id %||% NULL, request_id %||% current$request_id %||% NULL)) {
      as.integer(current$total_steps %||% analysis_launch_total_steps(target_metric))
    } else {
      analysis_launch_total_steps(target_metric)
    }
    detail_text <- detail %||%
      shiny::isolate(rv$workspace_modal_loading_detail %||%
                       workspace_modal_loading_default_detail("analysis", target_metric))

    paste0(
      "Loading page, please wait. ",
      detail_text,
      " ",
      completed_steps,
      " of ",
      total_steps,
      " launch steps complete."
    )
  }

  close_analysis_launch_progress <- function(request_id = NULL, force = FALSE) {
    current <- analysis_launch_progress
    if (is.null(current)) {
      return(invisible(FALSE))
    }

    if (!isTRUE(force) &&
        !is.null(request_id) &&
        !identical(current$request_id %||% NULL, request_id)) {
      return(invisible(FALSE))
    }

    if (!is.null(current$progress)) {
      try(current$progress$close(), silent = TRUE)
    }
    if (!is.null(current$final_notification_id %||% NULL)) {
      remove_final_loading_notification(session, current$final_notification_id)
    }
    analysis_launch_progress <<- NULL
    invisible(TRUE)
  }

  show_analysis_launch_progress <- function(request_id, metric = NULL) {
    remove_analysis_launch_spinner_notification(session, request_id)
    close_analysis_launch_progress(force = TRUE)

    total_steps <- analysis_launch_total_steps(metric)
    progress <- shiny::Progress$new(session, min = 0, max = total_steps)

    analysis_launch_progress <<- list(
      request_id = request_id,
      progress = progress,
      metric = metric,
      total_steps = total_steps,
      completed_steps = 0L,
      detail = workspace_modal_loading_default_detail("analysis", metric),
      final_notification_id = NULL
    )

    update_analysis_launch_progress(request_id, metric = metric)
    invisible(NULL)
  }

  update_analysis_launch_progress <- function(request_id = NULL, metric = NULL, detail = NULL) {
    current <- analysis_launch_progress
    if (is.null(current)) {
      return(invisible(FALSE))
    }

    request_id <- request_id %||% current$request_id %||% NULL
    if (!identical(current$request_id %||% NULL, request_id)) {
      return(invisible(FALSE))
    }

    current$metric <- metric %||% current$metric %||% NULL
    if (!is.null(detail)) {
      current$detail <- detail
    }

    completed_steps <- as.integer(current$completed_steps %||% 0L)
    total_steps <- as.integer(current$total_steps %||% analysis_launch_total_steps(current$metric))

    if (!is.null(current$progress)) {
      try(
        current$progress$set(
          message = analysis_launch_progress_message(current$metric),
          detail = analysis_launch_progress_detail(
            request_id,
            detail = current$detail,
            metric = current$metric
          ),
          value = min(completed_steps, total_steps)
        ),
        silent = TRUE
      )
    } else if (!is.null(current$final_notification_id %||% NULL)) {
      show_final_loading_notification(
        session,
        current$final_notification_id,
        analysis_launch_progress_message(current$metric),
        analysis_launch_progress_detail(
          request_id,
          detail = current$detail,
          metric = current$metric
        )
      )
    }

    analysis_launch_progress <<- current
    invisible(TRUE)
  }

  advance_analysis_launch_progress <- function(request_id = NULL, metric = NULL, detail = NULL, steps = 1L) {
    current <- analysis_launch_progress
    if (is.null(current)) {
      return(invisible(FALSE))
    }

    request_id <- request_id %||% current$request_id %||% NULL
    if (!identical(current$request_id %||% NULL, request_id)) {
      return(invisible(FALSE))
    }

    current$metric <- metric %||% current$metric %||% NULL
    if (!is.null(detail)) {
      current$detail <- detail
    }

    step_count <- suppressWarnings(as.integer(steps))
    if (is.na(step_count) || step_count < 0L) {
      step_count <- 0L
    }

    current$total_steps <- as.integer(current$total_steps %||% analysis_launch_total_steps(current$metric))
    current$completed_steps <- min(
      as.integer(current$completed_steps %||% 0L) + step_count,
      current$total_steps
    )

    analysis_launch_progress <<- current
    update_analysis_launch_progress(
      request_id = request_id,
      metric = current$metric,
      detail = current$detail
    )
  }

  make_analysis_launch_progress_adapter <- function(request_id, metric = NULL) {
    list(
      set_detail = function(next_detail) {
        rv$workspace_modal_loading_detail <- next_detail
        update_analysis_launch_progress(
          request_id = request_id,
          metric = metric,
          detail = next_detail
        )
        invisible(NULL)
      },
      advance = function(next_detail = shiny::isolate(rv$workspace_modal_loading_detail %||% NULL)) {
        if (!is.null(next_detail)) {
          rv$workspace_modal_loading_detail <- next_detail
        }
        advance_analysis_launch_progress(
          request_id = request_id,
          metric = metric,
          detail = next_detail
        )
        invisible(NULL)
      },
      close = function() {
        invisible(NULL)
      }
    )
  }

  show_analysis_launch_final_loading <- function(request_id = NULL, metric = NULL, detail = NULL) {
    current <- analysis_launch_progress
    if (is.null(current)) {
      return(invisible(FALSE))
    }

    request_id <- request_id %||% current$request_id %||% NULL
    if (!identical(current$request_id %||% NULL, request_id)) {
      return(invisible(FALSE))
    }

    current$metric <- metric %||% current$metric %||% NULL
    if (!is.null(detail)) {
      current$detail <- detail
    }

    if (!is.null(current$progress)) {
      try(current$progress$close(), silent = TRUE)
      current$progress <- NULL
    }

    current$final_notification_id <- current$final_notification_id %||%
      paste0("analysis-launch-final-", request_id)

    show_final_loading_notification(
      session,
      current$final_notification_id,
      analysis_launch_progress_message(current$metric),
      analysis_launch_progress_detail(
        request_id,
        detail = current$detail,
        metric = current$metric
      )
    )

    analysis_launch_progress <<- current
    invisible(TRUE)
  }

  set_workspace_modal_loading_detail <- function(detail, request_id = NULL, metric = NULL) {
    rv$workspace_modal_loading_detail <- detail

    if (identical(shiny::isolate(rv$workspace_modal_type %||% NULL), "analysis")) {
      if (identical(detail, "Rendering workspace...")) {
        advance_analysis_launch_progress(request_id, metric = metric, detail = detail)
        show_analysis_launch_final_loading(request_id, metric = metric, detail = detail)
      } else {
        update_analysis_launch_progress(request_id, metric = metric, detail = detail)
      }
    }

    invisible(NULL)
  }

  workspace_modal_min_shell_seconds <- function(modal_type, total_steps) {
    if (identical(modal_type, "phase2") && identical(as.integer(total_steps %||% 0L), 0L)) {
      return(0.2)
    }

    0
  }

  analysis_modal_progress_value <- function(request_id = NULL) {
    request_id <- request_id %||% shiny::isolate(rv$analysis_tab_request_id %||% NULL)
    if (!analysis_tab_request_is_current(rv, request_id)) {
      return(0L)
    }

    status <- shiny::isolate(rv$analysis_tab_status %||% empty_analysis_tab_status("pending"))
    as.integer(sum(vapply(status, identical, logical(1), "ready")))
  }

  set_analysis_modal_tab_status <- function(request_id, tab_key, status, detail = NULL) {
    shiny::isolate({
      if (!modal_request_is_current(request_id) || !analysis_tab_request_is_current(rv, request_id)) {
        return(invisible(FALSE))
      }

      set_analysis_tab_status(rv, tab_key, status, request_id)
      if (!is.null(detail)) {
        set_workspace_modal_loading_detail(detail, request_id, rv$workspace_modal_metric %||% rv$current_metric %||% NULL)
      } else {
        update_analysis_launch_progress(request_id)
      }

      invisible(TRUE)
    })
  }

  preload_analysis_modal_tab <- function(request_id, tab_key, detail, action) {
    tab_label <- analysis_tab_labels()[[tab_key]] %||% tab_key
    set_analysis_modal_tab_status(request_id, tab_key, "loading", detail)

    ready_detail <- tryCatch(
      action(),
      error = function(e) {
        set_analysis_modal_tab_status(
          request_id,
          tab_key,
          "error",
          paste0(tab_label, " failed to load.")
        )
        stop(paste0(tab_label, " failed to load: ", conditionMessage(e)), call. = FALSE)
      }
    )

    if (!isTRUE(modal_request_is_current(request_id)) ||
        !isTRUE(analysis_tab_request_is_current(rv, request_id))) {
      return(invisible(FALSE))
    }

    set_analysis_modal_tab_status(
      request_id,
      tab_key,
      "ready",
      ready_detail %||% paste0(tab_label, " ready.")
    )

    invisible(TRUE)
  }

  mark_workspace_modal_ready <- function(request_id) {
    shiny::isolate({
      if (!modal_request_is_current(request_id)) {
        return(invisible(NULL))
      }

      rv$workspace_modal_stage <- "ready"
      rv$workspace_modal_error <- NULL
    })

    invisible(NULL)
  }

  notify_workspace_modal_ready <- function(request_id) {
    shiny::isolate({
      if (!modal_request_is_current(request_id)) {
        return(invisible(NULL))
      }

      rv$workspace_modal_ready_nonce <- isolate(rv$workspace_modal_ready_nonce %||% 0L) + 1L
    })

    invisible(NULL)
  }

  set_workspace_modal_ready <- function(request_id, modal_type, notify = TRUE) {
    mark_workspace_modal_ready(request_id)
    if (isTRUE(notify)) {
      notify_workspace_modal_ready(request_id)
    }
    invisible(NULL)
  }

  schedule_workspace_modal_ready <- function(request_id, modal_type, total_steps, notify = TRUE) {
    delay_seconds <- workspace_modal_min_shell_seconds(modal_type, total_steps)

    if (isTRUE(delay_seconds > 0)) {
      later::later(function() {
        set_workspace_modal_ready(request_id, modal_type, notify = notify)
        invisible(NULL)
      }, delay_seconds)
      return(invisible(NULL))
    }

    set_workspace_modal_ready(request_id, modal_type, notify = notify)
    invisible(NULL)
  }

  show_workspace_modal_dialog <- function(title, body_content = uiOutput("workspace_modal_body")) {
    removeModal()
    session$sendCustomMessage("clearModalBackdrop", list())
    showModal(
      modalDialog(
        title = title,
        body_content,
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "xl",
        class = "workspace-modal-dialog"
      )
    )

    invisible(NULL)
  }

  prepare_workspace_modal <- function(request_id, modal_type, metric) {
    loading_message <- workspace_modal_loading_label(modal_type)

    tryCatch(
      {
        target_metric <- shiny::isolate({
          rv$workspace_modal_stage <- "loading"
          rv$workspace_modal_error <- NULL

          metric %||% rv$current_metric %||% NULL
        })

        set_workspace_modal_loading_detail(
          workspace_modal_loading_default_detail(modal_type, target_metric),
          request_id,
          target_metric
        )

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
        analysis_progress <- if (identical(modal_type, "analysis")) {
          make_analysis_launch_progress_adapter(request_id, target_metric)
        } else {
          NULL
        }

        shiny::isolate({
          if (!modal_request_is_current(request_id)) {
            return(invisible(NULL))
          }

          switch(
            modal_type,
            analysis = NULL,
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

          set_workspace_modal_loading_detail("Rendering workspace...", request_id, target_metric)

          if (!modal_request_is_current(request_id)) {
            return(invisible(NULL))
          }

          schedule_workspace_modal_ready(
            request_id,
            modal_type,
            total_steps,
            notify = !identical(modal_type, "analysis")
          )
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

  analysis_loading_status_ui <- function() {
    labels <- analysis_tab_labels()
    status <- rv$analysis_tab_status %||% empty_analysis_tab_status("pending")

    tagList(lapply(names(labels), function(tab_key) {
      tab_status <- status[[tab_key]] %||% "pending"
      status_icon <- switch(
        tab_status,
        ready = icon("check-circle"),
        loading = icon("spinner", class = "fa-spin"),
        error = icon("exclamation-circle"),
        icon("clock")
      )
      status_text <- switch(
        tab_status,
        ready = "Ready",
        loading = "Loading",
        error = "Error",
        "Waiting"
      )

      div(
        class = paste("workspace-analysis-loading-row", paste0("is-", tab_status)),
        div(class = "workspace-analysis-loading-row-label", labels[[tab_key]]),
        div(
          class = "workspace-analysis-loading-row-state",
          status_icon,
          tags$span(status_text)
        )
      )
    }))
  }

  workspace_modal_body_content <- function(modal_type, stage, metric) {
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
      if (identical(modal_type, "analysis")) {
        labels <- analysis_tab_labels()
        total_tabs <- length(labels)
        ready_tabs <- analysis_modal_progress_value()
        progress_pct <- if (total_tabs > 0) round(100 * ready_tabs / total_tabs) else 0

        return(div(
          class = "workspace-modal-loading-shell workspace-analysis-loading-shell",
          div(
            class = "workspace-modal-loading-header",
            icon("spinner", class = "fa-spin"),
            tags$span(workspace_modal_loading_label(modal_type))
          ),
          div(
            class = "workspace-modal-loading-meta",
            paste0(ready_tabs, " of ", total_tabs, " analysis tabs ready")
          ),
          div(
            class = "progress workspace-modal-loading-progress",
            div(
              class = "progress-bar progress-bar-striped progress-bar-animated workspace-modal-loading-progress-bar",
              role = "progressbar",
              style = paste0("width: ", progress_pct, "%;"),
              `aria-valuemin` = 0,
              `aria-valuemax` = total_tabs,
              `aria-valuenow` = ready_tabs
            )
          ),
          div(
            class = "workspace-modal-loading-detail",
            rv$workspace_modal_loading_detail %||% workspace_modal_loading_default_detail(modal_type, metric)
          ),
          div(
            class = "workspace-analysis-loading-status",
            analysis_loading_status_ui()
          )
        ))
      }

      return(div(
        class = "workspace-modal-loading-shell",
        div(
          class = "workspace-modal-loading-header",
          icon("spinner", class = "fa-spin"),
          tags$span(workspace_modal_loading_label(modal_type))
        ),
        div(
          class = "progress workspace-modal-loading-progress",
          div(
            class = "progress-bar progress-bar-striped progress-bar-animated w-100 d-flex align-items-center justify-content-start"
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
      analysis = mod_analysis_workspace_ui("analysis"),
      phase1 = mod_phase1_exploration_ui("phase1", dialog_mode = TRUE),
      phase2 = mod_phase2_consistency_ui("phase2"),
      phase3 = mod_phase3_verification_ui("phase3", dialog_mode = TRUE),
      phase4 = mod_phase4_finalization_ui("phase4", dialog_mode = TRUE),
      summary_export = mod_summary_export_ui("summary_export"),
      div(class = "alert alert-warning", "Unknown workspace requested.")
    )
  }

  output$workspace_modal_body <- renderUI({
    modal_type <- rv$workspace_modal_type %||% NULL
    req(!is.null(modal_type))

    stage <- rv$workspace_modal_stage %||% "loading"
    metric <- rv$workspace_modal_metric %||% rv$current_metric %||% NULL

    workspace_modal_body_content(modal_type, stage, metric)
  })
  outputOptions(output, "workspace_modal_body", suspendWhenHidden = FALSE)

  observeEvent(input$workspace_modal_retry, {
    req(rv$workspace_modal_type)
    launch_workspace_modal(rv, rv$workspace_modal_type, rv$workspace_modal_metric %||% NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$workspace_modal_client_ready, {
    request_id <- isolate(rv$workspace_modal_nonce %||% 0L)
    modal_type <- rv$workspace_modal_type %||% NULL

    if (!identical(modal_type, "analysis") || !modal_request_is_current(request_id)) {
      return(invisible(NULL))
    }

    if (identical(shiny::isolate(rv$workspace_modal_stage %||% "loading"), "ready")) {
      notify_workspace_modal_ready(request_id)
    }

    remove_analysis_launch_spinner_notification(session, request_id)
    close_analysis_launch_progress(request_id)
    invisible(NULL)
  }, ignoreInit = TRUE)

  observeEvent(rv$workspace_modal_nonce, {
    modal_type <- rv$workspace_modal_type %||% NULL
    req(!is.null(modal_type))

    metric <- rv$workspace_modal_metric %||% rv$current_metric
    request_id <- isolate(rv$workspace_modal_nonce %||% 0L)

    if (identical(modal_type, "analysis")) {
      ensure_analysis_workspace_server()
      close_analysis_launch_progress(force = TRUE)
      removeModal()
      prepare_workspace_modal(request_id, modal_type, metric)

      if (!modal_request_is_current(request_id)) {
        remove_analysis_launch_spinner_notification(session, request_id)
        close_analysis_launch_progress(request_id)
        return(invisible(NULL))
      }

      title_metric <- shiny::isolate(rv$workspace_modal_metric %||% metric)
      title <- workspace_modal_title(modal_type, title_metric)
      stage <- shiny::isolate(rv$workspace_modal_stage %||% "loading")
      body_content <- shiny::isolate(
        workspace_modal_body_content(modal_type, stage, title_metric)
      )
      show_workspace_modal_dialog(title, body_content = body_content)
      session$sendCustomMessage("bindWorkspaceModalContent", list())
      remove_analysis_launch_spinner_notification(session, request_id)

      invisible(NULL)
      return()
    }

    close_analysis_launch_progress(force = TRUE)
    title <- workspace_modal_title(modal_type, metric)
    show_workspace_modal_dialog(title)

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
