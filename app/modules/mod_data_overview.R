## ── Module: Data Overview ─────────────────────────────────────────────────────
## Sidebar layout: Data Source + Session Management in sidebar,
## inline Configuration Editor + collapsible data panels in main area.

library(shiny)
library(bslib)
library(DT)

data_setup_intro_card <- function() {
  card(
    class = "border-primary mb-3 data-setup-intro-card",
    card_header(
      class = "bg-primary text-white",
      tags$span("StreamCurves")
    ),
    card_body(
      p("This application guides you through a structured, 4-phase evaluation
         of stratification variables and reference curve development for geomorphic
         metrics. Upload your own dataset to begin the workflow and run
         the analysis."),
      p("For each metric, you will explore candidate stratifications, verify
         consistency across metrics, confirm your selection, then build and
         finalize reference curves scored on a 0-1 scale.")
    )
  )
}

upload_format_tooltip_content <- function() {
  div(
    class = "upload-format-tooltip",
    div(
      class = "upload-format-row",
      tags$strong("File type:"),
      tags$span("CSV or XLSX (first sheet used).")
    ),
    div(
      class = "upload-format-row",
      tags$strong("Structure:"),
      tags$span("Rows = sites, Columns = metrics + predictors.")
    ),
    div(
      class = "upload-format-row",
      tags$strong("Character columns:"),
      tags$span("Trimmed automatically.")
    ),
    div(
      class = "upload-format-row",
      tags$strong("Derived variables:"),
      tags$span("Computed if source columns exist.")
    )
  )
}

session_management_ui <- function(ns) {
  if (isTRUE(connect_cloud_runtime)) {
    return(
      div(
        class = "alert alert-info py-2 px-2 mt-3 mb-0",
        icon("cloud"),
        " Session save/load is unavailable in this Connect Cloud deployment."
      )
    )
  }

  tagList(
    tags$strong("Save Current Session"),
    div(
      class = "mt-2",
      textInput(ns("session_name"), NULL, placeholder = "Session name...",
                width = "100%"),
      actionButton(ns("save_session"), "Save",
                   class = "btn btn-outline-primary btn-sm w-100 mt-1",
                   icon = icon("save"))
    ),
    tags$hr(class = "my-2"),
    tags$strong("Load Previous Session"),
    div(
      class = "mt-2",
      selectInput(ns("load_session_select"), NULL,
                  choices = c("(none)"), width = "100%"),
      div(
        class = "d-flex gap-2 mt-1",
        actionButton(ns("refresh_sessions"), "",
                     class = "btn btn-outline-secondary btn-sm",
                     icon = icon("refresh")),
        actionButton(ns("load_session_btn"), "Load",
                     class = "btn btn-outline-success btn-sm flex-grow-1",
                     icon = icon("upload"))
      )
    )
  )
}

mod_data_overview_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "data-setup-shell",
    layout_sidebar(
      height = "calc(100vh - var(--app-navbar-offset, 4.5rem))",
      sidebar = sidebar(
        title = "Data & Setup",
        width = 320,
        open = list(desktop = "always", mobile = "closed"),
        fillable = TRUE,
        class = "data-setup-sidebar",

      ## ── Data Source ──────────────────────────────────────────────────────
      div(
        class = "data-upload-label",
        tags$span("Upload CSV or XLSX"),
        bslib::tooltip(
          trigger = tags$button(
            type = "button",
            class = "upload-format-help",
            `aria-label` = "Expected file format",
            "?"
          ),
          upload_format_tooltip_content(),
          placement = "right"
        )
      ),
      fileInput(
        ns("upload_file"),
        label = NULL,
        accept = c(".csv", ".xlsx"),
        buttonLabel = "Choose File",
        placeholder = "No file selected"
      ),
      uiOutput(ns("upload_status")),
      uiOutput(ns("reset_analysis_button")),
      hr(),

      ## ── Session Management ──────────────────────────────────────────────
      session_management_ui(ns)
    ),

    ## ── Main Content ──────────────────────────────────────────────────────
      uiOutput(ns("main_content"))
    )
  )
}

filter_removed_strat_rows <- function(df, removed_key) {
  if (is.null(df) || !is.data.frame(df) || !("stratification" %in% names(df))) {
    return(df)
  }

  df |>
    dplyr::filter(.data$stratification != removed_key)
}

sanitize_removed_stratification_state <- function(rv, removed_key) {
  make_none_decision <- function(metric) {
    tibble::tibble(
      metric = metric,
      decision_type = "none",
      selected_strat = NA_character_,
      selected_p_value = NA_real_,
      selected_n_groups = NA_integer_,
      selected_min_n = NA_integer_,
      runner_up_strat = NA_character_,
      runner_up_p_value = NA_real_,
      needs_review = TRUE,
      review_reason = paste0("Restored session selection removed: ", removed_key),
      notes = "Selection cleared because the stratification is no longer supported."
    )
  }

  sanitize_verification <- function(verification) {
    if (is.null(verification)) return(NULL)

    verification$finalists <- setdiff(verification$finalists %||% character(0), removed_key)

    status <- verification$verification_status %||% list()
    if (length(status) > 0 && !is.null(names(status))) {
      verification$verification_status <- status[!(names(status) %in% removed_key)]
    }

    if (identical(verification$selected_strat %||% "none", removed_key)) {
      verification$selected_strat <- "none"
    }

    verification
  }

  sanitize_screening <- function(screening) {
    if (is.null(screening)) return(NULL)

    screening$results <- filter_removed_strat_rows(screening$results, removed_key)
    screening$pairwise <- filter_removed_strat_rows(screening$pairwise, removed_key)
    if (!is.null(screening$plots)) {
      screening$plots[[removed_key]] <- NULL
    }
    screening
  }

  if (removed_key %in% names(rv$data %||% list())) {
    rv$data[[removed_key]] <- NULL
  }

  rv$strat_config[[removed_key]] <- NULL
  rv$factor_recode_config[[removed_key]] <- NULL

  for (metric in names(rv$metric_config)) {
    allowed <- rv$metric_config[[metric]]$allowed_stratifications %||% character(0)
    rv$metric_config[[metric]]$allowed_stratifications <- setdiff(allowed, removed_key)
  }

  if (!is.null(rv$decision_log) && nrow(rv$decision_log) > 0) {
    if ("selected_strat" %in% names(rv$decision_log)) {
      rv$decision_log$selected_strat[rv$decision_log$selected_strat == removed_key] <- NA_character_
    }
    if ("auto_recommended" %in% names(rv$decision_log)) {
      rv$decision_log$auto_recommended[rv$decision_log$auto_recommended == removed_key] <- NA_character_
    }
  }

  rv$phase2_settings$strat_filter <- setdiff(rv$phase2_settings$strat_filter %||% character(0), removed_key)
  rv$phase2_ranking <- filter_removed_strat_rows(rv$phase2_ranking, removed_key)
  rv$cross_metric_consistency <- NULL

  for (metric in names(rv$phase1_candidates)) {
    rv$phase1_candidates[[metric]] <- filter_removed_strat_rows(rv$phase1_candidates[[metric]], removed_key)
  }
  for (metric in names(rv$all_layer1_results)) {
    rv$all_layer1_results[[metric]] <- filter_removed_strat_rows(rv$all_layer1_results[[metric]], removed_key)
  }
  for (metric in names(rv$all_layer2_results)) {
    rv$all_layer2_results[[metric]] <- filter_removed_strat_rows(rv$all_layer2_results[[metric]], removed_key)
  }
  for (metric in names(rv$phase3_verification)) {
    rv$phase3_verification[[metric]] <- sanitize_verification(rv$phase3_verification[[metric]])
  }

  completed_with_removed <- character(0)

  for (metric in names(rv$metric_phase_cache)) {
    cache <- rv$metric_phase_cache[[metric]]
    if (is.null(cache)) next

    cache$phase1_screening <- sanitize_screening(cache$phase1_screening %||% NULL)
    cache$phase1_effect_sizes <- filter_removed_strat_rows(cache$phase1_effect_sizes %||% NULL, removed_key)

    if (!is.null(cache$strat_decision_user) &&
        nrow(cache$strat_decision_user) > 0 &&
        identical(cache$strat_decision_user$selected_strat[1] %||% NA_character_, removed_key)) {
      cache$strat_decision_user <- make_none_decision(metric)
      cache$reference_curve <- NULL
      cache$current_stratum_level <- NULL
      cache$phase4_data <- NULL
      cache$stratum_results <- NULL
    }

    rv$metric_phase_cache[[metric]] <- cache
  }

  for (metric in names(rv$completed_metrics)) {
    entry <- rv$completed_metrics[[metric]]
    if (!is.null(entry$strat_decision) &&
        nrow(entry$strat_decision) > 0 &&
        identical(entry$strat_decision$selected_strat[1] %||% NA_character_, removed_key)) {
      completed_with_removed <- c(completed_with_removed, metric)
    }
  }

  if (!is.null(rv$strat_decision_user) &&
      nrow(rv$strat_decision_user) > 0 &&
      identical(rv$strat_decision_user$selected_strat[1] %||% NA_character_, removed_key)) {
    rv$strat_decision_user <- make_none_decision(rv$current_metric %||% NA_character_)
  }

  for (metric in completed_with_removed) {
    clear_metric_phase4_results(rv, metric)
    rv$phase3_verification[[metric]] <- sanitize_verification(rv$phase3_verification[[metric]])
    if (is.null(rv$metric_phase_cache[[metric]])) {
      rv$metric_phase_cache[[metric]] <- list()
    }
    rv$metric_phase_cache[[metric]]$strat_decision_user <- make_none_decision(metric)
  }

  invisible(NULL)
}

mod_data_overview_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_persistence_enabled <- !isTRUE(connect_cloud_runtime)

    ## ── Data loaded gate ────────────────────────────────────────────────

    ## ── Config editor init trigger ──────────────────────────────────────
    config_init_trigger <- reactiveVal(0L)
    upload_error <- reactiveVal(NULL)

    mod_config_editor_server("config_editor", rv,
                              init_trigger = reactive(config_init_trigger()))


    ## ── Load Data click ─────────────────────────────────────────────────
    output$reset_analysis_button <- renderUI({
      actionButton(
        ns("reset_analysis"),
        "Reset Analysis",
        class = "btn btn-outline-danger w-100 mt-2",
        icon = icon("trash"),
        disabled = if (is.null(rv$data)) "disabled" else NULL
      )
    })

    observeEvent(input$reset_analysis, {
      req(!is.null(rv$data))
      showModal(modalDialog(
        title = "Reset Analysis",
        "This will return the app to its startup state. Loaded data,
         configuration changes, cached results, and all phase outputs
         will be cleared. This cannot be undone.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset_analysis"), "Reset Analysis",
                       class = "btn btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_reset_analysis, {
      removeModal()
      reset_app_to_startup(rv)
      upload_error(NULL)
      session$sendCustomMessage("clearModalBackdrop", list())
      showNotification("Analysis reset and app returned to startup state.",
                       type = "message", duration = 3)
    })

    ## ── Main content (shown after Load Data click) ──────────────────────
    output$main_content <- renderUI({
      config_ns <- NS(ns("config_editor"))

      loaded_content <- tagList(
        build_config_inline(config_ns),
        div(class = "mt-3"),
        bslib::accordion(
          id = ns("data_accordion"),
          open = FALSE,
          bslib::accordion_panel(
            title = "Pre-Run Validation",
            icon = bsicons::bs_icon("shield-check"),
            uiOutput(ns("validation_warnings"))
          ),
          bslib::accordion_panel(
            title = "QA Log",
            icon = bsicons::bs_icon("journal-text"),
            DT::DTOutput(ns("qa_log_table"))
          ),
          bslib::accordion_panel(
            title = "Missing Data Summary",
            icon = bsicons::bs_icon("exclamation-diamond"),
            DT::DTOutput(ns("missing_table"))
          ),
          bslib::accordion_panel(
            title = "Full Dataset",
            icon = bsicons::bs_icon("table"),
            div(style = "overflow-x: auto;", DT::DTOutput(ns("full_data_table")))
          )
        )
      )

      startup_content <- card(
        class = "border-info",
        card_header("Get Started"),
        card_body(
          p("Upload a CSV or XLSX file to load your dataset and open the setup panels automatically.")
        )
      )

      if (isTRUE(rv$app_data_loaded)) {
        loaded_content
      } else {
        tagList(
          data_setup_intro_card(),
          startup_content
        )
      }
    })

    ## ── Upload handler ────────────────────────────────────────────────────
    observeEvent(input$upload_file, {
      req(input$upload_file)
      upload_error(NULL)
      file_info <- input$upload_file
      ext <- tolower(tools::file_ext(file_info$name))

      tryCatch({
        ## Read raw data
        if (ext == "csv") {
          raw <- readr::read_csv(file_info$datapath, show_col_types = FALSE)
        } else if (ext == "xlsx") {
          raw <- readxl::read_excel(file_info$datapath, sheet = 1)
        } else {
          upload_error("Unsupported file type. Please upload CSV or XLSX.")
          showNotification("Unsupported file type. Please upload CSV or XLSX.",
                           type = "error", duration = 5)
          return()
        }

        ## Trim character columns
        raw <- raw |>
          dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim))

        ## Clean + derive
        clean_result <- clean_data(raw, rv$metric_config, rv$strat_config)
        derived <- derive_variables(clean_result$data,
                                    rv$factor_recode_config,
                                    rv$predictor_config)

        ## Precheck
        precheck <- run_metric_precheck(derived, rv$metric_config)

        ## Reset analysis since data changed
        reset_all_analysis(rv)

        ## Update reactive state
        rv$data            <- derived
        rv$qa_log          <- clean_result$qa_log
        rv$precheck_df     <- precheck
        rv$data_source     <- "upload"
        rv$upload_filename <- file_info$name
        rv$data_fingerprint <- digest::digest(derived, algo = "md5")

        rv$app_data_loaded <- TRUE
        session$onFlushed(function() {
          config_init_trigger(isolate(config_init_trigger()) + 1L)
        }, once = TRUE)

        showNotification(
          paste0("Loaded ", nrow(derived), " rows x ", ncol(derived),
                 " cols from ", file_info$name),
          type = "message", duration = 5
        )
      }, error = function(e) {
        upload_error(conditionMessage(e))
        showNotification(
          paste0("Upload failed: ", conditionMessage(e)),
          type = "error", duration = 8
        )
      })
    })

    ## ── Switch back to built-in data ─────────────────────────────────────

    ## ── Upload status display ────────────────────────────────────────────
    output$upload_status <- renderUI({
      if (!is.null(upload_error())) {
        div(
          class = "alert alert-danger py-1 px-2 mt-1",
          icon("exclamation-triangle"),
          upload_error()
        )
      }
    })

    output$qa_log_table <- DT::renderDT({
      DT::datatable(
        rv$qa_log,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    output$missing_table <- DT::renderDT({
      missing_counts <- sapply(rv$data, function(x) sum(is.na(x)))
      missing_df <- data.frame(
        Column = names(missing_counts),
        Missing = as.integer(missing_counts),
        Percent = round(100 * missing_counts / nrow(rv$data), 1),
        stringsAsFactors = FALSE
      )
      missing_df <- missing_df[missing_df$Missing > 0, ]
      missing_df <- missing_df[order(-missing_df$Missing), ]

      DT::datatable(
        missing_df,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    output$full_data_table <- DT::renderDT({
      DT::datatable(
        rv$data,
        options = list(pageLength = 15, scrollX = TRUE, scrollY = "400px"),
        rownames = FALSE,
        class = "compact stripe",
        filter = "top"
      )
    })

    ## ── Validation warnings ─────────────────────────────────────────────
    output$validation_warnings <- renderUI({
      warnings <- list()

      ## Check for sparse ecoregions
      if ("Ecoregion" %in% names(rv$data)) {
        eco_counts <- table(rv$data$Ecoregion)
        sparse <- eco_counts[eco_counts < 10]
        if (length(sparse) > 0) {
          for (eco in names(sparse)) {
            warnings <- c(warnings, list(
              div(class = "alert alert-warning py-1 px-2 mb-1",
                  icon("exclamation-triangle"),
                  sprintf(" Ecoregion '%s' has only %d sites", eco, sparse[[eco]]))
            ))
          }
        }
      }

      ## Check metrics with few observations
      precheck <- rv$precheck_df
      if (!is.null(precheck) && nrow(precheck) > 0) {
        sparse_metrics <- precheck |> dplyr::filter(n_obs < 15)
        if (nrow(sparse_metrics) > 0) {
          warnings <- c(warnings, list(
            div(class = "alert alert-info py-1 px-2 mb-1",
                icon("info-circle"),
                sprintf(" %d metric(s) have fewer than 15 observations",
                        nrow(sparse_metrics)))
          ))
        }
      }

      ## Check DACAT=3
      if ("DACAT" %in% names(rv$data)) {
        dacat3_n <- sum(rv$data$DACAT == 3, na.rm = TRUE)
        if (dacat3_n < 10) {
          warnings <- c(warnings, list(
            div(class = "alert alert-warning py-1 px-2 mb-1",
                icon("exclamation-triangle"),
                sprintf(" DACAT=3 (large watershed) has only %d sites", dacat3_n))
          ))
        }
      }

      if (length(warnings) == 0) {
        div(class = "text-success", icon("check-circle"), " No validation warnings.")
      } else {
        tagList(warnings)
      }
    })

    ## ── Session save ────────────────────────────────────────────────────
    observeEvent(input$save_session, {
      if (!session_persistence_enabled) {
        return(invisible(NULL))
      }

      req(input$session_name, nchar(trimws(input$session_name)) > 0)
      session_name <- trimws(input$session_name)

      ## Create sessions directory
      sessions_dir <- file.path(project_root, "outputs", "sessions")
      dir.create(sessions_dir, showWarnings = FALSE, recursive = TRUE)

      session_data <- list(
        version = "2.3",
        saved_at = Sys.time(),
        session_name = session_name,
        data_source = rv$data_source,
        data_fingerprint = rv$data_fingerprint,
        ## Phase 1
        phase1_candidates = rv$phase1_candidates,
        all_layer1_results = rv$all_layer1_results,
        all_layer2_results = rv$all_layer2_results,
        ## Phase 2
        phase2_ranking = rv$phase2_ranking,
        cross_metric_consistency = rv$cross_metric_consistency,
        phase2_settings = rv$phase2_settings,
        phase2_metric_overrides = rv$phase2_metric_overrides,
        summary_available_overrides = rv$summary_available_overrides,
        summary_edit_notes = rv$summary_edit_notes,
        ## Phase 3
        phase3_verification = rv$phase3_verification,
        ## Per-metric cache
        metric_phase_cache = rv$metric_phase_cache,
        ## Per-stratum results
        stratum_results = rv$stratum_results,
        ## Global
        completed_metrics = rv$completed_metrics,
        decision_log = rv$decision_log,
        ## Configs
        metric_config = isolate(rv$metric_config),
        strat_config = isolate(rv$strat_config),
        predictor_config = isolate(rv$predictor_config),
        factor_recode_config = isolate(rv$factor_recode_config),
        output_config = isolate(rv$output_config),
        config_version = isolate(rv$config_version)
      )

      save_path <- file.path(sessions_dir, paste0(session_name, ".rds"))
      saveRDS(session_data, save_path)

      showNotification(
        paste0("Session '", session_name, "' saved successfully."),
        type = "message", duration = 3
      )
    })

    ## ── Refresh session list ────────────────────────────────────────────
    refresh_session_list <- function() {
      if (!session_persistence_enabled) {
        return(invisible(NULL))
      }

      sessions_dir <- file.path(project_root, "outputs", "sessions")
      if (dir.exists(sessions_dir)) {
        files <- list.files(sessions_dir, pattern = "\\.rds$", full.names = FALSE)
        session_names <- tools::file_path_sans_ext(files)
        if (length(session_names) > 0) {
          choices <- c("(none)", session_names)
        } else {
          choices <- "(none)"
        }
      } else {
        choices <- "(none)"
      }
      updateSelectInput(session, "load_session_select", choices = choices)
    }

    if (session_persistence_enabled) {
      observe({ refresh_session_list() })
      observeEvent(input$refresh_sessions, { refresh_session_list() })
    }

    observeEvent(rv$app_reset_nonce, {
      upload_error(NULL)
      if (session_persistence_enabled) {
        refresh_session_list()
        updateTextInput(session, "session_name", value = "")
        updateSelectInput(session, "load_session_select", selected = "(none)")
      }
      session$sendCustomMessage("clearFileInput",
                                list(id = ns("upload_file")))
      session$sendCustomMessage("clearModalBackdrop", list())
    }, ignoreInit = TRUE)

    ## ── Session load ────────────────────────────────────────────────────
    observeEvent(input$load_session_btn, {
      if (!session_persistence_enabled) {
        return(invisible(NULL))
      }

      req(input$load_session_select, input$load_session_select != "(none)")
      session_name <- input$load_session_select

      load_path <- file.path(project_root, "outputs", "sessions",
                             paste0(session_name, ".rds"))
      if (!file.exists(load_path)) {
        showNotification("Session file not found.", type = "error")
        return()
      }

      session_data <- readRDS(load_path)

      if (identical(session_data$data_source %||% NULL, "builtin")) {
        showNotification(
          paste0(
            "Session '", session_name,
            "' uses the removed built-in example dataset and cannot be restored. Upload a dataset first."
          ),
          type = "error",
          duration = 8
        )
        return()
      }

      ## Verify data fingerprint
      current_fp <- rv$data_fingerprint
      if (!is.null(session_data$data_fingerprint) &&
          !is.null(current_fp) && !is.na(current_fp) &&
          session_data$data_fingerprint != current_fp) {
        showNotification(
          "Warning: Dataset has changed since session was saved. Results may be stale.",
          type = "warning", duration = 8
        )
      }

      ## Restore state — handle both v1.2 and v2.0 formats
      session_version <- session_data$version %||% "1.0"

      if (!is.null(session_data$completed_metrics)) {
        rv$completed_metrics <- session_data$completed_metrics
      }
      if (!is.null(session_data$decision_log)) {
        rv$decision_log <- session_data$decision_log
      }
      if (!is.null(session_data$all_layer1_results)) {
        rv$all_layer1_results <- session_data$all_layer1_results
      }
      if (!is.null(session_data$all_layer2_results)) {
        rv$all_layer2_results <- session_data$all_layer2_results
      }
      if (!is.null(session_data$cross_metric_consistency)) {
        rv$cross_metric_consistency <- session_data$cross_metric_consistency
      }
      rv$phase2_settings <- empty_phase2_settings()

      if (session_version >= "2.0") {
        ## v2.0 fields
        rv$phase2_metric_overrides <- list()
        rv$summary_available_overrides <- list()
        rv$summary_edit_notes <- list()
        if (!is.null(session_data$phase1_candidates)) {
          rv$phase1_candidates <- session_data$phase1_candidates
        }
        if (!is.null(session_data$phase2_ranking)) {
          rv$phase2_ranking <- session_data$phase2_ranking
        }
        if (!is.null(session_data$phase2_settings)) {
          rv$phase2_settings <- session_data$phase2_settings
        }
        if (!is.null(session_data$phase2_metric_overrides)) {
          rv$phase2_metric_overrides <- session_data$phase2_metric_overrides
        }
        if (!is.null(session_data$summary_available_overrides)) {
          rv$summary_available_overrides <- session_data$summary_available_overrides
        }
        if (!is.null(session_data$summary_edit_notes)) {
          rv$summary_edit_notes <- session_data$summary_edit_notes
        }
        if (!is.null(session_data$phase3_verification)) {
          rv$phase3_verification <- session_data$phase3_verification
        }
        if (!is.null(session_data$metric_phase_cache)) {
          rv$metric_phase_cache <- session_data$metric_phase_cache
        }
        if (!is.null(session_data$stratum_results)) {
          rv$stratum_results <- session_data$stratum_results
        }
      } else {
        ## v1.2 compatibility: map metric_step_cache -> metric_phase_cache
        if (!is.null(session_data$metric_step_cache)) {
          rv$metric_phase_cache <- session_data$metric_step_cache
        }
      }

      ## Restore configs if present
      if (!is.null(session_data$metric_config)) {
        rv$metric_config <- session_data$metric_config
      }
      if (!is.null(session_data$strat_config)) {
        rv$strat_config <- session_data$strat_config
      }
      if (!is.null(session_data$predictor_config)) {
        rv$predictor_config <- session_data$predictor_config
      }
      if (!is.null(session_data$factor_recode_config)) {
        rv$factor_recode_config <- session_data$factor_recode_config
      }
      if (!is.null(session_data$output_config)) {
        rv$output_config <- session_data$output_config
      }
      if (!is.null(session_data$config_version)) {
        rv$config_version <- session_data$config_version
      }

      sanitize_removed_stratification_state(rv, "Ecoregion_Group")
      rv$phase2_settings <- normalize_phase2_settings(rv, rv$phase2_settings)

      ## Restore per-phase state for whichever metric is currently selected
      if (!is.null(rv$current_metric) &&
          rv$current_metric %in% names(rv$metric_phase_cache)) {
        restore_metric_phase_state(rv, rv$current_metric)
      }

      ## Re-populate config editor with restored configs
      rv$app_data_loaded <- !is.null(rv$data)
      config_init_trigger(isolate(config_init_trigger()) + 1L)

      showNotification(
        paste0("Session '", session_name, "' loaded. ",
               length(rv$completed_metrics), " completed metrics restored."),
        type = "message", duration = 5
      )
    })

  })
}
