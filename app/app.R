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
    workspace_modal_nonce = 0L,
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

  observeEvent(rv$workspace_modal_nonce, {
    modal_type <- rv$workspace_modal_type %||% NULL
    req(!is.null(modal_type))

    metric <- rv$workspace_modal_metric %||% rv$current_metric
    title <- switch(
      modal_type,
      phase1 = paste0("Phase 1: Explore", if (!is.null(metric)) paste0(" — ", rv$metric_config[[metric]]$display_name %||% metric) else ""),
      phase2 = "Phase 2: Compare",
      phase3 = paste0("Phase 3: Verify", if (!is.null(metric)) paste0(" — ", rv$metric_config[[metric]]$display_name %||% metric) else ""),
      phase4 = paste0("Phase 4: Finalize", if (!is.null(metric)) paste0(" — ", rv$metric_config[[metric]]$display_name %||% metric) else ""),
      summary_export = "Summary / Export",
      "Workspace"
    )

    body <- switch(
      modal_type,
      phase1 = mod_phase1_exploration_ui("phase1", dialog_mode = TRUE),
      phase2 = mod_phase2_consistency_ui("phase2"),
      phase3 = mod_phase3_verification_ui("phase3", dialog_mode = TRUE),
      phase4 = mod_phase4_finalization_ui("phase4", dialog_mode = TRUE),
      summary_export = mod_summary_export_ui("summary_export"),
      div(class = "alert alert-warning", "Unknown workspace requested.")
    )

    removeModal()
    showModal(
      modalDialog(
        title = title,
        body,
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "xl",
        class = "workspace-modal-dialog"
      )
    )
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
