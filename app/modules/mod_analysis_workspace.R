library(shiny)
library(bslib)

mod_analysis_workspace_ui <- function(id) {
  ns <- NS(id)

  navset_card_tab(
    id = ns("analysis_tabs"),
    selected = "reference_curves",
    nav_panel("Exploratory", value = "exploratory", mod_phase1_exploration_ui(ns("phase1"), dialog_mode = TRUE)),
    nav_panel("Cross-Metric Analysis", value = "cross_metric", mod_phase2_consistency_ui(ns("phase2"))),
    nav_panel("Verification", value = "verification", mod_phase3_verification_ui(ns("phase3"), dialog_mode = TRUE)),
    nav_panel("Reference Curves", value = "reference_curves", mod_phase4_finalization_ui(ns("phase4"), dialog_mode = TRUE))
  )
}

mod_analysis_workspace_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    root_session <- session$rootScope() %||% session

    mod_phase1_exploration_server("phase1", rv, dialog_mode = TRUE, workspace_scope = "analysis")
    mod_phase2_consistency_server("phase2", rv, workspace_scope = "analysis")
    mod_phase3_verification_server("phase3", rv, parent_session = root_session, dialog_mode = TRUE, workspace_scope = "analysis")
    mod_phase4_finalization_server("phase4", rv, dialog_mode = TRUE, workspace_scope = "analysis")

    observeEvent(input$analysis_tabs, {
      if (!isTRUE(workspace_scope_is_active(
        rv,
        workspace_scope = "analysis",
        isolate_state = TRUE
      ))) {
        return(invisible(NULL))
      }

      request_id <- rv$analysis_tab_request_id %||% NULL
      selected_tab <- input$analysis_tabs %||% "reference_curves"
      if (!analysis_tab_request_is_current(rv, request_id) ||
          !(selected_tab %in% analysis_tab_keys())) {
        return(invisible(NULL))
      }

      current_status <- get_analysis_tab_status(rv, selected_tab)
      if (identical(current_status, "ready") || identical(current_status, "loading")) {
        return(invisible(NULL))
      }

      set_analysis_tab_status(rv, selected_tab, "loading", request_id)
      request_analysis_tab_preload(rv, selected_tab, request_id)
      invisible(NULL)
    }, ignoreInit = FALSE)

    invisible(NULL)
  })
}
