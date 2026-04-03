app_lines <- readLines("app/app.R", warn = FALSE)
app_text <- paste(app_lines, collapse = "\n")
summary_lines <- readLines("app/modules/mod_summary_page.R", warn = FALSE)
summary_text <- paste(summary_lines, collapse = "\n")
phase_tracker_lines <- readLines("app/helpers/phase_tracker.R", warn = FALSE)
phase_tracker_text <- paste(phase_tracker_lines, collapse = "\n")
notification_lines <- readLines("app/helpers/notifications.R", warn = FALSE)
notification_text <- paste(notification_lines, collapse = "\n")
analysis_workspace_lines <- readLines("app/modules/mod_analysis_workspace.R", warn = FALSE)
analysis_workspace_text <- paste(analysis_workspace_lines, collapse = "\n")
phase1_lines <- readLines("app/modules/mod_phase1_exploration.R", warn = FALSE)
phase3_lines <- readLines("app/modules/mod_phase3_verification.R", warn = FALSE)
phase4_lines <- readLines("app/modules/mod_phase4_finalization.R", warn = FALSE)
custom_js_lines <- readLines("app/www/custom.js", warn = FALSE)
custom_js_text <- paste(custom_js_lines, collapse = "\n")

stopifnot(any(grepl("analysis_launch_total_steps <- function", app_lines, fixed = TRUE)))
stopifnot(any(grepl("analysis_launch_progress_value <- function", app_lines, fixed = TRUE)))
stopifnot(any(grepl("show_analysis_launch_progress <- function", app_lines, fixed = TRUE)))
stopifnot(any(grepl("update_analysis_launch_progress <- function", app_lines, fixed = TRUE)))
stopifnot(any(grepl("advance_analysis_launch_progress <- function", app_lines, fixed = TRUE)))
stopifnot(any(grepl("make_analysis_launch_progress_adapter <- function", app_lines, fixed = TRUE)))
stopifnot(any(grepl("show_analysis_launch_final_loading <- function", app_lines, fixed = TRUE)))
stopifnot(any(grepl("analysis_launch_progress_message <- function", app_lines, fixed = TRUE)))
stopifnot(any(grepl("show_workspace_modal_dialog <- function", app_lines, fixed = TRUE)))
stopifnot(any(grepl("notify_workspace_modal_ready <- function", app_lines, fixed = TRUE)))
stopifnot(any(grepl("close_analysis_launch_progress <- function", app_lines, fixed = TRUE)))
stopifnot(any(grepl("remove_analysis_launch_spinner_notification(session, request_id)", app_lines, fixed = TRUE)))
stopifnot(!any(grepl("show_analysis_launch_progress(request_id, metric)", app_lines, fixed = TRUE)))
stopifnot(any(grepl("close_analysis_launch_progress(force = TRUE)", app_lines, fixed = TRUE)))
stopifnot(any(grepl("body_content <- shiny::isolate(", app_lines, fixed = TRUE)))
stopifnot(any(grepl("workspace_modal_body_content(modal_type, stage, title_metric)", app_lines, fixed = TRUE)))
stopifnot(any(grepl("show_workspace_modal_dialog(title, body_content = body_content)", app_lines, fixed = TRUE)))
stopifnot(any(grepl("analysis = NULL", app_lines, fixed = TRUE)))
stopifnot(any(grepl('notify = !identical\\(modal_type, "analysis"\\)', app_lines)))
stopifnot(any(grepl("analysis_launch_spinner_notification_id <- function", notification_lines, fixed = TRUE)))
stopifnot(any(grepl("show_analysis_launch_spinner_notification <- function", notification_lines, fixed = TRUE)))
stopifnot(any(grepl("remove_analysis_launch_spinner_notification <- function", notification_lines, fixed = TRUE)))
stopifnot(any(grepl("show_final_loading_notification <- function", notification_lines, fixed = TRUE)))
stopifnot(any(grepl("final_loading_notification_ui <- function", notification_lines, fixed = TRUE)))
stopifnot(any(grepl("close_button = TRUE", notification_lines, fixed = TRUE)))
stopifnot(any(grepl("closeButton = FALSE", notification_lines, fixed = TRUE)))
stopifnot(any(grepl('paste0\\("analysis-launch-spinner-", resolved_request_id\\)', notification_lines)))
stopifnot(any(grepl("next_workspace_modal_request_id <- function", phase_tracker_lines, fixed = TRUE)))
stopifnot(any(grepl("request_analysis_tab_preload <- function", phase_tracker_lines, fixed = TRUE)))
stopifnot(any(grepl("complete_analysis_tab_preload <- function", phase_tracker_lines, fixed = TRUE)))
stopifnot(any(grepl('request_id <- next_workspace_modal_request_id\\(rv\\)', summary_lines)))
stopifnot(any(grepl("show_analysis_launch_spinner_notification\\(", summary_lines)))
stopifnot(any(grepl('Loading page, please wait\\.', summary_lines)))
stopifnot(any(grepl('launch_workspace_modal\\(rv, "analysis", metric, request_id = request_id\\)', summary_lines)))
stopifnot(any(grepl('mod_phase1_exploration_ui\\(ns\\("phase1"\\), dialog_mode = TRUE\\)', analysis_workspace_lines)))
stopifnot(any(grepl('mod_phase2_consistency_ui\\(ns\\("phase2"\\)\\)', analysis_workspace_lines)))
stopifnot(any(grepl('mod_phase3_verification_ui\\(ns\\("phase3"\\), dialog_mode = TRUE\\)', analysis_workspace_lines)))
stopifnot(any(grepl('mod_phase4_finalization_ui\\(ns\\("phase4"\\), dialog_mode = TRUE\\)', analysis_workspace_lines)))
stopifnot(any(grepl('mod_phase1_exploration_server\\("phase1", rv, dialog_mode = TRUE, workspace_scope = "analysis"\\)', analysis_workspace_lines)))
stopifnot(any(grepl('mod_phase2_consistency_server\\("phase2", rv, workspace_scope = "analysis"\\)', analysis_workspace_lines)))
stopifnot(any(grepl('mod_phase3_verification_server\\("phase3", rv, parent_session = root_session, dialog_mode = TRUE, workspace_scope = "analysis"\\)', analysis_workspace_lines)))
stopifnot(any(grepl('mod_phase4_finalization_server\\("phase4", rv, dialog_mode = TRUE, workspace_scope = "analysis"\\)', analysis_workspace_lines)))
stopifnot(any(grepl('observeEvent\\(input\\$analysis_tabs, \\{', analysis_workspace_lines)))
stopifnot(any(grepl('set_analysis_tab_status\\(rv, selected_tab, "loading", request_id\\)', analysis_workspace_lines)))
stopifnot(any(grepl('request_analysis_tab_preload\\(rv, selected_tab, request_id\\)', analysis_workspace_lines)))
stopifnot(any(grepl('outputOptions\\(output, "phase1_page", suspendWhenHidden = FALSE\\)', phase1_lines)))
stopifnot(any(grepl('outputOptions\\(output, "phase3_page", suspendWhenHidden = FALSE\\)', phase3_lines)))
stopifnot(any(grepl('outputOptions\\(output, "phase4_page", suspendWhenHidden = FALSE\\)', phase4_lines)))
stopifnot(any(grepl("uiOutput\\(ns\\(\"artifact_status\"\\)\\)", phase4_lines)))
stopifnot(any(grepl('artifact_mode = "full"', phase4_lines, fixed = TRUE)))
stopifnot(any(grepl('artifact_mode = "summary"', phase4_lines, fixed = TRUE)))
stopifnot(any(grepl('session\\$sendCustomMessage\\("clearModalBackdrop", list\\(\\)\\)', app_lines)))
stopifnot(any(grepl('Shiny.addCustomMessageHandler\\("clearModalBackdrop", function\\(message\\)', custom_js_lines)))
stopifnot(any(grepl('\\$\\(document\\)\\.on\\("hidden\\.bs\\.modal", "\\.workspace-modal-dialog", function\\(\\)', custom_js_lines)))
stopifnot(any(grepl('\\$\\(document\\)\\.on\\("shown\\.bs\\.modal", "#shiny-modal", function\\(\\)', custom_js_lines)))
stopifnot(any(grepl("Shiny\\.setInputValue\\(\"workspace_modal_client_ready\", Date\\.now\\(\\)", custom_js_lines)))

stopifnot(grepl(
  'next_workspace_modal_request_id <- function\\(rv\\) \\{[\\s\\S]*isolate\\(rv\\$workspace_modal_nonce %\\|\\|% 0L\\) \\+ 1L[\\s\\S]*launch_workspace_modal <- function\\(rv, phase, metric = NULL, request_id = NULL\\) \\{[\\s\\S]*next_request_id <- request_id %\\|\\|% next_workspace_modal_request_id\\(rv\\)[\\s\\S]*rv\\$workspace_modal_nonce <- next_request_id[\\s\\S]*invisible\\(next_request_id\\)',
  phase_tracker_text,
  perl = TRUE
))

stopifnot(grepl(
  'observeEvent\\(input\\[\\[paste0\\("open_analysis_", metric\\)\\]\\], \\{[\\s\\S]*request_id <- next_workspace_modal_request_id\\(rv\\)[\\s\\S]*show_analysis_launch_spinner_notification\\([\\s\\S]*Loading page, please wait\\.[\\s\\S]*launch_workspace_modal\\(rv, "analysis", metric, request_id = request_id\\)',
  summary_text,
  perl = TRUE
))

stopifnot(grepl(
  'workspace_modal_metric_label <- function\\(metric\\) \\{[\\s\\S]*metric_config_snapshot <- shiny::isolate\\(rv\\$metric_config %\\|\\|% list\\(\\)\\)[\\s\\S]*metric_entry <- metric_config_snapshot\\[\\[metric\\]\\] %\\|\\|% list\\(\\)[\\s\\S]*metric_entry\\$display_name %\\|\\|% metric',
  app_text,
  perl = TRUE
))

stopifnot(grepl(
  'observeEvent\\(input\\$workspace_modal_client_ready, \\{[\\s\\S]*request_id <- isolate\\(rv\\$workspace_modal_nonce %\\|\\|% 0L\\)[\\s\\S]*modal_type <- rv\\$workspace_modal_type %\\|\\|% NULL[\\s\\S]*if \\(!identical\\(modal_type, "analysis"\\) \\|\\| !modal_request_is_current\\(request_id\\)\\) \\{[\\s\\S]*if \\(identical\\(shiny::isolate\\(rv\\$workspace_modal_stage %\\|\\|% "loading"\\), "ready"\\)\\) \\{[\\s\\S]*notify_workspace_modal_ready\\(request_id\\)[\\s\\S]*remove_analysis_launch_spinner_notification\\(session, request_id\\)[\\s\\S]*close_analysis_launch_progress\\(request_id\\)',
  app_text,
  perl = TRUE
))

stopifnot(grepl(
  'if \\(identical\\(modal_type, "analysis"\\)\\) \\{[\\s\\S]*ensure_analysis_workspace_server\\(\\)[\\s\\S]*close_analysis_launch_progress\\(force = TRUE\\)[\\s\\S]*removeModal\\(\\)[\\s\\S]*prepare_workspace_modal\\(request_id, modal_type, metric\\)[\\s\\S]*title_metric <- shiny::isolate\\(rv\\$workspace_modal_metric %\\|\\|% metric\\)[\\s\\S]*title <- workspace_modal_title\\(modal_type, title_metric\\)[\\s\\S]*stage <- shiny::isolate\\(rv\\$workspace_modal_stage %\\|\\|% "loading"\\)[\\s\\S]*body_content <- shiny::isolate\\([\\s\\S]*workspace_modal_body_content\\(modal_type, stage, title_metric\\)[\\s\\S]*show_workspace_modal_dialog\\(title, body_content = body_content\\)[\\s\\S]*session\\$sendCustomMessage\\("bindWorkspaceModalContent", list\\(\\)\\)[\\s\\S]*remove_analysis_launch_spinner_notification\\(session, request_id\\)[\\s\\S]*invisible\\(NULL\\)[\\s\\S]*return\\(\\)',
  app_text,
  perl = TRUE
))

stopifnot(grepl(
  'mod_analysis_workspace_server <- function\\(id, rv\\) \\{[\\s\\S]*root_session <- session\\$rootScope\\(\\) %\\|\\|% session[\\s\\S]*mod_phase1_exploration_server\\("phase1", rv, dialog_mode = TRUE, workspace_scope = "analysis"\\)[\\s\\S]*mod_phase2_consistency_server\\("phase2", rv, workspace_scope = "analysis"\\)[\\s\\S]*mod_phase3_verification_server\\("phase3", rv, parent_session = root_session, dialog_mode = TRUE, workspace_scope = "analysis"\\)[\\s\\S]*mod_phase4_finalization_server\\("phase4", rv, dialog_mode = TRUE, workspace_scope = "analysis"\\)[\\s\\S]*observeEvent\\(input\\$analysis_tabs, \\{[\\s\\S]*set_analysis_tab_status\\(rv, selected_tab, "loading", request_id\\)[\\s\\S]*request_analysis_tab_preload\\(rv, selected_tab, request_id\\)',
  analysis_workspace_text,
  perl = TRUE
))

stopifnot(grepl(
  'function signalWorkspaceModalReady\\(modalElement\\) \\{[\\s\\S]*bindWorkspaceModalContent\\(modalElement\\)[\\s\\S]*Shiny\\.setInputValue\\("workspace_modal_client_ready", Date\\.now\\(\\), \\{[\\s\\S]*priority: "event"',
  custom_js_text,
  perl = TRUE
))

stopifnot(grepl(
  'show_workspace_modal_dialog <- function\\(title, body_content = uiOutput\\("workspace_modal_body"\\)\\) \\{[\\s\\S]*removeModal\\(\\)[\\s\\S]*session\\$sendCustomMessage\\("clearModalBackdrop", list\\(\\)\\)[\\s\\S]*showModal\\(',
  app_text,
  perl = TRUE
))

cat("analysis_launch_feedback_checks: OK\n")
