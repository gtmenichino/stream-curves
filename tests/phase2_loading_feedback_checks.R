phase2_lines <- readLines("app/modules/mod_phase2_consistency.R", warn = FALSE)
phase2_text <- paste(phase2_lines, collapse = "\n")
summary_lines <- readLines("app/helpers/summary_page.R", warn = FALSE)
summary_text <- paste(summary_lines, collapse = "\n")
js_lines <- readLines("app/www/custom.js", warn = FALSE)
js_text <- paste(js_lines, collapse = "\n")
css_lines <- readLines("app/www/custom.css", warn = FALSE)
css_text <- paste(css_lines, collapse = "\n")

stopifnot(any(grepl('matrix_loading <- reactiveVal\\(FALSE\\)', phase2_lines)))
stopifnot(any(grepl('mod_phase2_consistency_server <- function\\(id, rv, workspace_scope = "standalone"\\)', phase2_lines)))
stopifnot(any(grepl('phase2_workspace_active <- function\\(isolate_state = FALSE\\)', phase2_lines)))
stopifnot(any(grepl('loading_mode <- reactiveVal\\("idle"\\)', phase2_lines)))
stopifnot(any(grepl('pending_refresh <- reactiveVal\\(NULL\\)', phase2_lines)))
stopifnot(any(grepl('active_refresh_request_id <- reactiveVal\\(NULL\\)', phase2_lines)))
stopifnot(any(grepl('phase2_controls_ready <- reactiveVal\\(FALSE\\)', phase2_lines)))
stopifnot(any(grepl('phase2_applied_settings <- reactiveVal\\(NULL\\)', phase2_lines)))
stopifnot(any(grepl('phase2_programmatic_strat_update <- reactiveVal\\(FALSE\\)', phase2_lines)))
stopifnot(any(grepl('phase2_control_reset_nonce <- reactiveVal\\(0L\\)', phase2_lines)))
stopifnot(any(grepl('loading_notification_id <- ns\\("consistency_matrix_loading"\\)', phase2_lines)))
stopifnot(any(grepl('phase2_results_ready_input_id <- ns\\("phase2_results_ready"\\)', phase2_lines)))
stopifnot(any(grepl('get_matrix_loading <- function\\(', phase2_lines)))
stopifnot(any(grepl('get_pending_refresh <- function\\(', phase2_lines)))
stopifnot(any(grepl('get_active_refresh_request_id <- function\\(', phase2_lines)))
stopifnot(any(grepl('get_phase2_controls_ready <- function\\(', phase2_lines)))
stopifnot(any(grepl('get_phase2_applied_settings <- function\\(', phase2_lines)))
stopifnot(any(grepl('get_phase2_programmatic_strat_update <- function\\(', phase2_lines)))
stopifnot(any(grepl('resolve_phase2_settings <- function\\(', phase2_lines)))
stopifnot(any(grepl('phase2_settings_match <- function\\(', phase2_lines)))
stopifnot(any(grepl('phase2_input_settings <- function\\(', phase2_lines)))
stopifnot(any(grepl('phase2_inputs_bound <- function\\(', phase2_lines)))
stopifnot(any(grepl('reset_phase2_controls_state <- function\\(', phase2_lines)))
stopifnot(any(grepl('show_final_loading_notification\\(', phase2_lines)))
stopifnot(any(grepl('remove_final_loading_notification\\(', phase2_lines)))
stopifnot(any(grepl('close_button = FALSE', phase2_lines, fixed = TRUE)))
stopifnot(any(grepl('No second click is needed\\.', phase2_lines)))
stopifnot(grepl(
  'observeEvent\\(input\\$phase2_results_ready, \\{[\\s\\S]*finish_consistency_refresh\\(request_id = input\\$phase2_results_ready %\\|\\|% NULL\\)',
  phase2_text,
  perl = TRUE
))
stopifnot(any(grepl('session\\$sendCustomMessage\\(', phase2_lines)))
stopifnot(any(grepl('"watchPhase2ResultsReady"', phase2_lines, fixed = TRUE)))
stopifnot(any(grepl('finish_consistency_refresh(request_id = request_id)', phase2_lines, fixed = TRUE)))
stopifnot(any(grepl('refresh_phase2_ranking_shared\\(rv, resolved_settings, persist_settings = FALSE\\)', phase2_lines)))
stopifnot(any(grepl('recompute_phase2_shared\\(rv, resolved_settings, persist_settings = FALSE\\)', phase2_lines)))
stopifnot(any(grepl('freezeReactiveValue(input, "strat_filter")', phase2_lines, fixed = TRUE)))
stopifnot(any(grepl('outputOptions\\(output, "consistency_ui", suspendWhenHidden = FALSE\\)', phase2_lines)))
stopifnot(any(grepl('outputOptions\\(output, "results_ui", suspendWhenHidden = FALSE\\)', phase2_lines)))
stopifnot(any(grepl('outputOptions\\(output, "current_metric_context", suspendWhenHidden = FALSE\\)', phase2_lines)))
stopifnot(any(grepl('outputOptions\\(output, "sig_threshold_label", suspendWhenHidden = FALSE\\)', phase2_lines)))
stopifnot(any(grepl('outputOptions\\(output, "heatmap", suspendWhenHidden = FALSE\\)', phase2_lines)))
stopifnot(any(grepl('outputOptions\\(output, "ranking_table", suspendWhenHidden = FALSE\\)', phase2_lines)))
stopifnot(any(grepl('if \\(!isTRUE\\(phase2_workspace_active\\(\\)\\)\\) \\{', phase2_lines)))
stopifnot(!any(grepl('uiOutput\\(ns\\("artifact_status"\\)\\)', phase2_lines)))
stopifnot(!any(grepl('output\\$artifact_status <- renderUI\\(', phase2_lines)))
stopifnot(!any(grepl('matrix_error <- reactiveVal\\(NULL\\)', phase2_lines)))
stopifnot(!any(grepl('finish_consistency_refresh <- function\\(request_id = active_refresh_request_id\\(', phase2_lines)))

stopifnot(grepl(
  'phase2_workspace_active <- function\\(isolate_state = FALSE\\)[\\s\\S]*get_phase2_controls_ready <- function\\([\\s\\S]*phase2_settings_match <- function\\([\\s\\S]*reset_phase2_controls_state <- function\\([\\s\\S]*finish_consistency_refresh <- function\\(request_id = NULL,[\\s\\S]*current_request_id <- get_active_refresh_request_id\\([\\s\\S]*next_request <- get_pending_refresh\\([\\s\\S]*request_consistency_refresh <- function\\([\\s\\S]*phase2_workspace_active\\(isolate_state = TRUE\\)[\\s\\S]*resolved_settings <- resolve_phase2_settings\\(settings\\)[\\s\\S]*phase2_settings_match\\(resolved_settings, get_phase2_applied_settings\\(\\)\\)[\\s\\S]*set_phase2_applied_settings\\(resolved_settings\\)[\\s\\S]*session\\$sendCustomMessage\\([\\s\\S]*"watchPhase2ResultsReady"[\\s\\S]*finish_consistency_refresh\\(request_id = request_id\\)',
  phase2_text,
  perl = TRUE
))

stopifnot(grepl(
  'observe\\([\\s\\S]*phase2_control_reset_nonce\\([\\s\\S]*phase2_inputs_bound\\([\\s\\S]*set_phase2_applied_settings\\(resolve_phase2_settings\\(phase2_input_settings\\(\\)\\)\\)[\\s\\S]*set_phase2_controls_ready\\(TRUE\\)[\\s\\S]*observeEvent\\(input\\$support_threshold, \\{[\\s\\S]*get_phase2_controls_ready\\([\\s\\S]*mode = "tier_only"[\\s\\S]*support_threshold = input\\$support_threshold[\\s\\S]*\\}, ignoreInit = TRUE\\)',
  phase2_text,
  perl = TRUE
))

stopifnot(!grepl(
  'observeEvent\\(input\\$support_threshold, \\{[\\s\\S]*recompute_phase2_shared\\(',
  phase2_text,
  perl = TRUE
))

stopifnot(grepl(
  'observeEvent\\(input\\$metric_filter, \\{[\\s\\S]*get_phase2_controls_ready\\([\\s\\S]*phase2_settings_match\\(resolved_settings, get_phase2_applied_settings\\(\\)\\)[\\s\\S]*set_phase2_programmatic_strat_update\\(strat_selection_changed\\)[\\s\\S]*updatePickerInput\\([\\s\\S]*request_consistency_refresh\\(',
  phase2_text,
  perl = TRUE
))

stopifnot(grepl(
  'observeEvent\\(input\\$strat_filter, \\{[\\s\\S]*get_phase2_controls_ready\\([\\s\\S]*get_phase2_programmatic_strat_update\\([\\s\\S]*set_phase2_programmatic_strat_update\\(FALSE\\)[\\s\\S]*request_consistency_refresh\\(',
  phase2_text,
  perl = TRUE
))

stopifnot(any(grepl('recompute_phase2_shared <- function\\(', summary_lines)))
stopifnot(any(grepl('refresh_phase2_ranking_shared <- function\\(', summary_lines)))
stopifnot(any(grepl('persist_settings = TRUE', summary_lines, fixed = TRUE)))
stopifnot(grepl(
  'recompute_phase2_shared <- function\\([\\s\\S]*if \\(isTRUE\\(persist_settings\\)\\) \\{[\\s\\S]*set_phase2_settings\\(rv, settings\\)[\\s\\S]*\\} else \\{[\\s\\S]*normalize_phase2_settings\\(rv, settings\\)',
  summary_text,
  perl = TRUE
))
stopifnot(grepl(
  'refresh_phase2_ranking_shared <- function\\([\\s\\S]*if \\(isTRUE\\(persist_settings\\)\\) \\{[\\s\\S]*set_phase2_settings\\(rv, settings\\)[\\s\\S]*\\} else \\{[\\s\\S]*normalize_phase2_settings\\(rv, settings\\)',
  summary_text,
  perl = TRUE
))

stopifnot(any(grepl('Shiny.addCustomMessageHandler("watchPhase2ResultsReady"', js_lines, fixed = TRUE)))
stopifnot(any(grepl('phase2ResultsAreReady', js_lines, fixed = TRUE)))
stopifnot(any(grepl('phase2OutputHasPlotContent', js_lines, fixed = TRUE)))
stopifnot(any(grepl('phase2OutputHasTableContent', js_lines, fixed = TRUE)))
stopifnot(grepl(
  'Shiny\\.addCustomMessageHandler\\("watchPhase2ResultsReady"[\\s\\S]*Shiny\\.setInputValue\\([\\s\\S]*message\\.readyInputId[\\s\\S]*message\\.requestId',
  js_text,
  perl = TRUE
))

stopifnot(any(grepl('phase2-consistency-shell-output\\.recalculating', css_lines)))
stopifnot(any(grepl('phase2-consistency-results-output\\.recalculating', css_lines)))
stopifnot(grepl(
  'phase2-consistency-shell-output\\.recalculating,[\\s\\S]*phase2-consistency-results-output \\.recalculating[\\s\\S]*opacity: 1;',
  css_text,
  perl = TRUE
))

cat("phase2_loading_feedback_checks: OK\n")
