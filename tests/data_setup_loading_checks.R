module_lines <- readLines("app/modules/mod_data_overview.R", warn = FALSE)
module_text <- paste(module_lines, collapse = "\n")
helper_lines <- readLines("app/helpers/notifications.R", warn = FALSE)
global_lines <- readLines("app/global.R", warn = FALSE)
js_lines <- readLines("app/www/custom.js", warn = FALSE)
js_text <- paste(js_lines, collapse = "\n")
css_lines <- readLines("app/www/custom.css", warn = FALSE)
css_text <- paste(css_lines, collapse = "\n")

stopifnot(any(grepl("resolve_session_snapshot <- function\\(session_data, progress_cb = NULL\\)", module_lines)))
stopifnot(any(grepl("final_loading_notification_ui <- function", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("show_final_loading_notification <- function", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("remove_final_loading_notification <- function", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("close_button = TRUE", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("streamcurves-final-loading-close", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("closeButton = FALSE", helper_lines, fixed = TRUE)))
stopifnot(!any(grepl("streamcurves-final-loading-progress", helper_lines, fixed = TRUE)))
stopifnot(any(grepl('source\\("helpers/notifications.R", local = TRUE\\)', global_lines)))

stopifnot(any(grepl("begin_data_setup_load_progress <- function\\(total_steps, message, detail = NULL\\)", module_lines)))
stopifnot(any(grepl("schedule_data_setup_load_close <- function\\(request_id, delay_seconds = data_setup_load_final_timeout_seconds\\)", module_lines)))
stopifnot(any(grepl("show_data_setup_final_loading <- function", module_lines, fixed = TRUE)))
stopifnot(any(grepl("final_notification_id = NULL", module_lines, fixed = TRUE)))
stopifnot(any(grepl("show_final_loading_notification\\(", module_lines)))
stopifnot(any(grepl("remove_final_loading_notification\\(", module_lines)))
stopifnot(any(grepl("later::later\\(", module_lines)))

stopifnot(grepl(
  'finalize_data_setup_load_progress <- function\\(request_id,[\\s\\S]*Loading page, please wait\\. Preparing Data & Setup tab\\.[\\s\\S]*show_data_setup_final_loading\\(request_id\\)[\\s\\S]*schedule_data_setup_load_close\\(request_id\\)[\\s\\S]*session\\$onFlushed\\([\\s\\S]*"refreshMetadataAccordion"[\\s\\S]*readyInputId = ns\\("data_setup_load_ready"\\)[\\s\\S]*requestId = request_id',
  module_text,
  perl = TRUE
))

stopifnot(any(grepl("Loading page, please wait. Preparing Data & Setup tab.", module_lines, fixed = TRUE)))
stopifnot(any(grepl('readyInputId = ns\\("data_setup_load_ready"\\)', module_lines)))
stopifnot(any(grepl('observeEvent\\(input\\$data_setup_load_ready, \\{', module_lines)))
stopifnot(any(grepl('close_data_setup_load_progress\\(as.character\\(request_id\\)\\)', module_lines)))

stopifnot(grepl(
  'observeEvent\\(input\\$upload_file, \\{[\\s\\S]*begin_data_setup_load_progress\\([\\s\\S]*message = "Loading Workbook"[\\s\\S]*Loading workbook tables\\.[\\s\\S]*Cleaning uploaded data\\.[\\s\\S]*Deriving analysis variables\\.[\\s\\S]*Running pre-run validation\\.[\\s\\S]*Applying dataset to the app\\.[\\s\\S]*finalize_data_setup_load_progress\\(request_id\\)',
  module_text,
  perl = TRUE
))

stopifnot(grepl(
  'resolve_session_snapshot <- function\\(session_data, progress_cb = NULL\\)[\\s\\S]*progress_cb\\("Rebuilding derived data from workbook metadata\\.\\.\\.", value = 2L\\)[\\s\\S]*progress_cb\\("Recomputing pre-run validation from restored data\\.\\.\\.", value = 3L\\)',
  module_text,
  perl = TRUE
))

stopifnot(grepl(
  'restore_session_into_rv <- function\\(session_data, source_name = NULL, progress_cb = NULL\\)[\\s\\S]*progress_cb\\("Resolving session contents\\.\\.\\.", value = 1L\\)[\\s\\S]*progress_cb\\("Restoring saved analysis state\\.\\.\\.", value = 4L\\)',
  module_text,
  perl = TRUE
))

stopifnot(grepl(
  'observeEvent\\(input\\$load_session_file, \\{[\\s\\S]*begin_data_setup_load_progress\\([\\s\\S]*message = "Loading Session"[\\s\\S]*Reading saved session snapshot\\.[\\s\\S]*restore_session_into_rv\\([\\s\\S]*progress_cb = function\\(detail, value = NULL\\)[\\s\\S]*update_data_setup_load_progress\\(request_id, detail = detail, value = value\\)[\\s\\S]*finalize_data_setup_load_progress\\(request_id\\)',
  module_text,
  perl = TRUE
))

stopifnot(any(grepl("refreshMetadataAccordionLayout\\(containerId, onComplete\\)", js_lines)))
stopifnot(any(grepl("readyInputId", js_lines, fixed = TRUE)))
stopifnot(any(grepl("Shiny.setInputValue", js_lines, fixed = TRUE)))
stopifnot(!any(grepl("setProgressNotificationState", js_lines, fixed = TRUE)))
stopifnot(!any(grepl("removeProgressNotification", js_lines, fixed = TRUE)))
stopifnot(grepl(
  'Shiny\\.addCustomMessageHandler\\("refreshMetadataAccordion"[\\s\\S]*message\\.readyInputId[\\s\\S]*Shiny\\.setInputValue\\(',
  js_text,
  perl = TRUE
))

stopifnot(any(grepl("streamcurves-final-loading-notification", css_lines, fixed = TRUE)))
stopifnot(any(grepl("streamcurves-final-loading-spinner", css_lines, fixed = TRUE)))
stopifnot(any(grepl("streamcurves-final-loading-heading", css_lines, fixed = TRUE)))
stopifnot(any(grepl("streamcurves-final-loading-close", css_lines, fixed = TRUE)))
stopifnot(any(grepl("display: flex", css_lines, fixed = TRUE)))
stopifnot(any(grepl("width: 100%", css_lines, fixed = TRUE)))
stopifnot(any(grepl("margin: 0 0 0 auto", css_lines, fixed = TRUE)))
stopifnot(!any(grepl("streamcurves-final-loading-progress", css_lines, fixed = TRUE)))
stopifnot(!any(grepl("streamcurves-final-loading-progress-bar", css_lines, fixed = TRUE)))
stopifnot(grepl(
  'streamcurves-final-loading-notification[\\s\\S]*display: flex[\\s\\S]*width: 100%[\\s\\S]*streamcurves-final-loading-header[\\s\\S]*width: 100%[\\s\\S]*streamcurves-final-loading-heading[\\s\\S]*streamcurves-final-loading-close[\\s\\S]*margin: 0 0 0 auto[\\s\\S]*streamcurves-final-loading-detail',
  css_text,
  perl = TRUE
))

cat("data_setup_loading_checks: OK\n")
