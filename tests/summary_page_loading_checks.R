summary_lines <- readLines("app/modules/mod_summary_page.R", warn = FALSE)
summary_text <- paste(summary_lines, collapse = "\n")

stopifnot(any(grepl('bulk_recompute_phase <- reactiveVal\\("idle"\\)', summary_lines)))
stopifnot(any(grepl("make_progress_notifier <- function", summary_lines, fixed = TRUE)))
stopifnot(any(grepl("set_status = function\\(", summary_lines)))
stopifnot(!any(grepl("set_finalizing = function\\(", summary_lines)))
stopifnot(any(grepl("show_bulk_refresh_final_loading <- function", summary_lines, fixed = TRUE)))
stopifnot(any(grepl("remove_bulk_refresh_final_loading <- function", summary_lines, fixed = TRUE)))
stopifnot(any(grepl("show_final_loading_notification\\(", summary_lines)))
stopifnot(any(grepl("remove_final_loading_notification\\(", summary_lines)))
stopifnot(any(grepl('uiOutput\\(ns\\("bulk_refresh_status"\\)\\)', summary_lines)))

stopifnot(grepl(
  'output\\$bulk_refresh_status <- renderUI\\(\\{[\\s\\S]*tags\\$span\\("Loading page, please wait\\."\\)',
  summary_text,
  perl = TRUE
))

stopifnot(grepl(
  'run_bulk_recompute <- function\\(metrics\\) \\{[\\s\\S]*bulk_recompute_phase\\("refreshing"\\)[\\s\\S]*progress\\$close\\(\\)[\\s\\S]*show_bulk_refresh_final_loading\\([\\s\\S]*Loading page, please wait\\. Refreshing reference curve analysis table\\.[\\s\\S]*session\\$onFlushed\\(function\\(\\) \\{[\\s\\S]*remove_bulk_refresh_final_loading\\(\\)[\\s\\S]*bulk_recompute_active\\(FALSE\\)[\\s\\S]*bulk_recompute_phase\\("idle"\\)',
  summary_text,
  perl = TRUE
))

stopifnot(grepl(
  'observeEvent\\(input\\$recompute_all, \\{[\\s\\S]*build_summary_recompute_plan\\(rv, summary_metrics\\(\\)\\)[\\s\\S]*show_bulk_manual_recompute_modal\\(plan\\)',
  summary_text,
  perl = TRUE
))

stopifnot(grepl(
  'observeEvent\\(input\\$confirm_bulk_recompute, \\{[\\s\\S]*run_bulk_recompute\\(resolve_summary_recompute_metrics\\(',
  summary_text,
  perl = TRUE
))

cat("summary_page_loading_checks: OK\n")
