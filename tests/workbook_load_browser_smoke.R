suppressPackageStartupMessages({})

if (!requireNamespace("shinytest2", quietly = TRUE)) {
  cat("workbook_load_browser_smoke: SKIPPED (shinytest2 not installed)\n")
  quit(save = "no", status = 0L)
}

if (!requireNamespace("chromote", quietly = TRUE) ||
    !requireNamespace("processx", quietly = TRUE) ||
    !requireNamespace("httpuv", quietly = TRUE)) {
  cat("workbook_load_browser_smoke: SKIPPED (chromote/processx/httpuv not installed)\n")
  quit(save = "no", status = 0L)
}

chrome_candidates <- c(
  "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
  "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
  "C:/Program Files/Google/Chrome/Application/chrome.exe",
  "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
)
chrome_path <- chrome_candidates[file.exists(chrome_candidates)][1]

if (is.na(chrome_path) || !nzchar(chrome_path)) {
  cat("workbook_load_browser_smoke: SKIPPED (no Chrome/Edge executable found)\n")
  quit(save = "no", status = 0L)
}

Sys.setenv(NOT_CRAN = "true", CHROMOTE_CHROME = chrome_path, CHROMOTE_HEADLESS = "new")
options(chromote.timeout = 60)

wait_until <- function(predicate, timeout_sec = 120, interval_sec = 1, label = "condition") {
  start_time <- Sys.time()
  repeat {
    result <- tryCatch(predicate(), error = function(e) FALSE)
    if (isTRUE(result)) {
      return(invisible(TRUE))
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed >= timeout_sec) {
      stop(sprintf("Timed out waiting for %s after %s seconds.", label, timeout_sec), call. = FALSE)
    }

    Sys.sleep(interval_sec)
  }
}

debug_port <- httpuv::randomPort()
profile_dir <- tempfile("workbook-load-smoke-profile-")
dir.create(profile_dir, recursive = TRUE, showWarnings = FALSE)

browser_proc <- processx::process$new(
  command = chrome_path,
  args = c(
    "--headless=new",
    paste0("--remote-debugging-port=", debug_port),
    paste0("--user-data-dir=", normalizePath(profile_dir, winslash = "/", mustWork = FALSE)),
    "--no-first-run",
    "--no-default-browser-check",
    "about:blank"
  ),
  cleanup = TRUE,
  cleanup_tree = TRUE
)

on.exit({
  if (browser_proc$is_alive()) {
    browser_proc$kill()
  }
}, add = TRUE)

wait_until(
  function() {
    url <- sprintf("http://127.0.0.1:%s/json/version", debug_port)
    con <- url(url, open = "r")
    on.exit(close(con), add = TRUE)
    length(readLines(con, warn = FALSE)) > 0
  },
  timeout_sec = 30,
  interval_sec = 1,
  label = "browser debugging port"
)

chromote::set_default_chromote_object(
  chromote::Chromote$new(
    browser = chromote::ChromeRemote$new("127.0.0.1", debug_port)
  )
)

app_dir <- normalizePath("app", winslash = "/", mustWork = TRUE)
workbook_path <- normalizePath(".local/test_workbook.xlsx", winslash = "/", mustWork = TRUE)

app <- shinytest2::AppDriver$new(
  app_dir = app_dir,
  name = "workbook_load_browser_smoke",
  load_timeout = 120000
)
on.exit(app$stop(), add = TRUE)

current_inputs <- function() {
  app$get_values()$input
}

set_input_value <- function(id, value) {
  params <- c(stats::setNames(list(value), id), list(wait_ = FALSE))
  do.call(app$set_inputs, params)
}

coalesce_null <- function(value, default = NULL) {
  if (is.null(value)) default else value
}

upload_input_file <- function(id, path) {
  if (!is.function(app$upload_file)) {
    stop("This shinytest2 build does not expose app$upload_file().", call. = FALSE)
  }

  do.call(app$upload_file, stats::setNames(list(path), id))
  invisible(NULL)
}

loading_workbook_count <- function() {
  html <- app$get_html("body")
  if (!nzchar(html)) {
    return(0L)
  }

  as.integer(grepl("Loading Workbook", html, fixed = TRUE))
}

dom_exists <- function(id) {
  html <- app$get_html("body")
  isTRUE(grepl(id, html, fixed = TRUE))
}

dom_has_text <- function(text) {
  html <- app$get_html("body")
  isTRUE(grepl(text, html, fixed = TRUE))
}

dom_has_class <- function(id, class_name) {
  js_id <- gsub("(['\\\\])", "\\\\\\1", id, perl = TRUE)
  js_class <- gsub("(['\\\\])", "\\\\\\1", class_name, perl = TRUE)

  isTRUE(app$get_js(sprintf(
    "var el = document.getElementById('%s'); return !!(el && el.classList && el.classList.contains('%s'));",
    js_id,
    js_class
  )))
}

upload_input_file("data_overview-upload_file", workbook_path)
app$wait_for_idle(timeout = 120000)

wait_until(
  function() loading_workbook_count() == 0L,
  timeout_sec = 30,
  label = "Loading Workbook notification to clear"
)

wait_until(
  function() identical(coalesce_null(current_inputs()[["main_navbar"]], NULL), "Data & Setup"),
  timeout_sec = 30,
  label = "Data & Setup tab to remain active after upload"
)

set_input_value("main_navbar", "Reference Curves")

wait_until(
  function() "summary-open_analysis_perRiffle" %in% names(current_inputs()),
  timeout_sec = 60,
  label = "Reference Curves analysis controls"
)

app$click("summary-open_analysis_perRiffle", wait_ = FALSE)

wait_until(
  function() dom_exists("analysis-analysis_tabs"),
  timeout_sec = 60,
  label = "Analysis popup tabs"
)

wait_until(
  function() dom_exists("analysis-phase1-phase1_page") &&
    dom_exists("analysis-phase2-consistency_ui") &&
    dom_exists("analysis-phase3-phase3_page") &&
    dom_exists("analysis-phase4-phase4_page"),
  timeout_sec = 120,
  label = "Analysis popup phase content"
)

wait_until(
  function() dom_has_text("workspace-phase-body") &&
    dom_has_text("Metric Settings"),
  timeout_sec = 120,
  label = "Analysis popup rendered body content"
)

wait_until(
  function() !dom_has_class("analysis-phase1-phase1_page", "recalculating") &&
    !dom_has_class("analysis-phase3-phase3_page", "recalculating") &&
    !dom_has_class("analysis-phase4-phase4_page", "recalculating"),
  timeout_sec = 120,
  label = "Analysis popup dynamic phase pages to finish recalculating"
)

cat("workbook_load_browser_smoke: OK\n")
