suppressPackageStartupMessages({})

project_root <- normalizePath(".", winslash = "/")

source("tests/local_workbook_helper.R", local = TRUE)

if (!requireNamespace("shinytest2", quietly = TRUE)) {
  cat("phase1_scatter_browser_stability_check: SKIPPED (shinytest2 not installed)\n")
  quit(save = "no", status = 0L)
}

chrome_candidates <- c(
  "C:/Program Files/Google/Chrome/Application/chrome.exe",
  "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
  "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
  "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
)
chrome_path <- chrome_candidates[file.exists(chrome_candidates)][1]

if (is.na(chrome_path) || !nzchar(chrome_path)) {
  cat("phase1_scatter_browser_stability_check: SKIPPED (no Chrome/Edge executable found)\n")
  quit(save = "no", status = 0L)
}

Sys.setenv(NOT_CRAN = "true", CHROMOTE_CHROME = chrome_path)

app_dir <- normalizePath("app", winslash = "/", mustWork = TRUE)
workbook_path <- require_streamcurves_test_workbook("phase1_scatter_browser_stability_check", project_root = project_root)

app <- shinytest2::AppDriver$new(
  app_dir = app_dir,
  name = "phase1_scatter_browser_stability",
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

upload_input_file <- function(id, path) {
  if (!is.function(app$upload_file)) {
    stop("This shinytest2 build does not expose app$upload_file().", call. = FALSE)
  }

  do.call(app$upload_file, stats::setNames(list(path), id))
  invisible(NULL)
}

wait_until <- function(predicate, timeout_sec = 120, interval_sec = 1, label = "condition") {
  start_time <- Sys.time()
  repeat {
    if (isTRUE(predicate())) {
      return(invisible(TRUE))
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed >= timeout_sec) {
      stop(sprintf("Timed out waiting for %s after %s seconds.", label, timeout_sec), call. = FALSE)
    }

    Sys.sleep(interval_sec)
  }
}

wait_for_input <- function(id, timeout_sec = 120) {
  wait_until(
    function() id %in% names(current_inputs()),
    timeout_sec = timeout_sec,
    label = paste("input", id)
  )
}

wait_for_stable_value <- function(id, expected, timeout_sec = 20) {
  wait_until(
    function() identical(current_inputs()[[id]], expected),
    timeout_sec = timeout_sec,
    label = paste("stable value for", id)
  )
  for (idx in seq_len(3L)) {
    Sys.sleep(2)
    app$wait_for_idle(timeout = timeout_sec * 1000)
    stopifnot(identical(current_inputs()[[id]], expected))
  }
}

select_options <- function(id) {
  js_id <- gsub("'", "\\\\'", id, fixed = TRUE)
  app$get_js(sprintf(
    "var el = document.getElementById('%s'); if (!el) { return null; } return Array.from(el.options).map(function(opt) { return opt.value; });",
    js_id
  ))
}

upload_input_file("data_overview-upload_file", workbook_path)
app$wait_for_idle(timeout = 120000)

set_input_value("main_navbar", "Reference Curves")
app$wait_for_idle(timeout = 30000)
wait_for_input("summary-open_analysis_perRiffle", timeout_sec = 60)

app$click("summary-open_analysis_perRiffle")
app$wait_for_idle(timeout = 120000)

set_input_value("analysis-analysis_tabs", "exploratory")
app$wait_for_idle(timeout = 30000)

compare_id <- "analysis-phase1-scatter_panel_compare_panel_1"
strat_id <- "analysis-phase1-scatter_panel_strat_key_panel_1"
toggle_id <- "analysis-phase1-scatter_panel_toggle_panel_1"
tabs_id <- "analysis-phase1-scatter_panel_tabs"
add_panel_id <- "analysis-phase1-add_scatter_panel"

wait_for_input(compare_id, timeout_sec = 120)
wait_for_input(strat_id, timeout_sec = 120)
wait_for_input(toggle_id, timeout_sec = 120)

compare_options <- select_options(compare_id)
current_compare <- current_inputs()[[compare_id]]
target_compare <- compare_options[compare_options != current_compare][[1]]
stopifnot(!is.null(target_compare), nzchar(target_compare))

set_input_value(compare_id, target_compare)
app$wait_for_idle(timeout = 30000)
wait_for_stable_value(compare_id, target_compare)

strat_options <- setdiff(select_options(strat_id), "none")
if (length(strat_options) > 0L) {
  target_strat <- strat_options[[1]]
  set_input_value(strat_id, target_strat)
  app$wait_for_idle(timeout = 30000)
  wait_for_stable_value(strat_id, target_strat)
}

set_input_value(toggle_id, FALSE)
app$wait_for_idle(timeout = 30000)
wait_for_stable_value(toggle_id, FALSE)

app$click(add_panel_id)
app$wait_for_idle(timeout = 30000)
wait_for_stable_value(tabs_id, "panel_2")

compare_id_panel_2 <- "analysis-phase1-scatter_panel_compare_panel_2"
wait_for_input(compare_id_panel_2, timeout_sec = 120)

compare_options_panel_2 <- select_options(compare_id_panel_2)
current_compare_panel_2 <- current_inputs()[[compare_id_panel_2]]
target_compare_panel_2 <- compare_options_panel_2[compare_options_panel_2 != current_compare_panel_2][[1]]
stopifnot(!is.null(target_compare_panel_2), nzchar(target_compare_panel_2))

set_input_value(compare_id_panel_2, target_compare_panel_2)
app$wait_for_idle(timeout = 30000)
wait_for_stable_value(compare_id_panel_2, target_compare_panel_2)

set_input_value(tabs_id, "panel_1")
app$wait_for_idle(timeout = 30000)
wait_for_stable_value(tabs_id, "panel_1")
wait_for_stable_value(compare_id, target_compare)

set_input_value(tabs_id, "panel_2")
app$wait_for_idle(timeout = 30000)
wait_for_stable_value(tabs_id, "panel_2")
wait_for_stable_value(compare_id_panel_2, target_compare_panel_2)

cat("phase1_scatter_browser_stability_check: OK\n")
