suppressPackageStartupMessages({})

project_root <- normalizePath(".", winslash = "/")

source("tests/local_workbook_helper.R", local = TRUE)

if (!requireNamespace("shinytest2", quietly = TRUE)) {
  cat("phase1_quick_mask_browser_smoke: SKIPPED (shinytest2 not installed)\n")
  quit(save = "no", status = 0L)
}

if (!requireNamespace("chromote", quietly = TRUE) ||
    !requireNamespace("processx", quietly = TRUE) ||
    !requireNamespace("httpuv", quietly = TRUE)) {
  cat("phase1_quick_mask_browser_smoke: SKIPPED (chromote/processx/httpuv not installed)\n")
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
  cat("phase1_quick_mask_browser_smoke: SKIPPED (no Chrome/Edge executable found)\n")
  quit(save = "no", status = 0L)
}

Sys.setenv(NOT_CRAN = "true", CHROMOTE_CHROME = chrome_path, CHROMOTE_HEADLESS = "new")
options(chromote.timeout = 60)

debug_port <- httpuv::randomPort()
profile_dir <- tempfile("phase1-quick-mask-smoke-profile-")
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

app_dir <- normalizePath("app", winslash = "/", mustWork = TRUE)
workbook_path <- require_streamcurves_test_workbook("phase1_quick_mask_browser_smoke", project_root = project_root)

app <- shinytest2::AppDriver$new(
  app_dir = app_dir,
  name = "phase1_quick_mask_browser_smoke",
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

wait_for_stable_values <- function(id, expected, timeout_sec = 20) {
  normalized_expected <- sort(as.character(expected))
  wait_until(
    function() identical(sort(as.character(current_inputs()[[id]] %||% character(0))), normalized_expected),
    timeout_sec = timeout_sec,
    label = paste("stable values for", id)
  )
  for (idx in seq_len(3L)) {
    Sys.sleep(2)
    app$wait_for_idle(timeout = timeout_sec * 1000)
    stopifnot(identical(sort(as.character(current_inputs()[[id]] %||% character(0))), normalized_expected))
  }
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

select_options <- function(id) {
  js_id <- gsub("'", "\\\\'", id, fixed = TRUE)
  app$get_js(sprintf(
    paste0(
      "(function(){",
      "var el = document.getElementById('%s');",
      "if (!el) { return null; }",
      "return Array.from(el.options).map(function(opt) { return opt.value; });",
      "})()"
    ),
    js_id
  ))
}

plotly_point_count <- function(id) {
  js_id <- gsub("'", "\\\\'", id, fixed = TRUE)
  app$get_js(sprintf(
    paste0(
      "(function(){",
      "var gd = document.getElementById('%s');",
      " if (!gd || !gd.data) { return null; }",
      " return gd.data.reduce(function(total, trace) {",
      "   var x = Array.isArray(trace.x) ? trace.x.length : 0;",
      "   return total + x;",
      " }, 0);",
      "})()"
    ),
    js_id
  ))
}

plotly_first_site_id <- function(id) {
  js_id <- gsub("'", "\\\\'", id, fixed = TRUE)
  app$get_js(sprintf(
    paste0(
      "(function(){",
      "var gd = document.getElementById('%s');",
      " if (!gd || !gd.data) { return null; }",
      " for (var i = 0; i < gd.data.length; i += 1) {",
      "   var texts = gd.data[i].text;",
      "   if (!texts) { continue; }",
      "   var arr = Array.isArray(texts) ? texts : [texts];",
      "   for (var j = 0; j < arr.length; j += 1) {",
      "     var match = /Site ID:\\s*(\\d+)/.exec(String(arr[j]));",
      "     if (match) { return match[1]; }",
      "   }",
      " }",
      " return null;",
      "})()"
    ),
    js_id
  ))
}

plot_image_src <- function(id) {
  js_id <- gsub("'", "\\\\'", id, fixed = TRUE)
  app$get_js(sprintf(
    paste0(
      "(function(){",
      "var el = document.getElementById('%s');",
      " if (!el) { return null; }",
      " var img = el.querySelector('img');",
      " return img ? img.getAttribute('src') : null;",
      "})()"
    ),
    js_id
  ))
}

ensure_scatter_points <- function(compare_id, plot_id) {
  candidate_metrics <- unique(c(current_inputs()[[compare_id]], select_options(compare_id)))
  chosen_metric <- NULL

  for (candidate in candidate_metrics) {
    if (is.null(candidate) || !nzchar(candidate)) {
      next
    }

    if (!identical(current_inputs()[[compare_id]], candidate)) {
      set_input_value(compare_id, candidate)
      app$wait_for_idle(timeout = 30000)
      wait_for_stable_value(compare_id, candidate)
    }

    points_ready <- tryCatch({
      wait_until(
        function() {
          count <- plotly_point_count(plot_id)
          !is.null(count) && isTRUE(count > 0)
        },
        timeout_sec = 30,
        label = paste("scatter points for", candidate)
      )
      TRUE
    }, error = function(e) FALSE)

    if (isTRUE(points_ready)) {
      chosen_metric <- candidate
      break
    }
  }

  stopifnot(!is.null(chosen_metric), nzchar(chosen_metric))
  invisible(chosen_metric)
}

upload_input_file("data_overview-upload_file", workbook_path)
app$wait_for_idle(timeout = 120000)

set_input_value("main_navbar", "Reference Curves")
app$wait_for_idle(timeout = 30000)
wait_for_input("summary-open_analysis_perRiffle", timeout_sec = 60)

app$click("summary-open_analysis_perRiffle", wait_ = FALSE)
app$wait_for_idle(timeout = 120000)

set_input_value("analysis-analysis_tabs", "exploratory")
app$wait_for_idle(timeout = 30000)

compare_id <- "analysis-phase1-scatter_panel_compare_panel_1"
quick_mask_id <- "analysis-phase1-phase1_quick_mask_sites"
scatter_plot_id <- "analysis-phase1-scatter_panel_plot_panel_1"
hist_plot_id <- "analysis-phase1-precheck_hist"

wait_for_input(compare_id, timeout_sec = 120)
wait_for_input(quick_mask_id, timeout_sec = 120)

ensure_scatter_points(compare_id, scatter_plot_id)

wait_until(
  function() {
    count <- plotly_point_count(scatter_plot_id)
    !is.null(count) && isTRUE(count > 0)
  },
  timeout_sec = 60,
  label = "scatter plot to render points"
)

wait_until(
  function() {
    src <- plot_image_src(hist_plot_id)
    !is.null(src) && nzchar(src)
  },
  timeout_sec = 60,
  label = "histogram image to render"
)

baseline_point_count <- plotly_point_count(scatter_plot_id)
baseline_hist_src <- plot_image_src(hist_plot_id)
masked_site_id <- plotly_first_site_id(scatter_plot_id)

stopifnot(!is.null(baseline_point_count), baseline_point_count > 0)
stopifnot(!is.null(baseline_hist_src), nzchar(baseline_hist_src))
stopifnot(!is.null(masked_site_id), nzchar(masked_site_id))

set_input_value(quick_mask_id, masked_site_id)
app$wait_for_idle(timeout = 30000)
wait_for_stable_values(quick_mask_id, masked_site_id)

wait_until(
  function() {
    count <- plotly_point_count(scatter_plot_id)
    !is.null(count) && isTRUE(count < baseline_point_count)
  },
  timeout_sec = 60,
  label = "scatter plot to refresh after temporary site hide"
)

wait_until(
  function() {
    src <- plot_image_src(hist_plot_id)
    !is.null(src) && nzchar(src) && !identical(src, baseline_hist_src)
  },
  timeout_sec = 60,
  label = "histogram to refresh after temporary site hide"
)

cat("phase1_quick_mask_browser_smoke: OK\n")
