## ── StreamCurves Geomorphic Reference Curve Analysis ──────────────────────
## run_all.R — Top-level entry point
##
## Usage:
##   source("run_all.R")                        # Run all metrics
##   source("R/run_pipeline.R")                  # Load pipeline, then:
##   run_pipeline(metrics = "perRiffle")         # Run single metric

## Source the pipeline orchestrator
source("R/run_pipeline.R")

## ── User-configurable settings ──────────────────────────────────────────────
n_cores <- 1L  # Sequential execution (parallel overhead not worth it for this dataset size)

## Execute full pipeline
run_dir <- run_pipeline(
  config_dir = "config",
  data_dir   = "data",
  output_dir = "outputs",
  metrics    = NULL,
  seed       = 42,
  n_cores    = n_cores
)

## ── Render reports ──────────────────────────────────────────────────────────
if (!is.null(run_dir) &&
    requireNamespace("quarto", quietly = TRUE) &&
    !is.null(quarto::quarto_path())) {

  cli::cli_h1("Rendering Reports")

  ## Create reports subdirectory inside the run output folder
  run_reports_dir <- file.path(run_dir, "reports")
  dir.create(run_reports_dir, showWarnings = FALSE, recursive = TRUE)

  report_specs <- list(
    list(file = "reports/dashboard.qmd", label = "Dashboard (HTML)", out_name = "dashboard.html"),
    list(file = "reports/summary.qmd",   label = "Summary (PDF)",    out_name = "summary.pdf"),
    list(file = "reports/poster.qmd",    label = "Poster 24x36 (PDF)", out_name = "poster.pdf"),
    list(file = "reports/appendix.qmd",  label = "Appendix (PDF)",   out_name = "appendix.pdf")
  )

  ## Filter to existing templates
  report_specs <- purrr::keep(report_specs, ~ file.exists(.x$file))

  if (n_cores > 1L && length(report_specs) > 0 &&
      requireNamespace("furrr", quietly = TRUE) &&
      requireNamespace("future", quietly = TRUE)) {

    future::plan(future::multisession, workers = min(n_cores, length(report_specs)))

    render_results <- furrr::future_map(report_specs, function(spec) {
      tryCatch({
        quarto::quarto_render(
          input = spec$file,
          execute_params = list(run_dir = run_dir)
        )
        rendered <- file.path("reports", spec$out_name)
        if (file.exists(rendered)) {
          file.copy(rendered, file.path(run_reports_dir, spec$out_name), overwrite = TRUE)
          file.remove(rendered)
        }
        list(label = spec$label, ok = TRUE, msg = "")
      }, error = function(e) {
        list(label = spec$label, ok = FALSE, msg = e$message)
      })
    })

    future::plan(future::sequential)

    for (r in render_results) {
      if (r$ok) {
        cli::cli_alert_success("{r$label} complete")
      } else {
        cli::cli_alert_danger("{r$label} failed: {r$msg}")
        if (grepl("latex|tinytex|pdf", tolower(r$msg))) {
          cli::cli_alert_info("  Tip: install LaTeX via quarto install tinytex")
        }
      }
    }
  } else {
    for (spec in report_specs) {
      tryCatch({
        cli::cli_alert_info("Rendering {spec$label}...")
        quarto::quarto_render(
          input = spec$file,
          execute_params = list(run_dir = run_dir)
        )
        rendered <- file.path("reports", spec$out_name)
        if (file.exists(rendered)) {
          file.copy(rendered, file.path(run_reports_dir, spec$out_name), overwrite = TRUE)
          file.remove(rendered)
        }
        cli::cli_alert_success("{spec$label} complete")
      }, error = function(e) {
        cli::cli_alert_danger("{spec$label} failed: {e$message}")
        if (grepl("latex|tinytex|pdf", tolower(e$message))) {
          cli::cli_alert_info("  Tip: install LaTeX via quarto install tinytex")
        }
      })
    }
  }
} else if (is.null(run_dir)) {
  cli::cli_alert_warning("Pipeline did not complete — skipping report rendering")
} else {
  cli::cli_alert_warning("Quarto not available — skipping report rendering")
  cli::cli_alert_info("  Install: https://quarto.org/docs/get-started/")
}
