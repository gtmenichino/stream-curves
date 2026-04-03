## -- Module: Export -----------------------------------------------------------
## Export current-session CSVs, bundles, and reports from the Reference Curves page.

library(shiny)
library(bslib)

empty_named_choices <- function() {
  stats::setNames(character(0), character(0))
}

boxplot_metric_choices <- function(metric_config) {
  metric_config <- metric_config %||% list()
  metric_keys <- names(metric_config) %||% character(0)

  if (length(metric_keys) == 0) {
    return(empty_named_choices())
  }

  metric_keys <- metric_keys[vapply(metric_keys, function(metric_key) {
    metric_entry <- metric_config[[metric_key]]
    !is.null(metric_entry) && !identical(metric_entry$metric_family %||% NULL, "categorical")
  }, logical(1))]

  if (length(metric_keys) == 0) {
    return(empty_named_choices())
  }

  stats::setNames(
    metric_keys,
    vapply(metric_keys, function(metric_key) {
      metric_config[[metric_key]]$display_name %||% metric_key
    }, character(1))
  )
}

boxplot_strat_choices <- function(rv, metric_key) {
  if (is.null(metric_key) || length(metric_key) == 0 || !nzchar(metric_key)) {
    return(empty_named_choices())
  }

  allowed <- get_metric_allowed_strats(rv, metric_key)
  if (length(allowed) == 0) {
    return(empty_named_choices())
  }

  stats::setNames(
    allowed,
    vapply(allowed, function(strat_key) {
      rv$strat_config[[strat_key]]$display_name %||% strat_key
    }, character(1))
  )
}

summary_export_output_enabled <- function(rv, key, default = TRUE) {
  cfg <- rv$output_config$summary_outputs[[key]] %||% NULL
  enabled <- cfg$enabled %||% NULL
  if (is.null(enabled)) default else isTRUE(enabled)
}

summary_export_report_enabled <- function(rv, key, default = TRUE) {
  cfg <- rv$output_config$report_products[[key]] %||% NULL
  enabled <- cfg$enabled %||% NULL
  if (is.null(enabled)) default else isTRUE(enabled)
}

summary_export_report_meta <- function(rv, key, default_file, default_format) {
  cfg <- rv$output_config$report_products[[key]] %||% list()
  report_file <- cfg$file %||% default_file
  report_path <- if (grepl("^[A-Za-z]:|^/", report_file)) {
    report_file
  } else {
    file.path(project_root, report_file)
  }

  list(
    path = report_path,
    format = cfg$format %||% default_format
  )
}

summary_export_stage_dir <- function(prefix = "summary_export") {
  file.path(
    tempdir(),
    paste0(prefix, "_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sprintf("%06d", sample.int(999999, 1)))
  )
}

summary_export_copy_file <- function(path, file) {
  if (!file.exists(path)) {
    stop("Requested export was not generated.")
  }
  ok <- file.copy(path, file, overwrite = TRUE)
  if (!isTRUE(ok)) {
    stop("Failed to prepare export file.")
  }
}

summary_export_zip_dir <- function(source_dir, file) {
  parent_dir <- dirname(source_dir)
  base_dir <- basename(source_dir)
  old_wd <- setwd(parent_dir)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(file, files = list.files(base_dir, full.names = TRUE, recursive = TRUE))
}

export_stat_card <- function(title, value, detail = NULL) {
  card(
    class = "h-100",
    card_header(title),
    card_body(
      tags$div(class = "fs-3 fw-semibold", value),
      if (!is.null(detail) && nzchar(detail)) {
        tags$p(class = "text-muted small mb-0 mt-2", detail)
      }
    )
  )
}

export_download_card <- function(title, description, control) {
  card(
    class = "h-100",
    card_header(title),
    card_body(
      tags$p(class = "mb-3", description),
      control
    )
  )
}

mod_summary_export_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("summary_page"))
}

mod_summary_export_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    export_context <- reactive({
      req(rv$data)
      build_summary_export_context(rv)
    })

    stage_exports <- function(include_appendix_plots = FALSE) {
      bundle_dir <- summary_export_stage_dir(
        if (isTRUE(include_appendix_plots)) "summary_export_appendix" else "summary_export_bundle"
      )
      write_summary_export_stage(
        rv,
        bundle_dir = bundle_dir,
        include_appendix_plots = include_appendix_plots
      )
    }

    render_report_download <- function(report_key, default_file, default_format,
                                       output_name, include_appendix_plots = FALSE) {
      output[[output_name]] <- downloadHandler(
        filename = function() {
          paste0(report_key, ".", if (identical(default_format, "html")) "html" else "pdf")
        },
        content = function(file) {
          if (!requireNamespace("quarto", quietly = TRUE)) {
            stop("The quarto package is required to render reports.")
          }

          report_meta <- summary_export_report_meta(rv, report_key, default_file, default_format)
          if (!file.exists(report_meta$path)) {
            stop("Report template not found.")
          }

          staged <- stage_exports(include_appendix_plots = include_appendix_plots)
          render_dir <- summary_export_stage_dir(paste0(report_key, "_render"))
          dir.create(render_dir, showWarnings = FALSE, recursive = TRUE)
          report_input <- file.path(render_dir, basename(report_meta$path))
          rendered_file <- file.path(render_dir, basename(file))
          file.copy(report_meta$path, report_input, overwrite = TRUE)

          quarto::quarto_render(
            input = report_input,
            output_format = report_meta$format,
            execute_params = list(
              session_dir = normalizePath(staged$bundle_dir, winslash = "/", mustWork = FALSE)
            ),
            execute_dir = render_dir,
            output_file = basename(file),
            quiet = TRUE
          )

          summary_export_copy_file(rendered_file, file)
        }
      )
    }

    output$dl_thresholds <- downloadHandler(
      filename = function() "reference_curve_thresholds.csv",
      content = function(file) {
        staged <- stage_exports()
        summary_export_copy_file(staged$files$thresholds, file)
      }
    )

    output$dl_metric_status <- downloadHandler(
      filename = function() "metric_status.csv",
      content = function(file) {
        staged <- stage_exports()
        summary_export_copy_file(staged$files$metric_status, file)
      }
    )

    output$dl_strat <- downloadHandler(
      filename = function() "stratification_decisions.csv",
      content = function(file) {
        staged <- stage_exports()
        summary_export_copy_file(staged$files$stratification_decisions, file)
      }
    )

    output$dl_decision_history <- downloadHandler(
      filename = function() "decision_history.csv",
      content = function(file) {
        staged <- stage_exports()
        summary_export_copy_file(staged$files$decision_history, file)
      }
    )

    output$dl_regional <- downloadHandler(
      filename = function() "regional_curves.csv",
      content = function(file) {
        staged <- stage_exports()
        summary_export_copy_file(staged$files$regional_curves, file)
      }
    )

    output$dl_bundle <- downloadHandler(
      filename = function() "results_bundle.zip",
      content = function(file) {
        staged <- stage_exports()
        summary_export_zip_dir(staged$bundle_dir, file)
      }
    )

    render_report_download("summary", "reports/summary.qmd", "pdf", "dl_summary_report")
    render_report_download("dashboard", "reports/dashboard.qmd", "html", "dl_dashboard_report")
    render_report_download(
      "appendix",
      "reports/appendix.qmd",
      "pdf",
      "dl_appendix_report",
      include_appendix_plots = TRUE
    )

    output$summary_page <- renderUI({
      if (is.null(rv$data)) {
        return(no_data_alert())
      }

      ctx <- export_context()
      status_cards <- list(
        export_stat_card("Summary metrics", ctx$session_meta$metric_count),
        export_stat_card("Curves ready", ctx$session_meta$complete_metrics),
        export_stat_card("Needs review", ctx$session_meta$review_metrics),
        export_stat_card("Manual curves", ctx$session_meta$manual_curve_metrics),
        export_stat_card(
          "Regional curves",
          ctx$session_meta$regional_curve_sets,
          if (ctx$session_meta$regional_curve_sets > 0) {
            paste(ctx$session_meta$regional_curve_rows, "rows available")
          } else {
            "No regional outputs in this session"
          }
        )
      )

      core_cards <- list()
      if (summary_export_output_enabled(rv, "thresholds")) {
        core_cards <- c(core_cards, list(
          export_download_card(
            "Reference Curve Thresholds",
            "CSV of current Phase 4 curve rows, including stratified and manual curves.",
            downloadButton(ns("dl_thresholds"), "Download CSV", class = "btn btn-outline-primary")
          )
        ))
      }
      if (summary_export_output_enabled(rv, "metric_status")) {
        core_cards <- c(core_cards, list(
          export_download_card(
            "Metric Status",
            "CSV of current status, curve stratification, warnings, and manual-curve flags for each summary metric.",
            downloadButton(ns("dl_metric_status"), "Download CSV", class = "btn btn-outline-primary")
          )
        ))
      }
      if (summary_export_output_enabled(rv, "stratification_decisions")) {
        core_cards <- c(core_cards, list(
          export_download_card(
            "Stratification Decisions",
            "CSV of the current selected and recommended curve stratification for each summary metric.",
            downloadButton(ns("dl_strat"), "Download CSV", class = "btn btn-outline-primary")
          )
        ))
      }
      if (summary_export_output_enabled(rv, "results_bundle")) {
        core_cards <- c(core_cards, list(
          export_download_card(
            "Full Results Bundle",
            "ZIP bundle of the current session exports, report context, metadata, and optional regional/history files.",
            downloadButton(ns("dl_bundle"), "Download ZIP", class = "btn btn-primary")
          )
        ))
      }

      report_cards <- list()
      if (summary_export_report_enabled(rv, "summary")) {
        report_cards <- c(report_cards, list(
          export_download_card(
            "PDF Summary",
            "Compact executive snapshot of current metric status, exceptions, and curve outputs.",
            downloadButton(ns("dl_summary_report"), "Download PDF", class = "btn btn-outline-secondary")
          )
        ))
      }
      if (summary_export_report_enabled(rv, "dashboard")) {
        report_cards <- c(report_cards, list(
          export_download_card(
            "HTML Dashboard",
            "Analyst-facing current-state dashboard with filterable tables for metrics, thresholds, decisions, and Phase 2 outputs.",
            downloadButton(ns("dl_dashboard_report"), "Download HTML", class = "btn btn-outline-secondary")
          )
        ))
      }
      if (summary_export_report_enabled(rv, "appendix")) {
        report_cards <- c(report_cards, list(
          export_download_card(
            "Full PDF Appendix",
            "Detailed per-metric reference-curve appendix with current threshold tables, notes, and curve plots.",
            downloadButton(ns("dl_appendix_report"), "Download PDF", class = "btn btn-outline-secondary")
          )
        ))
      }

      extra_cards <- list()
      if (summary_export_output_enabled(rv, "regional_curves") && nrow(ctx$regional_curves) > 0) {
        extra_cards <- c(extra_cards, list(
          export_download_card(
            "Regional Curves",
            "CSV of current regional curve fits completed in this session.",
            downloadButton(ns("dl_regional"), "Download CSV", class = "btn btn-outline-primary")
          )
        ))
      }
      if (summary_export_output_enabled(rv, "decision_history") && nrow(ctx$decision_history) > 0) {
        extra_cards <- c(extra_cards, list(
          export_download_card(
            "Decision History",
            "CSV of recorded user decisions. This is history only, not the authoritative current state.",
            downloadButton(ns("dl_decision_history"), "Download CSV", class = "btn btn-outline-primary")
          )
        ))
      }

      tagList(
        explanation_card(
          "Export",
          tags$p("Download outputs and reports generated from the current Shiny session."),
          tags$p("These exports no longer read legacy pipeline folders or ", tags$code("outputs/run_latest"), ".")
        ),
        card(
          card_header("Session Status"),
          card_body(
            do.call(layout_column_wrap, c(list(width = 1 / 5), status_cards))
          )
        ),
        if (length(core_cards) > 0) {
          card(
            card_header("Core Downloads"),
            card_body(
              do.call(layout_column_wrap, c(list(width = 1 / 2), core_cards))
            )
          )
        },
        if (length(report_cards) > 0) {
          card(
            card_header("Reports"),
            card_body(
              do.call(layout_column_wrap, c(list(width = 1 / 3), report_cards))
            )
          )
        },
        if (length(extra_cards) > 0) {
          card(
            card_header("Additional Exports"),
            card_body(
              do.call(layout_column_wrap, c(list(width = 1 / 2), extra_cards))
            )
          )
        }
      )
    })
  })
}
