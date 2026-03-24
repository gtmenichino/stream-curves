## ── Module: Export (Page 7) ───────────────────────────────────────────────────
## Download results as CSV, decision log, ZIP bundles, Quarto reports.

library(shiny)
library(bslib)

mod_export_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Export Results",
      p("Download analysis results for completed metrics. All files are generated
         from the current session's completed metrics and decision log.")
    ),

    card(
      card_header("Available Downloads"),
      card_body(
        layout_column_wrap(
          width = 1 / 3,

          card(
            card_header("Reference Curve Thresholds"),
            card_body(
              p("CSV table of Q25/Q75 thresholds and functional category boundaries
                 for all completed standard metrics."),
              downloadButton(ns("dl_thresholds"), "Download CSV",
                             class = "btn btn-outline-primary")
            )
          ),

          card(
            card_header("Stratification Decisions"),
            card_body(
              p("CSV table of stratification decisions for all completed metrics,
                 showing selected variable, p-value, and group sizes."),
              downloadButton(ns("dl_strat"), "Download CSV",
                             class = "btn btn-outline-primary")
            )
          ),

          card(
            card_header("Model Selections"),
            card_body(
              p("CSV table of selected models with predictors, fit statistics,
                 and diagnostic status for all completed metrics."),
              downloadButton(ns("dl_models"), "Download CSV",
                             class = "btn btn-outline-primary")
            )
          ),

          card(
            card_header("Regional Curves"),
            card_body(
              p("CSV table of power-function coefficients and R\u00b2 for
                 all completed regional/hydraulic geometry curves."),
              downloadButton(ns("dl_regional"), "Download CSV",
                             class = "btn btn-outline-primary")
            )
          ),

          card(
            card_header("Decision Log"),
            card_body(
              p("CSV of all stratification and model selection decisions,
                 including layer scores, rationale, and reviewer notes."),
              downloadButton(ns("dl_decision_log"), "Download CSV",
                             class = "btn btn-outline-primary")
            )
          ),

          card(
            card_header("Full Results Bundle"),
            card_body(
              p("ZIP archive containing all CSVs, decision log, session info,
                 and summary of all completed metrics."),
              downloadButton(ns("dl_bundle"), "Download ZIP",
                             class = "btn btn-primary")
            )
          )
        )
      )
    ),

    ## ── Quarto report generation ────────────────────────────────────────────
    card(
      card_header("Report Generation"),
      card_body(
        p(class = "text-muted", "Generate formatted reports from existing Quarto
           templates. Requires Quarto to be installed."),
        layout_column_wrap(
          width = 1 / 3,
          actionButton(ns("gen_summary_pdf"), "Generate PDF Summary",
                       class = "btn btn-outline-secondary", icon = icon("file-pdf")),
          actionButton(ns("gen_dashboard"), "Generate HTML Dashboard",
                       class = "btn btn-outline-secondary", icon = icon("chart-bar")),
          actionButton(ns("gen_appendix"), "Generate Full Appendix",
                       class = "btn btn-outline-secondary", icon = icon("book"))
        ),
        uiOutput(ns("report_status"))
      )
    ),

    ## ── Session summary ─────────────────────────────────────────────────────
    card(
      card_header("Session Summary"),
      card_body(
        p("Save/load sessions from the ", tags$strong("Data & Setup"), " tab."),
        textOutput(ns("session_summary"))
      )
    )
  )
}

mod_export_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## Helpers
    standard_metrics <- reactive({
      completed <- rv$completed_metrics
      purrr::keep(completed, ~ is.null(.x$type) || .x$type != "regional")
    })

    regional_metrics <- reactive({
      completed <- rv$completed_metrics
      purrr::keep(completed, ~ !is.null(.x$type) && .x$type == "regional")
    })

    ## ── Reference curve thresholds ──────────────────────────────────────────
    output$dl_thresholds <- downloadHandler(
      filename = function() paste0("reference_thresholds_", format(Sys.time(), "%Y%m%d"), ".csv"),
      content = function(file) {
        std <- standard_metrics()
        if (length(std) == 0) {
          write.csv(data.frame(message = "No completed metrics"), file, row.names = FALSE)
          return()
        }
        rows <- dplyr::bind_rows(lapply(std, function(entry) {
          if (!is.null(entry$reference_curve)) entry$reference_curve$curve_row else NULL
        }))
        write.csv(rows, file, row.names = FALSE)
      }
    )

    ## ── Stratification decisions ────────────────────────────────────────────
    output$dl_strat <- downloadHandler(
      filename = function() paste0("strat_decisions_", format(Sys.time(), "%Y%m%d"), ".csv"),
      content = function(file) {
        std <- standard_metrics()
        if (length(std) == 0) {
          write.csv(data.frame(message = "No completed metrics"), file, row.names = FALSE)
          return()
        }
        rows <- dplyr::bind_rows(lapply(std, function(entry) entry$strat_decision))
        write.csv(rows, file, row.names = FALSE)
      }
    )

    ## ── Model selections ────────────────────────────────────────────────────
    output$dl_models <- downloadHandler(
      filename = function() paste0("model_selections_", format(Sys.time(), "%Y%m%d"), ".csv"),
      content = function(file) {
        std <- standard_metrics()
        if (length(std) == 0) {
          write.csv(data.frame(message = "No completed metrics"), file, row.names = FALSE)
          return()
        }
        model_rows <- dplyr::bind_rows(lapply(std, function(entry) entry$model_selection))
        diag_rows <- dplyr::bind_rows(lapply(std, function(entry) {
          if (!is.null(entry$diagnostics)) entry$diagnostics$summary_row else NULL
        }))

        if (nrow(model_rows) > 0 && nrow(diag_rows) > 0) {
          combined <- dplyr::left_join(model_rows, diag_rows, by = "metric")
          write.csv(combined, file, row.names = FALSE)
        } else {
          write.csv(model_rows, file, row.names = FALSE)
        }
      }
    )

    ## ── Regional curves ─────────────────────────────────────────────────────
    output$dl_regional <- downloadHandler(
      filename = function() paste0("regional_curves_", format(Sys.time(), "%Y%m%d"), ".csv"),
      content = function(file) {
        reg <- regional_metrics()
        if (length(reg) == 0) {
          write.csv(data.frame(message = "No completed regional curves"), file, row.names = FALSE)
          return()
        }
        rows <- dplyr::bind_rows(lapply(reg, function(entry) entry$model_summary))
        write.csv(rows, file, row.names = FALSE)
      }
    )

    ## ── Decision log ────────────────────────────────────────────────────────
    output$dl_decision_log <- downloadHandler(
      filename = function() paste0("decision_log_", format(Sys.time(), "%Y%m%d"), ".csv"),
      content = function(file) {
        log <- rv$decision_log
        if (is.null(log) || nrow(log) == 0) {
          write.csv(data.frame(message = "No decisions recorded"), file, row.names = FALSE)
        } else {
          write.csv(log, file, row.names = FALSE)
        }
      }
    )

    ## ── Full bundle ─────────────────────────────────────────────────────────
    output$dl_bundle <- downloadHandler(
      filename = function() paste0("results_bundle_", format(Sys.time(), "%Y%m%d"), ".zip"),
      content = function(file) {
        tmpdir <- tempdir()
        bundle_dir <- file.path(tmpdir, "analysis_results")
        dir.create(bundle_dir, showWarnings = FALSE, recursive = TRUE)

        std <- standard_metrics()
        reg <- regional_metrics()

        if (length(std) > 0) {
          thresholds <- dplyr::bind_rows(lapply(std, function(e) {
            if (!is.null(e$reference_curve)) e$reference_curve$curve_row else NULL
          }))
          write.csv(thresholds, file.path(bundle_dir, "reference_thresholds.csv"), row.names = FALSE)

          strat_decs <- dplyr::bind_rows(lapply(std, function(e) e$strat_decision))
          write.csv(strat_decs, file.path(bundle_dir, "strat_decisions.csv"), row.names = FALSE)

          model_sels <- dplyr::bind_rows(lapply(std, function(e) e$model_selection))
          write.csv(model_sels, file.path(bundle_dir, "model_selections.csv"), row.names = FALSE)
        }

        if (length(reg) > 0) {
          reg_rows <- dplyr::bind_rows(lapply(reg, function(e) e$model_summary))
          write.csv(reg_rows, file.path(bundle_dir, "regional_curves.csv"), row.names = FALSE)
        }

        ## Decision log
        log <- rv$decision_log
        if (!is.null(log) && nrow(log) > 0) {
          write.csv(log, file.path(bundle_dir, "decision_log.csv"), row.names = FALSE)
        }

        ## Session info
        writeLines(capture.output(sessionInfo()),
                    file.path(bundle_dir, "session_info.txt"))

        ## Summary
        writeLines(
          c(
            paste0("Analysis Results - ", Sys.time()),
            paste0("Standard metrics completed: ", length(std)),
            paste0("Regional curves completed: ", length(reg)),
            paste0("Decision log entries: ", if (!is.null(log)) nrow(log) else 0),
            paste0("Metrics: ", paste(names(rv$completed_metrics), collapse = ", "))
          ),
          file.path(bundle_dir, "summary.txt")
        )

        old_wd <- setwd(tmpdir)
        on.exit(setwd(old_wd))
        zip(file, files = list.files("analysis_results", full.names = TRUE, recursive = TRUE))
      }
    )

    ## ── Quarto report generation ────────────────────────────────────────────
    report_msg <- reactiveVal(NULL)

    observeEvent(input$gen_summary_pdf, {
      report_path <- file.path(project_root, "reports", "summary.qmd")
      if (!file.exists(report_path)) {
        report_msg("Summary template not found.")
        return()
      }
      tryCatch({
        quarto::quarto_render(report_path, output_format = "pdf")
        report_msg("PDF summary generated successfully.")
      }, error = function(e) {
        report_msg(paste0("Report generation failed: ", e$message))
      })
    })

    observeEvent(input$gen_dashboard, {
      report_path <- file.path(project_root, "reports", "dashboard.qmd")
      if (!file.exists(report_path)) {
        report_msg("Dashboard template not found.")
        return()
      }
      tryCatch({
        quarto::quarto_render(report_path, output_format = "html")
        report_msg("HTML dashboard generated successfully.")
      }, error = function(e) {
        report_msg(paste0("Report generation failed: ", e$message))
      })
    })

    observeEvent(input$gen_appendix, {
      report_path <- file.path(project_root, "reports", "appendix.qmd")
      if (!file.exists(report_path)) {
        report_msg("Appendix template not found.")
        return()
      }
      tryCatch({
        quarto::quarto_render(report_path, output_format = "pdf")
        report_msg("Full appendix generated successfully.")
      }, error = function(e) {
        report_msg(paste0("Report generation failed: ", e$message))
      })
    })

    output$report_status <- renderUI({
      msg <- report_msg()
      if (is.null(msg)) return(NULL)
      div(class = "alert alert-info mt-2", msg)
    })

    ## ── Session summary ─────────────────────────────────────────────────────
    output$session_summary <- renderText({
      n_completed <- length(rv$completed_metrics)
      n_decisions <- if (!is.null(rv$decision_log)) nrow(rv$decision_log) else 0
      paste0(n_completed, " metrics completed, ", n_decisions, " decisions recorded.")
    })
  })
}
