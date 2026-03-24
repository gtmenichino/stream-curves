## ── Module: Summary / Export (merged) ─────────────────────────────────────────────
## 3 pills: Overview | Export | Details
## Absorbs mod_summary.R, mod_export.R, and mod_appendix.R.

library(shiny)
library(bslib)
library(DT)

mod_summary_export_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("summary_page"))
}

mod_summary_export_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Data gate: show alert or full page ────────────────────────────────────
    output$summary_page <- renderUI({
      if (is.null(rv$data)) return(no_data_alert())

      navset_pill(
        ## ── Pill 1: Overview ────────────────────────────────────────────────────
        nav_panel(
          "Overview",
          icon = bsicons::bs_icon("clipboard-data"),

          card(card_header("Metric Progress"), card_body(DT::DTOutput(ns("progress_table")))),
          card(card_header("Completed Metrics"), card_body(DT::DTOutput(ns("completed_table")))),
          card(card_header("Decision Log"), card_body(DT::DTOutput(ns("decision_log_table")))),
          card(card_header("Screening Results Summary"), card_body(DT::DTOutput(ns("overview_screening_table")))),
          card(card_header("Pending Metrics"), card_body(uiOutput(ns("pending_list")))),
          card(card_header("Visual Summary"), card_body(uiOutput(ns("visual_summary_ui")))),
          card(
            card_header("Cross-Metric Correlation"),
            card_body(
              actionButton(ns("run_cross"), "Run Cross-Metric Analysis",
                           class = "btn btn-primary", icon = icon("project-diagram")),
              uiOutput(ns("cross_ui"))
            )
          )
        ),

        ## ── Pill 2: Export ──────────────────────────────────────────────────────
        nav_panel(
          "Export",
          icon = bsicons::bs_icon("download"),

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
                card(card_header("Reference Curve Thresholds"),
                     card_body(p("CSV table of Q25/Q75 thresholds and functional category boundaries."),
                               downloadButton(ns("dl_thresholds"), "Download CSV", class = "btn btn-outline-primary"))),
                card(card_header("Stratification Decisions"),
                     card_body(p("CSV table of stratification decisions for all completed metrics."),
                               downloadButton(ns("dl_strat"), "Download CSV", class = "btn btn-outline-primary"))),
                card(card_header("Regional Curves"),
                     card_body(p("CSV table of power-function coefficients for regional curves."),
                               downloadButton(ns("dl_regional"), "Download CSV", class = "btn btn-outline-primary"))),
                card(card_header("Decision Log"),
                     card_body(p("CSV of all stratification decisions."),
                               downloadButton(ns("dl_decision_log"), "Download CSV", class = "btn btn-outline-primary"))),
                card(card_header("Full Results Bundle"),
                     card_body(p("ZIP archive containing all CSVs, decision log, and session info."),
                               downloadButton(ns("dl_bundle"), "Download ZIP", class = "btn btn-primary")))
              )
            )
          ),

          card(
            card_header("Report Generation"),
            card_body(
              p(class = "text-muted", "Generate formatted reports from existing Quarto templates."),
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
          )
        ),

        ## ── Pill 3: Details ─────────────────────────────────────────────────────
        nav_panel(
          "Details",
          icon = bsicons::bs_icon("journal-text"),

          explanation_card(
            "Full Results Explorer",
            p("Browse all screening results, boxplots, and stratification details.
               Read-only view \u2014 no decisions are made here.")
          ),

          card(card_header("All Screening Results"), card_body(DT::DTOutput(ns("all_screening_table")))),
          card(
            card_header("Boxplot Viewer"),
            card_body(
              layout_column_wrap(
                width = 1 / 2,
                selectInput(ns("boxplot_metric"), "Metric:", choices = NULL),
                selectInput(ns("boxplot_strat"), "Stratification:", choices = NULL)
              ),
              uiOutput(ns("boxplot_output"))
            )
          ),
          card(card_header("Stratification Registry"), card_body(DT::DTOutput(ns("strat_registry_table"))))
        )
      )
    })

    ## ════════════════════════════════════════════════════════════════════════
    ## Pill 1: Overview
    ## ════════════════════════════════════════════════════════════════════════

    ## ── Metric progress table ────────────────────────────────────────────────
    output$progress_table <- DT::renderDT({
      all_keys <- names(rv$metric_config)
      eligible <- all_keys[sapply(all_keys, function(mk) {
        rv$metric_config[[mk]]$metric_family != "categorical"
      })]

      rows <- purrr::map_dfr(eligible, function(mk) {
        mc <- rv$metric_config[[mk]]
        screened <- mk %in% names(rv$all_layer1_results)
        cands <- rv$phase1_candidates[[mk]]
        has_candidates <- !is.null(cands)
        cand_count <- if (has_candidates) {
          sum(sapply(cands, function(x) x$tier %in% c("promising", "possible")))
        } else { 0L }

        has_decision <- !is.null(rv$phase3_verification[[mk]]) ||
          (!is.null(rv$decision_log) && nrow(rv$decision_log) > 0 &&
           any(rv$decision_log$metric == mk & rv$decision_log$phase == "phase3"))

        completed <- mk %in% names(rv$completed_metrics)

        tibble::tibble(
          Metric    = mc$display_name %||% mk,
          Screened  = if (screened) "Yes" else "",
          Candidates = if (has_candidates) as.character(cand_count) else "",
          Decision  = if (has_decision) "Yes" else "",
          Completed = if (completed) "Yes" else ""
        )
      })

      DT::datatable(rows,
        options = list(pageLength = 25, dom = "tip", scrollX = TRUE),
        rownames = FALSE, class = "compact stripe"
      ) |>
        DT::formatStyle("Screened",
          backgroundColor = DT::styleEqual("Yes", "rgba(39,174,96,0.15)")) |>
        DT::formatStyle("Candidates",
          backgroundColor = DT::styleInterval(0.5, c("white", "rgba(52,152,219,0.15)"))) |>
        DT::formatStyle("Decision",
          backgroundColor = DT::styleEqual("Yes", "rgba(39,174,96,0.15)")) |>
        DT::formatStyle("Completed",
          backgroundColor = DT::styleEqual("Yes", "rgba(39,174,96,0.15)"))
    })

    ## ── Completed metrics table ─────────────────────────────────────────────
    output$completed_table <- DT::renderDT({
      completed <- rv$completed_metrics
      if (length(completed) == 0) {
        return(DT::datatable(
          data.frame(Message = "No metrics marked complete yet. In Phase 4, review the reference curve then click 'Mark Complete \u2713'."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      rows <- purrr::map_dfr(names(completed), function(mk) {
        entry <- completed[[mk]]

        if (!is.null(entry$type) && entry$type == "regional") {
          return(tibble::tibble(
            Metric = mk, Type = "Regional Curve",
            Stratification = entry$stratify %||% "None",
            Q25 = NA_character_, Q75 = NA_character_, Status = "N/A"
          ))
        }

        strat <- if (!is.null(entry$strat_decision) &&
                     entry$strat_decision$decision_type == "single") {
          entry$strat_decision$selected_strat
        } else { "None" }

        q25_val <- if (!is.null(entry$reference_curve)) sprintf("%.2f", entry$reference_curve$curve_row$q25) else "N/A"
        q75_val <- if (!is.null(entry$reference_curve)) sprintf("%.2f", entry$reference_curve$curve_row$q75) else "N/A"
        status  <- if (!is.null(entry$reference_curve)) entry$reference_curve$curve_row$curve_status else "N/A"

        tibble::tibble(
          Metric = rv$metric_config[[mk]]$display_name %||% mk,
          Type = "Reference Curve", Stratification = strat,
          Q25 = q25_val, Q75 = q75_val, Status = status
        )
      })

      DT::datatable(rows, options = list(dom = "t", paging = FALSE, scrollX = TRUE),
                     rownames = FALSE, class = "compact stripe")
    })

    ## ── Decision log table ──────────────────────────────────────────────────
    output$decision_log_table <- DT::renderDT({
      log <- rv$decision_log
      if (is.null(log) || nrow(log) == 0) {
        return(DT::datatable(
          data.frame(message = "No decisions recorded yet."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      display_cols <- intersect(
        c("timestamp", "metric", "decision_stage", "phase", "selected_strat",
          "layer1_p_value", "layer2_effect_size", "rationale"),
        names(log)
      )
      display_df <- log[, display_cols, drop = FALSE]

      if ("layer1_p_value" %in% names(display_df)) {
        display_df$layer1_p_value <- round(display_df$layer1_p_value, 4)
      }
      if ("layer2_effect_size" %in% names(display_df)) {
        display_df$layer2_effect_size <- round(display_df$layer2_effect_size, 4)
      }

      DT::datatable(display_df,
                     options = list(pageLength = 20, scrollX = TRUE),
                     rownames = FALSE, class = "compact stripe")
    })

    ## ── Overview screening summary ───────────────────────────────────────────
    output$overview_screening_table <- DT::renderDT({
      all_l1 <- rv$all_layer1_results
      if (length(all_l1) == 0) {
        return(DT::datatable(
          data.frame(Message = "No screening results yet. Run Phase 1 screening first."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      combined <- dplyr::bind_rows(all_l1) |>
        dplyr::mutate(
          metric_display = sapply(metric, function(mk) rv$metric_config[[mk]]$display_name %||% mk),
          strat_display = sapply(stratification, function(sk) rv$strat_config[[sk]]$display_name %||% sk),
          p_value = round(p_value, 4)
        ) |>
        dplyr::select(
          Metric = metric_display, Stratification = strat_display,
          `p-value` = p_value, `Min n` = min_group_n,
          Classification = classification
        )

      DT::datatable(combined,
        options = list(pageLength = 15, scrollX = TRUE, dom = "tip"),
        rownames = FALSE, class = "compact stripe", filter = "top"
      ) |>
        DT::formatStyle("p-value",
          backgroundColor = DT::styleInterval(
            c(0.05, 0.10),
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)", "white")
          ))
    })

    ## ── Pending metrics ─────────────────────────────────────────────────────
    output$pending_list <- renderUI({
      completed_keys <- names(rv$completed_metrics)
      all_keys <- names(rv$metric_config)
      eligible <- all_keys[sapply(all_keys, function(mk) {
        rv$metric_config[[mk]]$metric_family != "categorical"
      })]
      pending <- setdiff(eligible, completed_keys)

      if (length(pending) == 0) {
        return(div(class = "text-success fw-bold", icon("check-circle"),
                   " All eligible metrics have been analyzed!"))
      }

      tags$ul(class = "list-unstyled", lapply(pending, function(mk) {
        mc <- rv$metric_config[[mk]]
        screened <- mk %in% names(rv$all_layer1_results)
        has_candidates <- mk %in% names(rv$phase1_candidates)

        status_icons <- tagList(
          if (screened) tags$span(class = "badge bg-info me-1", "Screened"),
          if (has_candidates) tags$span(class = "badge bg-primary me-1", "Candidates"),
          if (!screened) tags$span(class = "badge bg-secondary me-1", "Not started")
        )

        tags$li(class = "mb-1",
          tags$strong(mc$display_name %||% mk),
          tags$span(class = "text-muted small ms-1", paste0("(", mc$metric_family, ")")),
          tags$span(class = "ms-2", status_icons)
        )
      }))
    })

    ## ── Visual summary ──────────────────────────────────────────────────────
    output$visual_summary_ui <- renderUI({
      completed <- rv$completed_metrics
      if (length(completed) == 0) {
        return(div(class = "text-muted", "Complete at least one metric."))
      }
      tagList(
        plotOutput(ns("summary_graphic"), height = "400px"),
        div(class = "mt-2",
            downloadButton(ns("dl_summary_graphic"), "Download Summary Graphic",
                           class = "btn btn-outline-primary"))
      )
    })

    output$summary_graphic <- renderPlot({
      completed <- rv$completed_metrics
      req(length(completed) > 0)
      std <- purrr::keep(completed, ~ is.null(.x$type) || .x$type != "regional")
      if (length(std) == 0) return(NULL)

      summary_df <- purrr::map_dfr(names(std), function(mk) {
        entry <- std[[mk]]
        strat <- if (!is.null(entry$strat_decision) &&
                     entry$strat_decision$decision_type == "single") {
          entry$strat_decision$selected_strat
        } else { "None" }
        tibble::tibble(
          metric = rv$metric_config[[mk]]$display_name %||% mk,
          stratification = strat
        )
      })

      ggplot2::ggplot(summary_df, ggplot2::aes(x = metric, y = 1, fill = stratification)) +
        ggplot2::geom_tile(color = "white", linewidth = 1, height = 0.8) +
        ggplot2::scale_fill_viridis_d(name = "Stratification") +
        ggplot2::labs(title = "Completed Metrics Summary",
                       subtitle = paste0(nrow(summary_df), " metrics completed"),
                       x = NULL, y = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       panel.grid = ggplot2::element_blank())
    })

    output$dl_summary_graphic <- downloadHandler(
      filename = function() paste0("summary_graphic_", format(Sys.time(), "%Y%m%d"), ".png"),
      content = function(file) {
        completed <- rv$completed_metrics
        std <- purrr::keep(completed, ~ is.null(.x$type) || .x$type != "regional")
        if (length(std) == 0) return()
        summary_df <- purrr::map_dfr(names(std), function(mk) {
          entry <- std[[mk]]
          strat <- if (!is.null(entry$strat_decision) &&
                       entry$strat_decision$decision_type == "single") {
            entry$strat_decision$selected_strat
          } else { "None" }
          tibble::tibble(metric = rv$metric_config[[mk]]$display_name %||% mk,
                         stratification = strat)
        })
        p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = metric, y = 1, fill = stratification)) +
          ggplot2::geom_tile(color = "white", linewidth = 1, height = 0.8) +
          ggplot2::scale_fill_viridis_d(name = "Stratification") +
          ggplot2::labs(title = "Completed Metrics Summary", x = NULL, y = NULL) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                         panel.grid = ggplot2::element_blank())
        ggplot2::ggsave(file, p, width = 12, height = 4, dpi = 300)
      }
    )

    ## ── Cross-metric analysis ───────────────────────────────────────────────
    cross_results <- reactiveVal(NULL)

    observeEvent(input$run_cross, {
      result <- run_cross_metric_analysis(rv$data, rv$metric_config)
      cross_results(result)
    })

    output$cross_ui <- renderUI({
      res <- cross_results()
      req(res)
      tagList(
        if (!is.null(res$plots$correlation_heatmap)) {
          card(class = "mt-3", card_header("Correlation Heatmap"),
               card_body(plotOutput(ns("cross_heatmap"), height = "500px")))
        },
        if (nrow(res$results) > 0) {
          redundant <- res$results |> dplyr::filter(redundant_flag == TRUE)
          if (nrow(redundant) > 0) {
            card(class = "mt-3", card_header("Redundancy Flags (|r| > 0.80)"),
                 card_body(DT::DTOutput(ns("redundancy_table"))))
          }
        },
        if (!is.null(res$plots$pca_biplot)) {
          card(class = "mt-3", card_header("PCA Biplot"),
               card_body(plotOutput(ns("pca_plot"), height = "450px")))
        }
      )
    })

    output$cross_heatmap <- renderPlot({
      res <- cross_results()
      req(res, !is.null(res$plots$correlation_heatmap))
      res$plots$correlation_heatmap
    })

    output$redundancy_table <- DT::renderDT({
      res <- cross_results()
      req(res)
      redundant <- res$results |>
        dplyr::filter(redundant_flag == TRUE) |>
        dplyr::select(display_1, display_2, pearson_r, spearman_rho) |>
        dplyr::mutate(pearson_r = round(pearson_r, 3), spearman_rho = round(spearman_rho, 3))
      DT::datatable(redundant, options = list(dom = "t", paging = FALSE),
                     rownames = FALSE, class = "compact stripe")
    })

    output$pca_plot <- renderPlot({
      res <- cross_results()
      req(res, !is.null(res$plots$pca_biplot))
      res$plots$pca_biplot
    })

    ## ════════════════════════════════════════════════════════════════════════
    ## Pill 2: Export
    ## ════════════════════════════════════════════════════════════════════════

    standard_metrics <- reactive({
      purrr::keep(rv$completed_metrics, ~ is.null(.x$type) || .x$type != "regional")
    })
    regional_metrics <- reactive({
      purrr::keep(rv$completed_metrics, ~ !is.null(.x$type) && .x$type == "regional")
    })

    output$dl_thresholds <- downloadHandler(
      filename = function() paste0("reference_thresholds_", format(Sys.time(), "%Y%m%d"), ".csv"),
      content = function(file) {
        std <- standard_metrics()
        if (length(std) == 0) { write.csv(data.frame(message = "No completed metrics"), file, row.names = FALSE); return() }
        rows <- dplyr::bind_rows(lapply(std, function(e) if (!is.null(e$reference_curve)) e$reference_curve$curve_row else NULL))
        write.csv(rows, file, row.names = FALSE)
      })

    output$dl_strat <- downloadHandler(
      filename = function() paste0("strat_decisions_", format(Sys.time(), "%Y%m%d"), ".csv"),
      content = function(file) {
        std <- standard_metrics()
        if (length(std) == 0) { write.csv(data.frame(message = "No completed metrics"), file, row.names = FALSE); return() }
        rows <- dplyr::bind_rows(lapply(std, function(e) e$strat_decision))
        write.csv(rows, file, row.names = FALSE)
      })

    output$dl_regional <- downloadHandler(
      filename = function() paste0("regional_curves_", format(Sys.time(), "%Y%m%d"), ".csv"),
      content = function(file) {
        reg <- regional_metrics()
        if (length(reg) == 0) { write.csv(data.frame(message = "No completed regional curves"), file, row.names = FALSE); return() }
        rows <- dplyr::bind_rows(lapply(reg, function(e) e$model_summary))
        write.csv(rows, file, row.names = FALSE)
      })

    output$dl_decision_log <- downloadHandler(
      filename = function() paste0("decision_log_", format(Sys.time(), "%Y%m%d"), ".csv"),
      content = function(file) {
        log <- rv$decision_log
        if (is.null(log) || nrow(log) == 0) {
          write.csv(data.frame(message = "No decisions recorded"), file, row.names = FALSE)
        } else { write.csv(log, file, row.names = FALSE) }
      })

    output$dl_bundle <- downloadHandler(
      filename = function() paste0("results_bundle_", format(Sys.time(), "%Y%m%d"), ".zip"),
      content = function(file) {
        tmpdir <- tempdir()
        bundle_dir <- file.path(tmpdir, "analysis_results")
        dir.create(bundle_dir, showWarnings = FALSE, recursive = TRUE)

        std <- standard_metrics()
        reg <- regional_metrics()

        if (length(std) > 0) {
          thresholds <- dplyr::bind_rows(lapply(std, function(e) if (!is.null(e$reference_curve)) e$reference_curve$curve_row else NULL))
          write.csv(thresholds, file.path(bundle_dir, "reference_thresholds.csv"), row.names = FALSE)
          strat_decs <- dplyr::bind_rows(lapply(std, function(e) e$strat_decision))
          write.csv(strat_decs, file.path(bundle_dir, "strat_decisions.csv"), row.names = FALSE)
        }
        if (length(reg) > 0) {
          reg_rows <- dplyr::bind_rows(lapply(reg, function(e) e$model_summary))
          write.csv(reg_rows, file.path(bundle_dir, "regional_curves.csv"), row.names = FALSE)
        }

        log <- rv$decision_log
        if (!is.null(log) && nrow(log) > 0) {
          write.csv(log, file.path(bundle_dir, "decision_log.csv"), row.names = FALSE)
        }

        writeLines(capture.output(sessionInfo()), file.path(bundle_dir, "session_info.txt"))
        writeLines(c(
          paste0("Analysis Results - ", Sys.time()),
          paste0("Standard metrics completed: ", length(std)),
          paste0("Regional curves completed: ", length(reg)),
          paste0("Decision log entries: ", if (!is.null(log)) nrow(log) else 0),
          paste0("Metrics: ", paste(names(rv$completed_metrics), collapse = ", "))
        ), file.path(bundle_dir, "summary.txt"))

        old_wd <- setwd(tmpdir)
        on.exit(setwd(old_wd))
        zip(file, files = list.files("analysis_results", full.names = TRUE, recursive = TRUE))
      })

    ## Quarto reports
    report_msg <- reactiveVal(NULL)

    observeEvent(input$gen_summary_pdf, {
      report_path <- file.path(project_root, "reports", "summary.qmd")
      if (!file.exists(report_path)) { report_msg("Summary template not found."); return() }
      tryCatch({ quarto::quarto_render(report_path, output_format = "pdf"); report_msg("PDF summary generated successfully.") },
               error = function(e) report_msg(paste0("Failed: ", e$message)))
    })

    observeEvent(input$gen_dashboard, {
      report_path <- file.path(project_root, "reports", "dashboard.qmd")
      if (!file.exists(report_path)) { report_msg("Dashboard template not found."); return() }
      tryCatch({ quarto::quarto_render(report_path, output_format = "html"); report_msg("HTML dashboard generated.") },
               error = function(e) report_msg(paste0("Failed: ", e$message)))
    })

    observeEvent(input$gen_appendix, {
      report_path <- file.path(project_root, "reports", "appendix.qmd")
      if (!file.exists(report_path)) { report_msg("Appendix template not found."); return() }
      tryCatch({ quarto::quarto_render(report_path, output_format = "pdf"); report_msg("Full appendix generated.") },
               error = function(e) report_msg(paste0("Failed: ", e$message)))
    })

    output$report_status <- renderUI({
      msg <- report_msg()
      if (is.null(msg)) return(NULL)
      div(class = "alert alert-info mt-2", msg)
    })

    ## ════════════════════════════════════════════════════════════════════════
    ## Pill 3: Details (Appendix)
    ## ════════════════════════════════════════════════════════════════════════

    ## All screening results
    output$all_screening_table <- DT::renderDT({
      all_l1 <- rv$all_layer1_results
      if (length(all_l1) == 0) {
        return(DT::datatable(
          data.frame(message = "No screening results yet."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      combined <- dplyr::bind_rows(all_l1) |>
        dplyr::mutate(
          metric_display = sapply(metric, function(mk) rv$metric_config[[mk]]$display_name %||% mk),
          strat_display = sapply(stratification, function(sk) rv$strat_config[[sk]]$display_name %||% sk),
          statistic = round(statistic, 3),
          p_value = round(p_value, 4)
        ) |>
        dplyr::select(
          Metric = metric_display, Stratification = strat_display,
          Test = test, Statistic = statistic, `p-value` = p_value,
          Groups = n_groups, `Min n` = min_group_n, Classification = classification
        )

      DT::datatable(combined, options = list(pageLength = 25, scrollX = TRUE),
                     rownames = FALSE, class = "compact stripe", filter = "top")
    })

    ## Boxplot viewer
    observe({
      metric_choices <- names(rv$metric_config)
      metric_choices <- metric_choices[sapply(metric_choices, function(mk) {
        rv$metric_config[[mk]]$metric_family != "categorical"
      })]
      named_choices <- setNames(metric_choices, sapply(metric_choices, function(mk) {
        rv$metric_config[[mk]]$display_name %||% mk
      }))
      updateSelectInput(session, "boxplot_metric", choices = named_choices)
    })

    observeEvent(input$boxplot_metric, {
      req(input$boxplot_metric)
      allowed <- get_metric_allowed_strats(rv, input$boxplot_metric)
      named_strats <- setNames(allowed, sapply(allowed, function(sk) {
        rv$strat_config[[sk]]$display_name %||% sk
      }))
      updateSelectInput(session, "boxplot_strat", choices = named_strats)
    })

    output$boxplot_output <- renderUI({
      req(input$boxplot_metric, input$boxplot_strat)
      plotOutput(ns("boxplot_render"), height = "500px")
    })

    output$boxplot_render <- renderPlot({
      req(input$boxplot_metric, input$boxplot_strat)
      res <- tryCatch(
        screen_stratification(rv$data, input$boxplot_metric, input$boxplot_strat,
                              rv$metric_config, rv$strat_config),
        error = function(e) NULL
      )
      if (!is.null(res) && !is.null(res$plot)) res$plot else NULL
    })

    ## Stratification registry
    output$strat_registry_table <- DT::renderDT({
      sc <- rv$strat_config
      registry_df <- purrr::map_dfr(names(sc), function(sk) {
        entry <- sc[[sk]]
        tibble::tibble(
          Key = sk,
          `Display Name` = entry$display_name %||% sk,
          Column = entry$column_name %||% "N/A",
          Type = entry$type %||% "single",
          `Min Group Size` = entry$min_group_size %||% 5,
          Levels = paste(entry$levels %||% character(0), collapse = ", "),
          Notes = entry$notes %||% ""
        )
      })
      DT::datatable(registry_df, options = list(dom = "t", paging = FALSE, scrollX = TRUE),
                     rownames = FALSE, class = "compact stripe")
    })
  })
}
