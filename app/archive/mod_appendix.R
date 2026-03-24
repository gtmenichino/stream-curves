## ── Module: Appendix / Full Results Explorer (Tab 8) ─────────────────────────
## Read-only browsable view of all screening results, boxplots, model tables.

library(shiny)
library(bslib)
library(DT)

mod_appendix_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Appendix: Full Results Explorer",
      p("Browse all screening results, boxplots, model candidates, and diagnostics
         across all metrics. This is a read-only view \u2014 no decisions are made here.")
    ),

    ## ── Screening results ───────────────────────────────────────────────────
    card(
      card_header("All Screening Results"),
      card_body(DT::DTOutput(ns("all_screening_table")))
    ),

    ## ── Boxplot viewer ──────────────────────────────────────────────────────
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

    ## ── All model candidates ────────────────────────────────────────────────
    card(
      card_header("All Model Candidates (Completed Metrics)"),
      card_body(DT::DTOutput(ns("all_models_table")))
    ),

    ## ── All diagnostics ─────────────────────────────────────────────────────
    card(
      card_header("All Diagnostic Summaries (Completed Metrics)"),
      card_body(DT::DTOutput(ns("all_diagnostics_table")))
    ),

    ## ── Stratification registry ─────────────────────────────────────────────
    card(
      card_header("Stratification Registry"),
      card_body(DT::DTOutput(ns("strat_registry_table")))
    )
  )
}

mod_appendix_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── All screening results table ─────────────────────────────────────────
    output$all_screening_table <- DT::renderDT({
      all_l1 <- rv$all_layer1_results
      if (length(all_l1) == 0) {
        return(DT::datatable(
          data.frame(message = "No screening results yet. Run bulk screening or complete metrics."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      combined <- dplyr::bind_rows(all_l1) |>
        dplyr::mutate(
          metric_display = sapply(metric, function(mk) {
            rv$metric_config[[mk]]$display_name %||% mk
          }),
          strat_display = sapply(stratification, function(sk) {
            rv$strat_config[[sk]]$display_name %||% sk
          }),
          statistic = round(statistic, 3),
          p_value = round(p_value, 4)
        ) |>
        dplyr::select(
          Metric = metric_display,
          Stratification = strat_display,
          Test = test,
          Statistic = statistic,
          `p-value` = p_value,
          Groups = n_groups,
          `Min n` = min_group_n,
          Classification = classification
        )

      DT::datatable(
        combined,
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe",
        filter = "top"
      )
    })

    ## ── Boxplot viewer ──────────────────────────────────────────────────────
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
      mc <- rv$metric_config[[input$boxplot_metric]]
      allowed <- mc$allowed_stratifications %||% character(0)
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
        screen_stratification(
          rv$data, input$boxplot_metric, input$boxplot_strat,
          rv$metric_config, rv$strat_config
        ),
        error = function(e) NULL
      )
      if (!is.null(res) && !is.null(res$plot)) res$plot else NULL
    })

    ## ── All model candidates ────────────────────────────────────────────────
    output$all_models_table <- DT::renderDT({
      completed <- rv$completed_metrics
      if (length(completed) == 0) {
        return(DT::datatable(
          data.frame(message = "No completed metrics yet."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      std <- purrr::keep(completed, ~ is.null(.x$type) || .x$type != "regional")
      if (length(std) == 0) {
        return(DT::datatable(
          data.frame(message = "No standard metrics completed."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      model_rows <- purrr::map_dfr(names(std), function(mk) {
        entry <- std[[mk]]
        if (!is.null(entry$model_candidates) && !is.null(entry$model_candidates$candidates_df)) {
          entry$model_candidates$candidates_df
        } else {
          tibble::tibble()
        }
      })

      if (nrow(model_rows) == 0) {
        return(DT::datatable(
          data.frame(message = "No model candidates available."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      DT::datatable(
        model_rows,
        options = list(pageLength = 20, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe",
        filter = "top"
      )
    })

    ## ── All diagnostics ─────────────────────────────────────────────────────
    output$all_diagnostics_table <- DT::renderDT({
      completed <- rv$completed_metrics
      std <- purrr::keep(completed, ~ is.null(.x$type) || .x$type != "regional")

      if (length(std) == 0) {
        return(DT::datatable(
          data.frame(message = "No diagnostics available."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      diag_rows <- purrr::map_dfr(names(std), function(mk) {
        entry <- std[[mk]]
        if (!is.null(entry$diagnostics) && !is.null(entry$diagnostics$summary_row)) {
          entry$diagnostics$summary_row
        } else {
          tibble::tibble()
        }
      })

      DT::datatable(
        diag_rows,
        options = list(pageLength = 20, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    ## ── Stratification registry ─────────────────────────────────────────────
    output$strat_registry_table <- DT::renderDT({
      sc <- rv$strat_config

      registry_df <- purrr::map_dfr(names(sc), function(sk) {
        entry <- sc[[sk]]
        tibble::tibble(
          Key = sk,
          `Display Name` = entry$display_name %||% sk,
          `Column` = entry$column_name %||% "N/A",
          Type = entry$type %||% "single",
          `Min Group Size` = entry$min_group_size %||% 5,
          Levels = paste(entry$levels %||% character(0), collapse = ", "),
          Notes = entry$notes %||% ""
        )
      })

      DT::datatable(
        registry_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
  })
}
