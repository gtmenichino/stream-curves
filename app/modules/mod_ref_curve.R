## ── Module: Reference Curve ─────────────────────────────────────────
## IQR-based piecewise-linear scoring curve (empirical, no model fitting).

library(shiny)
library(bslib)
library(DT)

mod_ref_curve_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Empirical Scoring Curve",
      p("The scoring curve maps metric values to a 0\u20131 index score using the
         interquartile range (IQR) of reference-standard sites. Q25 and Q75 define
         the 'Functioning' (0.70\u20131.00) range. Thresholds are computed directly
         from the empirical distribution \u2014 no model fitting required."),
      p("Functional categories: ",
        tags$span(class = "text-success fw-bold", "Functioning (0.70\u20131.00)"), " | ",
        tags$span(class = "text-warning fw-bold", "At-Risk (0.30\u20130.69)"), " | ",
        tags$span(class = "text-danger fw-bold", "Not Functioning (0.00\u20130.29)"))
    ),

    uiOutput(ns("curve_ui"))
  )
}

mod_ref_curve_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Build reference curve automatically ───────────────────────────────────
    curve_results <- reactive({
      req(rv$current_metric)
      metric <- rv$current_metric
      decision_tbl <- get_metric_phase4_decision_state(rv, metric)

      if (is.null(rv$current_stratum_level) && metric_has_phase4_cache(rv, metric, decision_tbl)) {
        cached <- get_metric_phase4_cached_result(rv, metric)
        if (!is.null(cached$reference_curve)) {
          rv$reference_curve <- cached$reference_curve
          return(cached$reference_curve)
        }
      }

      analysis_data <- rv$phase4_data %||% rv$data
      result <- build_reference_curve(
        analysis_data,
        metric,
        rv$metric_config,
        stratum_label = rv$current_stratum_level
      )
      rv$reference_curve <- result
      cache_metric_phase4_results(
        rv,
        metric,
        decision_tbl = decision_tbl,
        reference_curve = result
      )
      result
    })

    output$curve_ui <- renderUI({
      res <- curve_results()
      req(res)

      mc <- rv$metric_config[[rv$current_metric]]

      tagList(
        layout_column_wrap(
          width = 1 / 2,

          ## Bar chart / histogram
          if (!is.null(res$bar_chart_plot)) {
            card(
              card_header("Reference Distribution"),
              card_body(plotOutput(ns("bar_chart"), height = "400px"))
            )
          },

          ## Scoring curve
          if (!is.null(res$curve_plot)) {
            card(
              card_header("Scoring Curve"),
              card_body(plotOutput(ns("curve_plot"), height = "400px"))
            )
          }
        ),

        ## Descriptive statistics card
        card(
          card_header("Descriptive Statistics"),
          card_body(
            layout_column_wrap(
              width = 1 / 4,
              div(tags$strong("n"), tags$br(), tags$span(class = "fs-4", res$curve_row$n_reference)),
              div(tags$strong("Min"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$min_val, 2))),
              div(tags$strong("Q25"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$q25, 2))),
              div(tags$strong("Median"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$median_val, 2))),
              div(tags$strong("Q75"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$q75, 2))),
              div(tags$strong("Max"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$max_val, 2))),
              div(tags$strong("IQR"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$iqr, 2))),
              div(tags$strong("SD"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$sd_val, 2)))
            )
          )
        ),

        ## Key stats panel
        card(
          card_header("Key Statistics"),
          card_body(
            layout_column_wrap(
              width = 1 / 3,
              div(
                tags$strong("Sample Size"), tags$br(),
                tags$span(class = "fs-5", res$curve_row$n_reference)
              ),
              div(
                tags$strong("Stratification"), tags$br(),
                tags$span(class = "fs-5", {
                  if (!is.null(rv$current_stratum_level)) {
                    strat <- rv$strat_decision_user
                    strat_name <- if (!is.null(strat) && strat$decision_type == "single") {
                      rv$strat_config[[strat$selected_strat]]$display_name %||% strat$selected_strat
                    } else {
                      "Subset"
                    }
                    paste0(strat_name, ": ", rv$current_stratum_level)
                  } else {
                    strat <- rv$strat_decision_user
                    if (!is.null(strat) && strat$decision_type == "single") {
                      rv$strat_config[[strat$selected_strat]]$display_name %||% strat$selected_strat
                    } else {
                      "None"
                    }
                  }
                })
              ),
              div(
                tags$strong("Curve Status"), tags$br(),
                status_badge(
                  if (res$curve_row$curve_status == "complete") "pass" else "caution",
                  res$curve_row$curve_status
                )
              )
            )
          )
        ),

        ## Threshold table
        card(
          card_header("Scoring Thresholds"),
          card_body(DT::DTOutput(ns("threshold_table")))
        ),

        ## Curve details
        card(
          card_header("Curve Details"),
          card_body(
            tags$table(
              class = "table table-sm table-bordered",
              tags$tbody(
                tags$tr(tags$td(tags$strong("Q25")), tags$td(round(res$curve_row$q25, 3))),
                tags$tr(tags$td(tags$strong("Q75")), tags$td(round(res$curve_row$q75, 3))),
                tags$tr(tags$td(tags$strong("IQR")), tags$td(round(res$curve_row$iqr, 3))),
                tags$tr(tags$td(tags$strong("Direction")),
                        tags$td(if (isTRUE(mc$higher_is_better)) "Higher is better" else "Lower is better")),
                tags$tr(tags$td(tags$strong("n (reference)")), tags$td(res$curve_row$n_reference)),
                tags$tr(tags$td(tags$strong("Status")),
                        tags$td(status_badge(
                          if (res$curve_row$curve_status == "complete") "pass" else "caution",
                          res$curve_row$curve_status
                        )))
              )
            )
          )
        ),

        ## Export controls
        div(
          class = "d-flex gap-2 mb-3",
          downloadButton(ns("dl_curve_plot"), "Download Curve Plot (PNG)",
                         class = "btn btn-outline-secondary btn-sm"),
          downloadButton(ns("dl_curve_table"), "Download Curve Table (CSV)",
                         class = "btn btn-outline-secondary btn-sm")
        ),

        ## Mark complete
        div(
          class = "d-flex justify-content-end mt-3",
          actionButton(ns("mark_complete"), "Mark Metric Complete \u2713",
                       class = "btn btn-success btn-proceed",
                       icon = icon("check"))
        )
      )
    })

    output$bar_chart <- renderPlot({
      res <- curve_results()
      req(res, !is.null(res$bar_chart_plot))
      res$bar_chart_plot
    })

    output$curve_plot <- renderPlot({
      res <- curve_results()
      req(res, !is.null(res$curve_plot))
      res$curve_plot
    })

    output$threshold_table <- DT::renderDT({
      res <- curve_results()
      req(res)
      cr <- res$curve_row

      threshold_df <- data.frame(
        Category = c("Functioning", "Functioning-at-Risk", "Not Functioning"),
        `Score Range` = c("0.70 \u2013 1.00", "0.30 \u2013 0.69", "0.00 \u2013 0.29"),
        `Metric Min` = round(c(cr$functioning_min, cr$at_risk_min, cr$not_functioning_min), 2),
        `Metric Max` = round(c(cr$functioning_max, cr$at_risk_max, cr$not_functioning_max), 2),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      DT::datatable(
        threshold_df,
        options = list(dom = "t", paging = FALSE),
        rownames = FALSE,
        class = "compact"
      ) |>
        DT::formatStyle(
          "Category",
          backgroundColor = DT::styleEqual(
            c("Functioning", "Functioning-at-Risk", "Not Functioning"),
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)", "rgba(231,76,60,0.15)")
          )
        )
    })

    ## ── Export controls ──────────────────────────────────────────────────────
    output$dl_curve_plot <- downloadHandler(
      filename = function() {
        paste0(rv$current_metric, "_reference_curve_", format(Sys.time(), "%Y%m%d"), ".png")
      },
      content = function(file) {
        res <- curve_results()
        if (!is.null(res) && !is.null(res$curve_plot)) {
          ggplot2::ggsave(file, res$curve_plot, width = 8, height = 6, dpi = 300)
        }
      }
    )

    output$dl_curve_table <- downloadHandler(
      filename = function() {
        paste0(rv$current_metric, "_reference_curve_", format(Sys.time(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        res <- curve_results()
        if (!is.null(res) && !is.null(res$curve_row)) {
          write.csv(res$curve_row, file, row.names = FALSE)
        }
      }
    )

    ## ── Mark complete ─────────────────────────────────────────────────────────
    observeEvent(input$mark_complete, {
      req(rv$current_metric)
      mk <- rv$current_metric

      ## In subset mode, let Phase 4 finalization handle completion tracking
      if (!is.null(rv$current_stratum_level)) {
        showNotification(
          paste0(rv$metric_config[[mk]]$display_name, " \u2014 ",
                 rv$current_stratum_level, " stratum curve complete!"),
          type = "message", duration = 3
        )
        return()
      }

      ## Non-stratified: mark complete directly
      phase4_signature <- cache_metric_phase4_results(
        rv,
        mk,
        decision_tbl = get_metric_phase4_decision_state(rv, mk),
        reference_curve = rv$reference_curve
      )
      rv$completed_metrics[[mk]] <- list(
        strat_decision = rv$strat_decision_user,
        reference_curve = rv$reference_curve,
        phase4_signature = phase4_signature
      )

      showNotification(
        paste0(rv$metric_config[[mk]]$display_name, " marked complete!"),
        type = "message", duration = 3
      )
      notify_workspace_refresh(rv)
    })

    list(
      complete = reactive(input$mark_complete),
      back = reactive(NULL)
    )
  })
}
