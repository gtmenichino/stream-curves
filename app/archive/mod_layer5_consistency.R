## ── Module: Cross-Metric Consistency (Tab 4) ─────────────────────────────────
## Evaluates stratification consistency across multiple completed metrics.
## This is the user's brief "Layer 5" — a top-level tab, not a wizard step.

library(shiny)
library(bslib)
library(DT)

mod_layer5_consistency_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Cross-Metric Stratification Consistency",
      p("This view evaluates whether stratification variables perform consistently
         across multiple metrics. A stratification that is significant for most
         metrics may be preferable as a standard scheme for the assessment."),
      p(tags$strong("Requires:"), "Complete Layer 1 (Significance) and Layer 2
         (Effect Size) for at least 2 metrics.")
    ),

    uiOutput(ns("consistency_ui"))
  )
}

mod_layer5_consistency_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$consistency_ui <- renderUI({
      ## Gate: need at least 2 metrics
      n_metrics <- length(rv$all_layer1_results)

      if (n_metrics < 2) {
        return(div(
          class = "alert alert-info mt-3",
          icon("info-circle"),
          paste0(" Complete Layers 1\u20132 for at least 2 metrics to enable this view. ",
                 "Currently completed: ", n_metrics, " metric(s).")
        ))
      }

      tagList(
        ## Compute button
        card(
          card_body(
            actionButton(ns("compute_consistency"), "Compute Consistency Matrix",
                         class = "btn btn-primary", icon = icon("table"))
          )
        ),

        uiOutput(ns("results_ui"))
      )
    })

    ## ── Compute consistency ─────────────────────────────────────────────────
    consistency_results <- reactiveVal(NULL)

    observeEvent(input$compute_consistency, {
      req(length(rv$all_layer1_results) >= 2)

      result <- compute_strat_consistency(
        rv$all_layer1_results,
        rv$all_layer2_results,
        rv$metric_config,
        rv$strat_config
      )
      rv$cross_metric_consistency <- result
      consistency_results(result)
    })

    ## ── Results UI ──────────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      res <- consistency_results()
      req(res)

      tagList(
        ## Heatmap
        if (!is.null(res$heatmap_plot)) {
          card(
            card_header("Significance Heatmap"),
            card_body(plotOutput(ns("heatmap"), height = "450px"))
          )
        },

        ## Summary table
        card(
          card_header("Consistency Summary by Stratification"),
          card_body(DT::DTOutput(ns("summary_table")))
        ),

        ## Interpretation guide
        card(
          class = "border-info",
          card_header(class = "bg-info text-white", "Interpretation Guide"),
          card_body(
            tags$ul(
              tags$li("High consistency score (>0.6): Stratification is broadly effective across metrics."),
              tags$li("Moderate (0.3\u20130.6): Effective for some metrics but not all."),
              tags$li("Low (<0.3): Metric-specific; consider per-metric stratification choices.")
            )
          )
        ),

        ## Export
        div(
          class = "mt-3",
          downloadButton(ns("dl_matrix"), "Download Consistency Matrix CSV",
                         class = "btn btn-outline-primary"),
          downloadButton(ns("dl_heatmap"), "Download Heatmap PNG",
                         class = "btn btn-outline-primary ms-2")
        )
      )
    })

    ## ── Heatmap ─────────────────────────────────────────────────────────────
    output$heatmap <- renderPlot({
      res <- consistency_results()
      req(res, !is.null(res$heatmap_plot))
      res$heatmap_plot
    })

    ## ── Summary table ───────────────────────────────────────────────────────
    output$summary_table <- DT::renderDT({
      res <- consistency_results()
      req(res, nrow(res$summary) > 0)

      display_df <- res$summary |>
        dplyr::mutate(
          strat_display = sapply(stratification, function(sk) {
            rv$strat_config[[sk]]$display_name %||% sk
          }),
          mean_effect_size = round(mean_effect_size, 4)
        ) |>
        dplyr::select(
          Stratification = strat_display,
          `Metrics Tested` = n_metrics_tested,
          `Significant` = n_significant,
          `% Significant` = pct_significant,
          `Mean Effect Size` = mean_effect_size,
          `Consistency Score` = consistency_score
        )

      DT::datatable(
        display_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE,
                       order = list(list(5, "desc"))),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    ## ── Downloads ───────────────────────────────────────────────────────────
    output$dl_matrix <- downloadHandler(
      filename = function() paste0("strat_consistency_matrix_", format(Sys.time(), "%Y%m%d"), ".csv"),
      content = function(file) {
        res <- consistency_results()
        if (!is.null(res) && nrow(res$consistency_matrix) > 0) {
          write.csv(res$consistency_matrix, file, row.names = FALSE)
        } else {
          write.csv(data.frame(message = "No data"), file, row.names = FALSE)
        }
      }
    )

    output$dl_heatmap <- downloadHandler(
      filename = function() paste0("strat_consistency_heatmap_", format(Sys.time(), "%Y%m%d"), ".png"),
      content = function(file) {
        res <- consistency_results()
        if (!is.null(res) && !is.null(res$heatmap_plot)) {
          ggplot2::ggsave(file, res$heatmap_plot, width = 10, height = 6, dpi = 300)
        }
      }
    )
  })
}
