## ── Module: Precheck (Step 0) ─────────────────────────────────────────────────
## Summary statistics and quality flags for the selected metric.

library(shiny)
library(bslib)
library(DT)
library(ggplot2)

mod_precheck_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Step 0: Metric Precheck",
      p("This step shows summary statistics for the selected metric: sample size,
         distribution shape, missing data, and quality flags. Review these before
         proceeding to stratification screening."),
      p("Quality flags identify potential issues: low sample size (n < min_sample_size),
         near-zero variance, or impossible values (e.g., proportions outside 0-100%).")
    ),

    layout_column_wrap(
      width = 1 / 2,

      ## Summary stats card
      card(
        card_header("Summary Statistics"),
        card_body(DT::DTOutput(ns("stats_table")))
      ),

      ## Histogram
      card(
        card_header("Distribution"),
        card_body(plotOutput(ns("histogram"), height = "300px"))
      )
    ),

    ## Quality flags
    card(
      card_header("Quality Flags"),
      card_body(uiOutput(ns("quality_flags")))
    ),

    ## Proceed button
    div(
      class = "d-flex justify-content-end mt-3",
      actionButton(ns("proceed"), "Proceed to Stratification \u25b8",
                   class = "btn btn-primary btn-proceed")
    )
  )
}

mod_precheck_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    precheck_row <- reactive({
      req(rv$current_metric)
      rv$precheck_df |>
        dplyr::filter(metric == rv$current_metric)
    })

    output$stats_table <- DT::renderDT({
      row <- precheck_row()
      req(nrow(row) == 1)

      stats <- data.frame(
        Statistic = c("n", "Missing", "Min", "Q25", "Median", "Mean", "Q75",
                       "Max", "SD", "IQR"),
        Value = c(
          row$n_obs, row$n_missing,
          round(row$min, 3), round(row$q25, 3), round(row$median, 3),
          round(row$mean, 3), round(row$q75, 3), round(row$max, 3),
          round(row$sd, 3), round(row$iqr, 3)
        ),
        stringsAsFactors = FALSE
      )

      DT::datatable(stats, options = list(dom = "t", paging = FALSE),
                     rownames = FALSE, class = "compact")
    })

    output$histogram <- renderPlot({
      req(rv$current_metric)
      mc <- rv$metric_config[[rv$current_metric]]
      col <- mc$column_name
      req(col %in% names(rv$data))

      vals <- rv$data[[col]]
      vals <- vals[!is.na(vals)]
      req(length(vals) > 0)

      df <- data.frame(value = vals)
      ggplot(df, aes(x = value)) +
        geom_histogram(bins = 12, fill = "steelblue", alpha = 0.7, color = "white") +
        labs(
          title = paste0(mc$display_name, " Distribution"),
          x = paste0(mc$display_name, " (", mc$units, ")"),
          y = "Count"
        ) +
        theme_minimal()
    })

    output$quality_flags <- renderUI({
      row <- precheck_row()
      req(nrow(row) == 1)

      flags <- tagList()
      if (isTRUE(row$flag_low_n)) {
        flags <- tagList(flags, status_badge("caution", "Low sample size"), " ")
      }
      if (isTRUE(row$flag_low_variance)) {
        flags <- tagList(flags, status_badge("caution", "Low variance"), " ")
      }
      if (isTRUE(row$flag_impossible_values)) {
        flags <- tagList(flags, status_badge("fail", "Impossible values"), " ")
      }
      if (length(flags) == 0) {
        flags <- status_badge("pass", "All checks passed")
      }

      div(
        class = "d-flex align-items-center gap-2",
        tags$strong("Overall: "),
        status_badge(row$precheck_status),
        tags$span(class = "ms-3"),
        flags
      )
    })

    ## Return proceed clicks
    reactive(input$proceed)
  })
}
