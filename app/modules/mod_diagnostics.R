## ── Module: Diagnostics (Step 9) ──────────────────────────────────────────────
## 4-panel residual diagnostics and assumption checks.

library(shiny)
library(bslib)
library(DT)

mod_diagnostics_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Step 9: Residual Diagnostics",
      p("This step evaluates whether the selected model meets the assumptions of
         linear regression: normality of residuals (Shapiro-Wilk), constant variance
         (Breusch-Pagan), no multicollinearity (VIF), and no unduly influential
         observations (Cook's distance)."),
      p("A 'caution' status suggests the model may be borderline but usable.
         A 'fail' suggests reconsidering the model or transformations.")
    ),

    ## Auto-run diagnostics on entry
    uiOutput(ns("diagnostics_ui"))
  )
}

mod_diagnostics_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Run diagnostics automatically when step is entered ────────────────────
    diag_results <- reactive({
      req(rv$current_metric, rv$model_selection_user)
      sel <- rv$model_selection_user
      mc <- rv$metric_config[[rv$current_metric]]

      pred_list <- trimws(unlist(strsplit(sel$predictors, ",")))

      ## Add stratification variable if applicable
      strat_dec <- rv$phase4_strat_decision %||% rv$strat_decision_user
      analysis_data <- rv$phase4_data %||% rv$data
      if (!is.null(strat_dec) && strat_dec$decision_type == "single" &&
          !is.na(strat_dec$selected_strat)) {
        strat_var <- strat_dec$selected_strat
        if (!strat_var %in% pred_list && strat_var %in% names(analysis_data)) {
          pred_list <- c(strat_var, pred_list)
        }
      }

      formula_str <- paste(mc$column_name, "~", paste(pred_list, collapse = " + "))
      model_family <- if (isTRUE(mc$count_model)) "poisson" else "gaussian"

      result <- run_diagnostics(analysis_data, rv$current_metric, formula_str,
                                 model_family, rv$metric_config)
      rv$diagnostics <- result
      result
    })

    ## ── Render UI ─────────────────────────────────────────────────────────────
    output$diagnostics_ui <- renderUI({
      res <- diag_results()
      req(res)

      overall <- res$summary_row$overall_status

      tagList(
        ## Overall status banner
        if (overall == "pass") {
          div(class = "alert alert-success",
              tags$strong("Overall: "), status_badge("pass", "PASS"),
              " \u2014 Model meets all diagnostic assumptions.")
        } else if (overall == "caution") {
          div(class = "alert alert-warning",
              tags$strong("Overall: "), status_badge("caution", "CAUTION"),
              " \u2014 Some assumptions are borderline. Model may be acceptable but review the details.")
        } else {
          div(class = "alert alert-danger",
              tags$strong("Overall: "), status_badge("fail", "FAIL"),
              " \u2014 One or more assumptions are violated. Consider reconsidering model selection.")
        },

        ## 4-panel diagnostic plot
        if (!is.null(res$plots$diagnostic_4panel)) {
          card(
            card_header("Diagnostic Plots"),
            card_body(plotOutput(ns("diag_plot"), height = "550px"))
          )
        },

        ## Assumption tests table
        card(
          card_header("Assumption Tests"),
          card_body(DT::DTOutput(ns("tests_table")))
        ),

        ## Formula used
        card(
          card_header("Model Details"),
          card_body(
            div(class = "formula-preview", res$summary_row$formula),
            p(class = "text-muted mt-2",
              sprintf("Family: %s | n = %d",
                       res$summary_row$model_family, res$summary_row$n_obs))
          )
        ),

        ## Actions
        div(
          class = "d-flex justify-content-between mt-3",
          actionButton(ns("back"), "\u25c2 Back to Model Selection",
                       class = "btn btn-outline-secondary"),
          actionButton(ns("proceed"), "Proceed to Reference Curve \u25b8",
                       class = "btn btn-primary btn-proceed")
        )
      )
    })

    ## Render diagnostic plot
    output$diag_plot <- renderPlot({
      res <- diag_results()
      req(res, !is.null(res$plots$diagnostic_4panel))
      res$plots$diagnostic_4panel
    })

    ## Render tests table
    output$tests_table <- DT::renderDT({
      res <- diag_results()
      req(res)
      s <- res$summary_row

      tests_df <- data.frame(
        Test = c("Shapiro-Wilk (Normality)", "Breusch-Pagan (Heteroscedasticity)",
                  "VIF (Collinearity)", "Cook's Distance (Influence)"),
        Statistic = c(
          sprintf("p = %.4f", s$shapiro_p),
          sprintf("p = %.4f", s$bp_p),
          sprintf("max = %.2f", s$max_vif),
          sprintf("max = %.4f", s$max_cooks)
        ),
        Status = c(s$shapiro_status, s$bp_status, s$vif_status, s$cooks_status),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        tests_df,
        options = list(dom = "t", paging = FALSE),
        rownames = FALSE,
        class = "compact"
      ) |>
        DT::formatStyle(
          "Status",
          backgroundColor = DT::styleEqual(
            c("pass", "caution", "fail", "not_applicable"),
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)",
              "rgba(231,76,60,0.15)", "rgba(149,165,166,0.1)")
          )
        )
    })

    list(
      proceed = reactive(input$proceed),
      back = reactive(input$back)
    )
  })
}
