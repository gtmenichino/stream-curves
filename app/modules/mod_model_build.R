## ── Module: Model Building (Step 7) ──────────────────────────────────────────
## Best subsets / GLM candidates with user-controlled predictor selection.

library(shiny)
library(bslib)
library(DT)
library(ggplot2)

mod_model_build_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Step 7: Model Building",
      p("This step runs exhaustive best-subsets regression (for continuous/proportion
         metrics) or Poisson/negative-binomial GLM (for count metrics) to identify
         candidate models. Models are ranked by BIC (Bayesian Information Criterion),
         which balances fit and parsimony."),
      p("Select which predictors to include, then run model building. Models with
         \u0394BIC < 2 from the best are considered 'top models'.")
    ),

    ## Predictor selection
    card(
      card_header("Predictors to Include"),
      card_body(
        uiOutput(ns("pred_checkboxes")),
        uiOutput(ns("strat_note")),
        actionButton(ns("run_models"), "Build Models",
                     class = "btn btn-primary mt-2",
                     icon = icon("cogs"))
      )
    ),

    ## Results
    uiOutput(ns("results_ui"))
  )
}

mod_model_build_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Predictor checkboxes ──────────────────────────────────────────────────
    output$pred_checkboxes <- renderUI({
      req(rv$current_metric)
      mc <- rv$metric_config[[rv$current_metric]]
      allowed <- mc$allowed_predictors
      if (is.null(allowed)) allowed <- character(0)

      ## Filter to columns present in data
      analysis_data <- rv$phase4_data %||% rv$data
      available <- allowed[allowed %in% names(analysis_data)]

      ## Build display names with NA counts
      choices <- setNames(available, sapply(available, function(p) {
        pc <- rv$predictor_config[[p]]
        n_na <- sum(is.na(analysis_data[[p]]))
        label <- if (!is.null(pc)) pc$display_name else p
        if (n_na > 0) paste0(label, " \u2014 ", n_na, " NAs") else label
      }))

      checkboxGroupInput(
        ns("pred_checks"), NULL,
        choices = choices,
        selected = available,
        inline = TRUE
      )
    })

    ## Stratification note
    output$strat_note <- renderUI({
      dec <- rv$phase4_strat_decision %||% rv$strat_decision_user
      req(dec)

      if (dec$decision_type == "single" && !is.na(dec$selected_strat)) {
        sc <- rv$strat_config[[dec$selected_strat]]
        strat_name <- sc$display_name %||% dec$selected_strat
        mc <- rv$metric_config[[rv$current_metric]]
        mode <- mc$stratification_mode %||% "covariate"

        div(class = "alert alert-info mt-2 mb-0",
            tags$strong("Stratification: "), strat_name,
            if (mode == "covariate") {
              " will be included as a covariate in the model formula."
            } else {
              " \u2014 models will be fit separately per level."
            })
      }
    })

    ## ── Run model building ────────────────────────────────────────────────────
    model_results <- reactiveVal(NULL)

    observeEvent(input$run_models, {
      req(rv$current_metric, input$pred_checks)

      ## Temporarily override allowed_predictors to user's selection
      temp_config <- rv$metric_config
      temp_config[[rv$current_metric]]$allowed_predictors <- input$pred_checks

      strat_dec <- rv$phase4_strat_decision %||% rv$strat_decision_user
      if (is.null(strat_dec)) {
        strat_dec <- tibble::tibble(
          metric = rv$current_metric,
          decision_type = "none",
          selected_strat = NA_character_
        )
      }

      analysis_data <- rv$phase4_data %||% rv$data
      result <- build_model_candidates(
        analysis_data, rv$current_metric, strat_dec,
        temp_config, rv$predictor_config
      )

      rv$model_candidates <- result

      ## Auto-select
      if (nrow(result$candidates_df) > 0) {
        auto_sel <- select_final_models(
          result$candidates_df, result$importance_df, temp_config
        )
        rv$model_selection_auto <- auto_sel
      }

      model_results(result)
    })

    ## ── Results UI ────────────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      res <- model_results()
      req(res, nrow(res$candidates_df) > 0)

      tagList(
        ## Candidates table
        card(
          card_header("Candidate Models"),
          card_body(DT::DTOutput(ns("candidates_table")))
        ),

        layout_column_wrap(
          width = 1 / 2,
          ## BIC plot
          if (!is.null(res$plots$bic_vs_npred)) {
            card(
              card_header("BIC vs Predictors"),
              card_body(plotOutput(ns("bic_plot"), height = "300px"))
            )
          },
          ## Importance chart
          if (!is.null(res$plots$predictor_importance)) {
            card(
              card_header("Predictor Importance"),
              card_body(plotOutput(ns("importance_plot"), height = "300px"))
            )
          }
        ),

        ## Proceed
        div(
          class = "d-flex justify-content-between mt-3",
          actionButton(ns("back"), "\u25c2 Back to Stratification",
                       class = "btn btn-outline-secondary"),
          actionButton(ns("proceed"), "Proceed to Model Selection \u25b8",
                       class = "btn btn-primary btn-proceed")
        )
      )
    })

    ## Render candidates table
    output$candidates_table <- DT::renderDT({
      res <- model_results()
      req(res)

      display_df <- res$candidates_df |>
        dplyr::select(rank, predictors, n_predictors, r_squared, adj_r2, bic, delta_bic) |>
        dplyr::mutate(
          r_squared = round(r_squared, 4),
          adj_r2 = round(adj_r2, 4),
          bic = round(bic, 2),
          delta_bic = round(delta_bic, 2)
        )

      DT::datatable(
        display_df,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      ) |>
        DT::formatStyle(
          "delta_bic",
          backgroundColor = DT::styleInterval(2, c("rgba(52,152,219,0.1)", "white"))
        )
    })

    output$bic_plot <- renderPlot({
      res <- model_results()
      req(res, !is.null(res$plots$bic_vs_npred))
      res$plots$bic_vs_npred
    })

    output$importance_plot <- renderPlot({
      res <- model_results()
      req(res, !is.null(res$plots$predictor_importance))
      res$plots$predictor_importance
    })

    ## Return button clicks
    list(
      proceed = reactive(input$proceed),
      back = reactive(input$back)
    )
  })
}
