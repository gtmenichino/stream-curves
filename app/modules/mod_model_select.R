## ── Module: Model Selection (Step 8) ─────────────────────────────────────────
## BIC ranking + user picks model.

library(shiny)
library(bslib)
library(DT)

mod_model_select_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Step 8: Model Selection",
      p("The system recommends the most parsimonious model among those within
         \u0394BIC < 2 of the best. You can accept this recommendation or select
         any other candidate model."),
      p("Review the model formula, fit statistics, and any review flags before
         confirming.")
    ),

    ## System pick
    uiOutput(ns("system_pick_ui")),

    ## Full shortlist with selection
    card(
      card_header("All Candidate Models (click to select)"),
      card_body(DT::DTOutput(ns("shortlist_table")))
    ),

    ## Formula preview
    card(
      card_header("Selected Model Formula"),
      card_body(uiOutput(ns("formula_preview")))
    ),

    ## Actions
    div(
      class = "d-flex justify-content-between mt-3",
      actionButton(ns("back"), "\u25c2 Back to Model Building",
                   class = "btn btn-outline-secondary"),
      actionButton(ns("confirm"), "Confirm Model and Run Diagnostics \u25b8",
                   class = "btn btn-primary btn-proceed")
    )
  )
}

mod_model_select_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_row <- reactiveVal(1)

    ## ── System recommendation card ────────────────────────────────────────────
    output$system_pick_ui <- renderUI({
      req(rv$model_selection_auto, rv$model_candidates)
      auto_sel <- rv$model_selection_auto |>
        dplyr::filter(metric == rv$current_metric)
      req(nrow(auto_sel) > 0)

      card(
        class = "border-success",
        card_header(class = "bg-success text-white", "System Recommendation"),
        card_body(
          p(tags$strong("Predictors: "), auto_sel$predictors),
          p(sprintf("Adj R\u00b2 = %.4f | BIC = %.2f | \u0394BIC = %.2f | Rank: %d",
                     auto_sel$adj_r2, auto_sel$bic, auto_sel$delta_bic,
                     auto_sel$selected_rank)),
          p(tags$em("Selection: most parsimonious among top models (\u0394BIC < 2)")),
          if (isTRUE(auto_sel$needs_review)) {
            div(class = "alert alert-warning mt-2 mb-0",
                tags$strong("Review: "), auto_sel$review_reason)
          }
        )
      )
    })

    ## ── Shortlist table ───────────────────────────────────────────────────────
    output$shortlist_table <- DT::renderDT({
      req(rv$model_candidates)
      cands <- rv$model_candidates$candidates_df

      display_df <- cands |>
        dplyr::select(rank, predictors, n_predictors, adj_r2, bic, delta_bic) |>
        dplyr::mutate(
          adj_r2 = round(adj_r2, 4),
          bic = round(bic, 2),
          delta_bic = round(delta_bic, 2)
        )

      DT::datatable(
        display_df,
        selection = list(mode = "single", selected = 1),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      ) |>
        DT::formatStyle(
          "delta_bic",
          backgroundColor = DT::styleInterval(2, c("rgba(52,152,219,0.1)", "white"))
        )
    })

    ## Track user row selection
    observeEvent(input$shortlist_table_rows_selected, {
      sel <- input$shortlist_table_rows_selected
      if (length(sel) > 0) selected_row(sel[1])
    })

    ## ── Formula preview ───────────────────────────────────────────────────────
    output$formula_preview <- renderUI({
      req(rv$model_candidates)
      cands <- rv$model_candidates$candidates_df
      row_idx <- selected_row()
      req(row_idx <= nrow(cands))

      row <- cands[row_idx, ]
      mc <- rv$metric_config[[rv$current_metric]]

      pred_list <- trimws(unlist(strsplit(row$predictors, ",")))
      strat_dec <- rv$strat_decision_user
      if (!is.null(strat_dec) && strat_dec$decision_type == "single" &&
          !is.na(strat_dec$selected_strat)) {
        strat_var <- strat_dec$selected_strat
        if (!strat_var %in% pred_list) {
          pred_list <- c(strat_var, pred_list)
        }
      }

      formula_str <- paste(mc$column_name, "~", paste(pred_list, collapse = " + "))

      tagList(
        div(class = "formula-preview", formula_str),
        p(class = "text-muted mt-2",
          sprintf("Rank %d | %d predictors | Adj R\u00b2 = %.4f | BIC = %.2f",
                   row$rank, row$n_predictors,
                   ifelse(is.na(row$adj_r2), 0, row$adj_r2),
                   row$bic))
      )
    })

    ## ── Confirm selection ─────────────────────────────────────────────────────
    observeEvent(input$confirm, {
      req(rv$model_candidates)
      cands <- rv$model_candidates$candidates_df
      row_idx <- selected_row()
      req(row_idx <= nrow(cands))

      rv$model_selection_user <- cands[row_idx, ]
    })

    list(
      proceed = reactive(input$confirm),
      back = reactive(input$back)
    )
  })
}
