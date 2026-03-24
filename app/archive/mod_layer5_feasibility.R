## ── Module: Layer 5 — Operational Feasibility (Step 5) ───────────────────────
## Evaluates sample size adequacy and implementability of stratifications.

library(shiny)
library(bslib)
library(DT)

mod_layer5_feasibility_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Step 5: Operational Feasibility (Layer 5)",
      p("This layer evaluates whether each candidate stratification is practically
         implementable given current sample sizes, data completeness, and the number
         of resulting groups/curves."),
      p(tags$strong("What to review:"), "Group sizes, completeness, sparse cells."),
      p(tags$strong("How to decide:"), "Infeasible stratifications (min group n < 3,
         >50% sparse cells) should not proceed. Marginal ones may proceed with caution.")
    ),

    uiOutput(ns("feasibility_ui")),
    uiOutput(ns("nav_buttons"))
  )
}

mod_layer5_feasibility_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Compute feasibility ─────────────────────────────────────────────────
    feasibility_results <- reactive({
      req(rv$layer2_effect_sizes)
      strat_keys <- unique(rv$layer2_effect_sizes$stratification)

      feas <- assess_feasibility(rv$data, strat_keys, rv$strat_config)
      rv$layer5_feasibility <- feas
      feas
    })

    ## ── Feasibility UI ──────────────────────────────────────────────────────
    output$feasibility_ui <- renderUI({
      feas <- feasibility_results()
      req(feas, nrow(feas) > 0)

      strat_keys <- feas$stratification

      tagList(
        ## Quantitative indicators table
        card(
          card_header("Quantitative Indicators"),
          card_body(DT::DTOutput(ns("feas_table")))
        ),

        ## Qualitative checklist per stratification
        card(
          card_header("Qualitative Assessment"),
          card_body(
            lapply(strat_keys, function(sk) {
              sc <- rv$strat_config[[sk]]
              feas_row <- feas |> dplyr::filter(stratification == sk)
              auto_flag <- if (nrow(feas_row) > 0) feas_row$feasibility_flag[1] else "unknown"

              div(
                class = "mb-4 p-3 border rounded",
                tags$h6(sc$display_name %||% sk,
                        status_badge(
                          switch(auto_flag,
                            feasible = "pass", marginal = "caution",
                            infeasible = "fail", "not_applicable"),
                          auto_flag
                        )),
                checkboxGroupInput(
                  ns(paste0("checklist_", sk)), NULL,
                  choices = c(
                    "Sample sizes adequate in each group" = "adequate_n",
                    "Group boundaries understandable and reproducible" = "clear_boundaries",
                    "Stratification granularity appropriate" = "appropriate_grain",
                    "Can be explained to method users" = "explainable",
                    "Does not create too many final curves" = "manageable_curves"
                  ),
                  inline = FALSE
                ),
                radioButtons(
                  ns(paste0("feas_decision_", sk)), "Feasibility classification:",
                  choices = c("Feasible" = "feasible",
                              "Feasible with caution" = "marginal",
                              "Not feasible" = "infeasible"),
                  selected = auto_flag,
                  inline = TRUE
                ),
                textInput(ns(paste0("feas_note_", sk)), "Notes:",
                          placeholder = "Feasibility notes...", width = "100%")
              )
            })
          )
        )
      )
    })

    ## ── Feasibility table ───────────────────────────────────────────────────
    output$feas_table <- DT::renderDT({
      feas <- feasibility_results()
      req(feas)

      display_df <- feas |>
        dplyr::mutate(
          strat_display = sapply(stratification, function(sk) {
            rv$strat_config[[sk]]$display_name %||% sk
          })
        ) |>
        dplyr::select(
          Stratification = strat_display,
          Levels = n_levels,
          `Min Group n` = min_group_n,
          `Max Group n` = max_group_n,
          `% Sparse` = pct_sparse_cells,
          `Completeness %` = data_completeness_pct,
          Flag = feasibility_flag
        )

      DT::datatable(
        display_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      ) |>
        DT::formatStyle(
          "Flag",
          backgroundColor = DT::styleEqual(
            c("feasible", "marginal", "infeasible"),
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)", "rgba(231,76,60,0.15)")
          )
        )
    })

    ## ── Navigation ──────────────────────────────────────────────────────────
    output$nav_buttons <- renderUI({
      feas <- feasibility_results()
      if (is.null(feas)) return(NULL)

      div(
        class = "d-flex justify-content-between mt-3",
        actionButton(ns("back"), "\u25c2 Back to Practical Relevance",
                     class = "btn btn-outline-secondary"),
        actionButton(ns("proceed"), "Proceed to Decision \u25b8",
                     class = "btn btn-primary btn-proceed")
      )
    })

    ## ── Store reviewer feasibility decisions on proceed ─────────────────────
    observeEvent(input$proceed, {
      feas <- feasibility_results()
      req(feas)

      strat_keys <- feas$stratification
      reviewer_feas <- purrr::map_dfr(strat_keys, function(sk) {
        tibble::tibble(
          stratification = sk,
          feasibility_override = input[[paste0("feas_decision_", sk)]] %||% "marginal",
          note = input[[paste0("feas_note_", sk)]] %||% ""
        )
      })
      rv$layer5_reviewer <- reviewer_feas
    })

    list(
      proceed = reactive(input$proceed),
      back = reactive(input$back)
    )
  })
}
