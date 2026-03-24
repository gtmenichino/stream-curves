## ── Module: Layer 1 — Statistical Significance (Step 1) ──────────────────────
## Kruskal-Wallis + Wilcoxon screening. Refactored from mod_strat_screen.R
## (screening computation + results display, without the decision panel).

library(shiny)
library(bslib)
library(DT)

mod_layer1_significance_ui <- function(id) {
  ns <- NS(id)

  tagList(
    ## ── Header / Purpose ────────────────────────────────────────────────────
    explanation_card(
      "Step 1: Statistical Significance (Layer 1)",
      p("This layer tests whether grouping sites by a stratification variable
         produces statistically different metric distributions. The Kruskal-Wallis
         test is used for overall group differences, with pairwise Wilcoxon
         rank-sum tests for specific comparisons."),
      p(tags$strong("What to review:"), "p-values, boxplot separation, pairwise comparisons."),
      p(tags$strong("How to decide:"), "Stratifications with p < 0.05 pass this layer.
         Borderline results (0.05\u20130.10) receive a 'caution' flag.")
    ),

    ## ── Controls ────────────────────────────────────────────────────────────
    card(
      card_header("Stratifications to Test"),
      card_body(
        uiOutput(ns("strat_checkboxes")),
        actionButton(ns("run_screening"), "Run Screening",
                     class = "btn btn-primary mt-2",
                     icon = icon("play"))
      )
    ),

    ## ── Results ─────────────────────────────────────────────────────────────
    uiOutput(ns("results_ui")),

    ## ── Reviewer Controls ───────────────────────────────────────────────────
    uiOutput(ns("reviewer_ui")),

    ## ── Navigation ──────────────────────────────────────────────────────────
    uiOutput(ns("nav_buttons"))
  )
}

mod_layer1_significance_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Stratification checkboxes ─────────────────────────────────────────────
    output$strat_checkboxes <- renderUI({
      req(rv$current_metric)
      mc <- rv$metric_config[[rv$current_metric]]
      allowed <- mc$allowed_stratifications
      if (is.null(allowed)) allowed <- character(0)

      choices <- setNames(allowed, sapply(allowed, function(sk) {
        sc <- rv$strat_config[[sk]]
        if (!is.null(sc)) sc$display_name else sk
      }))

      checkboxGroupInput(
        ns("strat_checks"), NULL,
        choices = choices,
        selected = allowed,
        inline = TRUE
      )
    })

    ## ── Run screening ─────────────────────────────────────────────────────────
    screening_results <- reactiveVal(NULL)

    observeEvent(input$run_screening, {
      req(rv$current_metric, input$strat_checks)

      results_list <- lapply(input$strat_checks, function(sk) {
        screen_stratification(
          rv$data, rv$current_metric, sk,
          rv$metric_config, rv$strat_config
        )
      })

      result_rows <- dplyr::bind_rows(lapply(results_list, `[[`, "result_row"))
      pairwise_rows <- dplyr::bind_rows(
        purrr::keep(lapply(results_list, `[[`, "pairwise_df"), ~ nrow(.) > 0)
      )
      plots <- setNames(
        lapply(results_list, `[[`, "plot"),
        input$strat_checks
      )
      plots <- Filter(Negate(is.null), plots)

      rv$layer1_screening <- list(
        results = result_rows,
        pairwise = pairwise_rows,
        plots = plots
      )

      ## Cache for cross-metric use
      rv$all_layer1_results[[rv$current_metric]] <- result_rows

      screening_results(list(
        results = result_rows,
        pairwise = pairwise_rows,
        plots = plots
      ))
    })

    ## ── Results UI ────────────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      res <- screening_results()
      req(res)

      tagList(
        card(
          card_header("Screening Results"),
          card_body(DT::DTOutput(ns("results_table")))
        ),

        ## Boxplot gallery (tabbed)
        if (length(res$plots) > 0) {
          do.call(
            navset_card_tab,
            c(
              list(title = "Boxplots"),
              lapply(names(res$plots), function(sk) {
                nav_panel(
                  title = rv$strat_config[[sk]]$display_name %||% sk,
                  plotOutput(ns(paste0("plot_", sk)), height = "500px")
                )
              })
            )
          )
        },

        ## Pairwise details
        if (nrow(res$pairwise) > 0) {
          bslib::accordion(
            bslib::accordion_panel(
              "Pairwise Wilcoxon Details",
              DT::DTOutput(ns("pairwise_table"))
            ),
            open = FALSE
          )
        }
      )
    })

    ## Render results table
    output$results_table <- DT::renderDT({
      res <- screening_results()
      req(res)

      display_df <- res$results |>
        dplyr::select(stratification, test, statistic, p_value,
                       n_groups, min_group_n, classification) |>
        dplyr::mutate(
          statistic = round(statistic, 3),
          p_value = round(p_value, 4)
        )

      DT::datatable(
        display_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      ) |>
        DT::formatStyle(
          "p_value",
          backgroundColor = DT::styleInterval(
            c(0.05, 0.10),
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)", "white")
          )
        )
    })

    ## Render pairwise table
    output$pairwise_table <- DT::renderDT({
      res <- screening_results()
      req(res, nrow(res$pairwise) > 0)

      display_df <- res$pairwise |>
        dplyr::mutate(
          statistic = round(statistic, 3),
          p_value = round(p_value, 4),
          p_adjusted = round(p_adjusted, 4)
        )

      DT::datatable(
        display_df,
        options = list(pageLength = 20, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    ## Render boxplots dynamically
    observe({
      res <- screening_results()
      req(res)

      lapply(names(res$plots), function(sk) {
        local({
          local_sk <- sk
          output[[paste0("plot_", local_sk)]] <- renderPlot({
            res$plots[[local_sk]]
          })
        })
      })
    })

    ## ── Reviewer controls ───────────────────────────────────────────────────
    output$reviewer_ui <- renderUI({
      res <- screening_results()
      req(res)

      strats <- unique(res$results$stratification)

      tagList(
        card(
          card_header("Reviewer Assessment"),
          card_body(
            p(class = "text-muted", "Mark each stratification for further evaluation."),
            lapply(strats, function(sk) {
              sc <- rv$strat_config[[sk]]
              p_row <- res$results |> dplyr::filter(stratification == sk)
              p_label <- if (nrow(p_row) > 0 && !is.na(p_row$p_value[1])) {
                sprintf(" (p = %.4f)", p_row$p_value[1])
              } else {
                ""
              }

              div(
                class = "d-flex align-items-center gap-3 mb-2",
                tags$strong(paste0(sc$display_name %||% sk, p_label), style = "min-width: 200px;"),
                radioButtons(
                  ns(paste0("keep_", sk)), NULL,
                  choices = c("Keep" = "keep", "Discard" = "discard", "Undecided" = "undecided"),
                  selected = if (!is.na(p_row$p_value[1]) && p_row$p_value[1] < 0.10) "keep" else "undecided",
                  inline = TRUE
                ),
                textInput(ns(paste0("note_", sk)), NULL,
                          placeholder = "Reviewer note...", width = "300px")
              )
            })
          )
        )
      )
    })

    ## ── Navigation ──────────────────────────────────────────────────────────
    output$nav_buttons <- renderUI({
      res <- screening_results()
      if (is.null(res)) return(NULL)

      div(
        class = "d-flex justify-content-between mt-3",
        actionButton(ns("back"), "\u25c2 Back to Precheck",
                     class = "btn btn-outline-secondary"),
        actionButton(ns("proceed"), "Proceed to Effect Size \u25b8",
                     class = "btn btn-primary btn-proceed")
      )
    })

    ## Store reviewer notes into rv before proceeding
    observeEvent(input$proceed, {
      res <- screening_results()
      req(res)

      ## Collect keep/discard decisions
      strats <- unique(res$results$stratification)
      reviewer_notes <- purrr::map_dfr(strats, function(sk) {
        tibble::tibble(
          stratification = sk,
          keep_status = input[[paste0("keep_", sk)]] %||% "undecided",
          note = input[[paste0("note_", sk)]] %||% ""
        )
      })
      rv$layer1_reviewer <- reviewer_notes
    })

    ## Return navigation events
    list(
      proceed = reactive(input$proceed),
      back = reactive(input$back)
    )
  })
}
