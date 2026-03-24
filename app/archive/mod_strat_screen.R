## ── Module: Stratification Screening (Step 2) ────────────────────────────────
## Boxplots + KW tests + user picks stratification.

library(shiny)
library(bslib)
library(DT)

mod_strat_screen_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Step 2: Stratification Screening",
      p("This step tests whether grouping sites by a stratification variable
         (e.g., ecoregion, drainage area category) produces statistically different
         metric distributions. The Kruskal-Wallis test is used for overall group
         differences, with pairwise Wilcoxon rank-sum tests for specific comparisons."),
      p("Select which stratifications to test, review the results, then decide
         which (if any) to use as a covariate in model building.")
    ),

    ## Controls
    card(
      card_header("Stratifications to Test"),
      card_body(
        uiOutput(ns("strat_checkboxes")),
        actionButton(ns("run_screening"), "Run Screening",
                     class = "btn btn-primary mt-2",
                     icon = icon("play"))
      )
    ),

    ## Results (hidden until run)
    uiOutput(ns("results_ui")),

    ## Decision panel (hidden until results exist)
    uiOutput(ns("decision_ui"))
  )
}

mod_strat_screen_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Stratification checkboxes ─────────────────────────────────────────────
    output$strat_checkboxes <- renderUI({
      req(rv$current_metric)
      mc <- rv$metric_config[[rv$current_metric]]
      allowed <- mc$allowed_stratifications
      if (is.null(allowed)) allowed <- character(0)

      ## Build display names from strat_config
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

      ## Auto-decision
      auto_decision <- make_stratification_decisions(
        result_rows, pairwise_rows,
        rv$metric_config, rv$strat_config
      ) |> dplyr::filter(metric == rv$current_metric)

      rv$strat_screening <- list(
        results = result_rows,
        pairwise = pairwise_rows,
        plots = plots
      )
      rv$strat_decision_auto <- auto_decision

      screening_results(list(
        results = result_rows,
        pairwise = pairwise_rows,
        plots = plots,
        auto_decision = auto_decision
      ))
    })

    ## ── Results UI ────────────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      res <- screening_results()
      req(res)

      tagList(
        ## Results table
        card(
          card_header("Screening Results"),
          card_body(DT::DTOutput(ns("results_table")))
        ),

        ## Boxplot gallery (tabbed — one tab per stratification)
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
        ),

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

    ## ── Decision UI ───────────────────────────────────────────────────────────
    output$decision_ui <- renderUI({
      res <- screening_results()
      req(res)

      auto_dec <- res$auto_decision

      ## Build radio choices
      tested_strats <- unique(res$results$stratification)
      choices <- c(
        setNames(tested_strats, sapply(tested_strats, function(sk) {
          sc <- rv$strat_config[[sk]]
          p_row <- res$results |> dplyr::filter(stratification == sk)
          p_label <- if (nrow(p_row) > 0 && !is.na(p_row$p_value[1])) {
            sprintf(" (p = %.4f)", p_row$p_value[1])
          } else {
            ""
          }
          paste0(sc$display_name %||% sk, p_label)
        })),
        "none" = "None \u2014 skip stratification"
      )

      ## Pre-select recommendation
      selected_val <- if (!is.null(auto_dec) && nrow(auto_dec) > 0 &&
                          auto_dec$decision_type == "single" &&
                          !is.na(auto_dec$selected_strat)) {
        auto_dec$selected_strat
      } else {
        "none"
      }

      tagList(
        ## System recommendation
        card(
          class = if (!is.null(auto_dec) && isTRUE(auto_dec$needs_review)) {
            "border-warning"
          } else {
            "border-success"
          },
          card_header(
            class = if (!is.null(auto_dec) && isTRUE(auto_dec$needs_review)) {
              "bg-warning text-dark"
            } else {
              "bg-success text-white"
            },
            "System Recommendation"
          ),
          card_body(
            if (!is.null(auto_dec) && nrow(auto_dec) > 0 && auto_dec$decision_type == "single") {
              sc <- rv$strat_config[[auto_dec$selected_strat]]
              tagList(
                p(tags$strong("Recommended: "), sc$display_name %||% auto_dec$selected_strat),
                p("Scoring: significance \u00d7 0.5 + sample size \u00d7 0.3 + simplicity \u00d7 0.2"),
                p(sprintf("p-value: %.4f | groups: %d | min group n: %d",
                           auto_dec$selected_p_value,
                           auto_dec$selected_n_groups,
                           auto_dec$selected_min_n)),
                if (isTRUE(auto_dec$needs_review)) {
                  div(class = "alert alert-warning mt-2 mb-0",
                      tags$strong("Needs review: "), auto_dec$review_reason)
                }
              )
            } else {
              p("No stratification reached significance (p < 0.05). Consider skipping stratification.")
            }
          )
        ),

        ## User decision
        card(
          card_header("Your Decision"),
          card_body(
            radioButtons(ns("strat_choice"), "Select stratification:",
                         choices = choices, selected = selected_val),
            div(
              class = "d-flex justify-content-between mt-3",
              actionButton(ns("back"), "\u25c2 Back to Precheck",
                           class = "btn btn-outline-secondary"),
              actionButton(ns("confirm"), "Confirm Stratification and Proceed \u25b8",
                           class = "btn btn-primary btn-proceed")
            )
          )
        )
      )
    })

    ## Store user decision on confirm
    observeEvent(input$confirm, {
      choice <- input$strat_choice
      if (choice == "none") {
        rv$strat_decision_user <- tibble::tibble(
          metric = rv$current_metric,
          decision_type = "none",
          selected_strat = NA_character_,
          selected_p_value = NA_real_,
          selected_n_groups = NA_integer_,
          selected_min_n = NA_integer_,
          runner_up_strat = NA_character_,
          runner_up_p_value = NA_real_,
          needs_review = FALSE,
          review_reason = NA_character_,
          notes = "User chose none"
        )
      } else {
        ## Use auto decision if the user picked the same, else build custom row
        auto_dec <- rv$strat_decision_auto
        if (!is.null(auto_dec) && nrow(auto_dec) > 0 &&
            !is.na(auto_dec$selected_strat) && auto_dec$selected_strat == choice) {
          rv$strat_decision_user <- auto_dec
        } else {
          ## Build from screening results
          res <- rv$strat_screening$results
          row <- res |> dplyr::filter(stratification == choice)
          rv$strat_decision_user <- tibble::tibble(
            metric = rv$current_metric,
            decision_type = "single",
            selected_strat = choice,
            selected_p_value = if (nrow(row) > 0) row$p_value[1] else NA_real_,
            selected_n_groups = if (nrow(row) > 0) row$n_groups[1] else NA_integer_,
            selected_min_n = if (nrow(row) > 0) row$min_group_n[1] else NA_integer_,
            runner_up_strat = NA_character_,
            runner_up_p_value = NA_real_,
            needs_review = FALSE,
            review_reason = NA_character_,
            notes = "User override"
          )
        }
      }
    })

    ## Return button clicks
    list(
      proceed = reactive(input$confirm),
      back = reactive(input$back)
    )
  })
}
