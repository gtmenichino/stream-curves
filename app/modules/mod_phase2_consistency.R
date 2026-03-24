## ── Module: Phase 2 — Cross-Metric Consistency ──────────────────────────────────
## Evaluates stratification performance across multiple metrics.
## Enhanced with support score heatmap, tier classification, threshold controls.

library(shiny)
library(bslib)
library(DT)

mod_phase2_consistency_ui <- function(id) {
  ns <- NS(id)

  tagList(
    ## Instructional card
    explanation_card(
      "Phase 2: Cross-Metric Consistency",
      p("Which stratifications perform consistently across multiple metrics?"),
      p("After completing Phase 1 for at least 2 metrics, compare stratification
         performance using a support score heatmap. Identify broad-use candidates
         that work across most metrics vs. metric-specific ones."),
      p(tags$strong("Requires:"), " Phase 1 complete for \u2265 2 metrics.")
    ),

    uiOutput(ns("consistency_ui"))
  )
}

mod_phase2_consistency_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    phase2_settings <- reactive({
      normalize_phase2_settings(rv)
    })

    output$consistency_ui <- renderUI({
      if (is.null(rv$data)) return(no_data_alert())
      settings <- phase2_settings()

      ## Gate: need at least 2 metrics with Phase 1 results
      n_metrics <- length(get_phase2_metric_choices(rv))

      if (n_metrics < 2) {
        return(div(
          class = "alert alert-info mt-3",
          icon("info-circle"),
          paste0(" Complete Phase 1 for at least 2 metrics to enable this view. ",
                 "Currently completed: ", n_metrics, " metric(s).")
        ))
      }

      tagList(
        ## Controls
        card(
          card_header("Analysis Controls"),
          card_body(
            layout_column_wrap(
              width = 1 / 2,
              ## Metric filter — pickerInput dropdown
              shinyWidgets::pickerInput(
                ns("metric_filter"), "Metrics to Include:",
                choices = setNames(
                  get_phase2_metric_choices(rv),
                  sapply(get_phase2_metric_choices(rv), function(mk) {
                    rv$metric_config[[mk]]$display_name %||% mk
                  })
                ),
                selected = settings$metric_filter,
                multiple = TRUE,
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE, liveSearch = TRUE,
                  selectedTextFormat = "count > 3",
                  countSelectedText = "{0} of {1} selected",
                  noneSelectedText = "None selected"
                )
              ),
              ## Stratification filter — pickerInput dropdown
              shinyWidgets::pickerInput(
                ns("strat_filter"), "Stratifications to Include:",
                choices = {
                  all_strats <- get_phase2_strat_choices(rv, settings$metric_filter)
                  setNames(all_strats, sapply(all_strats, function(sk) {
                    rv$strat_config[[sk]]$display_name %||% sk
                  }))
                },
                selected = settings$strat_filter,
                multiple = TRUE,
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE, liveSearch = TRUE,
                  selectedTextFormat = "count > 3",
                  countSelectedText = "{0} of {1} selected",
                  noneSelectedText = "None selected"
                )
              ),
              ## Significance threshold slider
              sliderInput(ns("sig_threshold"), "Significance Level (p-value):",
                          min = 0.01, max = 0.10, value = settings$sig_threshold, step = 0.01),
              ## Support cutoff slider
              sliderInput(ns("support_threshold"), "Strong Support Cutoff:",
                          min = 0.1, max = 0.9, value = settings$support_threshold, step = 0.05)
            ),
            actionButton(ns("compute_matrix"), "Compute Consistency Matrix",
                         class = "btn btn-primary", icon = icon("table"))
          )
        ),

        uiOutput(ns("results_ui"))
      )
    })

    ## ── Compute consistency ───────────────────────────────────────────────────
    consistency_results <- reactiveVal(NULL)

    ## ── Shared recompute helper ─────────────────────────────────────────────
    recompute_consistency <- function(settings = NULL) {
      recompute_phase2_shared(rv, settings)
      consistency_results(rv$cross_metric_consistency %||% NULL)
      notify_workspace_refresh(rv)
    }

    observeEvent(rv$cross_metric_consistency, {
      consistency_results(rv$cross_metric_consistency %||% NULL)
    }, ignoreInit = FALSE)

    observeEvent(input$compute_matrix, {
      req(length(get_phase2_metric_choices(rv)) >= 2, input$metric_filter, input$strat_filter)
      tryCatch(
        recompute_consistency(list(
          metric_filter = input$metric_filter,
          strat_filter = input$strat_filter,
          sig_threshold = input$sig_threshold,
          support_threshold = input$support_threshold
        )),
        error = function(e) {
          showNotification(
            paste0("Consistency computation failed: ", conditionMessage(e)),
            type = "error", duration = 8
          )
        }
      )
    })

    ## ── Re-classify tiers when slider changes ─────────────────────────────────
    observeEvent(input$support_threshold, {
      req(rv$cross_metric_consistency, rv$cross_metric_consistency$summary)
      tryCatch(
        recompute_consistency(list(
          metric_filter = input$metric_filter %||% phase2_settings()$metric_filter,
          strat_filter = input$strat_filter %||% phase2_settings()$strat_filter,
          sig_threshold = input$sig_threshold %||% phase2_settings()$sig_threshold,
          support_threshold = input$support_threshold
        )),
        error = function(e) {
          showNotification(
            paste0("Support cutoff update failed: ", conditionMessage(e)),
            type = "error", duration = 8
          )
        }
      )
    }, ignoreInit = TRUE)

    ## ── Re-run consistency when significance threshold changes ───────────────
    observeEvent(input$sig_threshold, {
      req(consistency_results())
      tryCatch(
        recompute_consistency(list(
          metric_filter = input$metric_filter %||% phase2_settings()$metric_filter,
          strat_filter = input$strat_filter %||% phase2_settings()$strat_filter,
          sig_threshold = input$sig_threshold,
          support_threshold = input$support_threshold %||% phase2_settings()$support_threshold
        )),
        error = function(e) {
          showNotification(
            paste0("Significance threshold update failed: ", conditionMessage(e)),
            type = "error", duration = 8
          )
        }
      )
    }, ignoreInit = TRUE)

    ## ── Re-run consistency when stratification filter changes ────────────────
    observeEvent(input$strat_filter, {
      req(consistency_results())
      tryCatch(
        recompute_consistency(list(
          metric_filter = input$metric_filter %||% phase2_settings()$metric_filter,
          strat_filter = input$strat_filter,
          sig_threshold = input$sig_threshold %||% phase2_settings()$sig_threshold,
          support_threshold = input$support_threshold %||% phase2_settings()$support_threshold
        )),
        error = function(e) {
          showNotification(
            paste0("Stratification filter update failed: ", conditionMessage(e)),
            type = "error", duration = 8
          )
        }
      )
    }, ignoreInit = TRUE)

    ## ── Save carry-forward overrides ──────────────────────────────────────────
    observeEvent(input$save_carry_forward, {
      ranking <- rv$phase2_ranking
      req(ranking, nrow(ranking) > 0)

      ## Read radio button values and map to tier labels
      updated_tiers <- sapply(ranking$stratification, function(sk) {
        carry_val <- input[[paste0("carry_", sk)]]
        if (is.null(carry_val)) return(ranking$tier[ranking$stratification == sk])
        switch(carry_val,
          "broad_use"       = "Broad-Use Candidate",
          "metric_specific" = "Metric-Specific Candidate",
          "do_not_carry"    = "Weak Candidate",
          ranking$tier[ranking$stratification == sk]
        )
      })

      rv$phase2_ranking <- ranking |>
        dplyr::mutate(tier = updated_tiers)

      showNotification("Carry-forward selections saved.", type = "message", duration = 3)
      notify_workspace_refresh(rv)
    })

    ## ── Results UI ────────────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      res <- consistency_results()
      req(res)

      tagList(
        ## Support score heatmap
        card(
          card_header("Support Score Heatmap"),
          card_body(
            class = "heatmap-container",
            p(class = "text-muted small",
              textOutput(ns("sig_threshold_label"), inline = TRUE)),
            plotOutput(ns("heatmap"), height = "450px")
          )
        ),

        ## Ranked summary table
        card(
          card_header("Ranked Stratification Summary"),
          card_body(DT::DTOutput(ns("ranking_table")))
        ),

        ## Tier classification cards
        if (!is.null(rv$phase2_ranking) && nrow(rv$phase2_ranking) > 0) {
          card(
            card_header("Tier Classification"),
            card_body(
              layout_column_wrap(
                width = 1 / 3,
                tier_card("Broad-Use Candidate", "success",
                          rv$phase2_ranking |> dplyr::filter(tier == "Broad-Use Candidate")),
                tier_card("Metric-Specific Candidate", "warning",
                          rv$phase2_ranking |> dplyr::filter(tier == "Metric-Specific Candidate")),
                tier_card("Weak Candidate", "danger",
                          rv$phase2_ranking |> dplyr::filter(tier == "Weak Candidate"))
              )
            )
          )
        },

        ## Stratification flagging
        if (!is.null(rv$phase2_ranking) && nrow(rv$phase2_ranking) > 0) {
          card(
            card_header("Carry-Forward Classification"),
            card_body(
              p(class = "text-muted", "Flag each stratification for subsequent phases."),
              lapply(seq_len(nrow(rv$phase2_ranking)), function(i) {
                sk <- rv$phase2_ranking$stratification[i]
                sc <- rv$strat_config[[sk]]
                tier <- rv$phase2_ranking$tier[i]

                auto_flag <- switch(tier,
                  "Broad-Use Candidate" = "broad_use",
                  "Metric-Specific Candidate" = "metric_specific",
                  "do_not_carry"
                )

                div(
                  class = "d-flex align-items-center gap-3 mb-2",
                  tags$strong(sc$display_name %||% sk, style = "min-width: 200px;"),
                  status_badge(
                    switch(tier,
                      "Broad-Use Candidate" = "pass",
                      "Metric-Specific Candidate" = "caution",
                      "not_applicable"),
                    tier
                  ),
                  radioButtons(
                    ns(paste0("carry_", sk)), NULL,
                    choices = c("Broad-use" = "broad_use",
                                "Metric-specific" = "metric_specific",
                                "Do not carry forward" = "do_not_carry"),
                    selected = auto_flag,
                    inline = TRUE
                  )
                )
              })
            )
          )
        },

        ## Save carry-forward button
        if (!is.null(rv$phase2_ranking) && nrow(rv$phase2_ranking) > 0) {
          div(
            class = "d-flex justify-content-end mt-3 mb-3",
            actionButton(ns("save_carry_forward"), "Save Carry-Forward Selections",
                         class = "btn btn-success", icon = icon("check"))
          )
        },

        ## Downloads
        div(
          class = "mt-3",
          downloadButton(ns("dl_matrix"), "Download Consistency Matrix CSV",
                         class = "btn btn-outline-primary"),
          downloadButton(ns("dl_heatmap"), "Download Heatmap PNG",
                         class = "btn btn-outline-primary ms-2")
        )
      )
    })

    ## ── Significance threshold label ─────────────────────────────────────────
    output$sig_threshold_label <- renderText({
      settings <- phase2_settings()
      paste0("Cells colored by p-value significance at \u03b1 = ",
             input$sig_threshold %||% settings$sig_threshold)
    })

    ## ── Heatmap ───────────────────────────────────────────────────────────────
    output$heatmap <- renderPlot({
      res <- consistency_results()
      req(res, !is.null(res$heatmap_plot))
      res$heatmap_plot
    })

    ## ── Ranking table ─────────────────────────────────────────────────────────
    output$ranking_table <- DT::renderDT({
      ranking <- rv$phase2_ranking
      req(ranking, nrow(ranking) > 0)

      display_df <- ranking |>
        dplyr::mutate(
          strat_display = sapply(stratification, function(sk) {
            rv$strat_config[[sk]]$display_name %||% sk
          }),
          mean_effect_size = round(mean_effect_size, 4)
        ) |>
        dplyr::select(
          Stratification = strat_display,
          `Metrics Tested` = n_metrics_tested,
          `# Promising` = n_promising,
          `# Possible` = n_possible,
          `Mean Effect Size` = mean_effect_size,
          `Consistency Score` = consistency_score,
          Tier = tier
        )

      DT::datatable(
        display_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE,
                       order = list(list(5, "desc"))),
        rownames = FALSE,
        class = "compact stripe"
      ) |>
        DT::formatStyle(
          "Tier",
          backgroundColor = DT::styleEqual(
            c("Broad-Use Candidate", "Metric-Specific Candidate", "Weak Candidate"),
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)", "rgba(231,76,60,0.15)")
          )
        )
    })

    ## ── Downloads ─────────────────────────────────────────────────────────────
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


## ── Helper for tier classification cards ──────────────────────────────────────
tier_card <- function(tier_name, color, tier_data) {
  n <- nrow(tier_data)
  strats <- if (n > 0) {
    paste(tier_data$stratification, collapse = ", ")
  } else {
    "None"
  }

  card(
    class = paste0("border-", color),
    card_header(
      class = paste0("bg-", color, if (color != "warning") " text-white" else " text-dark"),
      tier_name
    ),
    card_body(
      p(paste0(n, " stratification(s)")),
      p(class = "text-muted", style = "font-size: 0.85rem;", strats)
    )
  )
}
