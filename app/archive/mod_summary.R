## ── Module: Summary (Page 6) ──────────────────────────────────────────────────
## Cross-metric summary, decision log, completed metrics overview.

library(shiny)
library(bslib)
library(DT)

mod_summary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    ## Completed metrics
    card(
      card_header("Completed Metrics"),
      card_body(DT::DTOutput(ns("completed_table")))
    ),

    ## Decision log
    card(
      card_header("Decision Log"),
      card_body(DT::DTOutput(ns("decision_log_table")))
    ),

    ## Pending metrics
    card(
      card_header("Pending Metrics"),
      card_body(uiOutput(ns("pending_list")))
    ),

    ## Visual summary
    card(
      card_header("Visual Summary"),
      card_body(
        uiOutput(ns("visual_summary_ui"))
      )
    ),

    ## Cross-metric analysis
    card(
      card_header("Cross-Metric Correlation"),
      card_body(
        actionButton(ns("run_cross"), "Run Cross-Metric Analysis",
                     class = "btn btn-primary",
                     icon = icon("project-diagram")),
        uiOutput(ns("cross_ui"))
      )
    )
  )
}

mod_summary_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## ── Completed metrics table ───────────────────────────────────────────────
    output$completed_table <- DT::renderDT({
      completed <- rv$completed_metrics
      req(length(completed) > 0)

      rows <- purrr::map_dfr(names(completed), function(mk) {
        entry <- completed[[mk]]

        if (!is.null(entry$type) && entry$type == "regional") {
          return(tibble::tibble(
            Metric = mk,
            Type = "Regional Curve",
            Stratification = entry$stratify %||% "None",
            Predictors = entry$predictor,
            Adj_R2 = NA_character_,
            Diagnostics = "N/A"
          ))
        }

        strat <- if (!is.null(entry$strat_decision) &&
                     entry$strat_decision$decision_type == "single") {
          entry$strat_decision$selected_strat
        } else {
          "None"
        }

        preds <- if (!is.null(entry$model_selection)) {
          entry$model_selection$predictors
        } else {
          "N/A"
        }

        adj_r2 <- if (!is.null(entry$model_selection) &&
                       !is.na(entry$model_selection$adj_r2)) {
          sprintf("%.4f", entry$model_selection$adj_r2)
        } else {
          "N/A"
        }

        diag_status <- if (!is.null(entry$diagnostics)) {
          entry$diagnostics$summary_row$overall_status
        } else {
          "N/A"
        }

        tibble::tibble(
          Metric = rv$metric_config[[mk]]$display_name %||% mk,
          Type = "Reference Curve",
          Stratification = strat,
          Predictors = preds,
          Adj_R2 = adj_r2,
          Diagnostics = diag_status
        )
      })

      DT::datatable(
        rows,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    ## ── Decision log table ──────────────────────────────────────────────────
    output$decision_log_table <- DT::renderDT({
      log <- rv$decision_log
      if (is.null(log) || nrow(log) == 0) {
        return(DT::datatable(
          data.frame(message = "No decisions recorded yet."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      display_df <- log |>
        dplyr::select(
          Time = timestamp,
          Metric = metric,
          Stage = decision_stage,
          `Selected Strat` = selected_strat,
          `Auto Recommended` = auto_recommended,
          `User Agreed` = user_agreed,
          Mode = strat_mode,
          `p-value` = layer1_p_value,
          `Effect Size` = layer2_effect_size,
          Feasibility = layer5_feasibility,
          Rationale = rationale
        ) |>
        dplyr::mutate(
          `p-value` = round(`p-value`, 4),
          `Effect Size` = round(`Effect Size`, 4)
        )

      DT::datatable(
        display_df,
        options = list(pageLength = 20, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    ## ── Pending metrics ───────────────────────────────────────────────────────
    output$pending_list <- renderUI({
      completed_keys <- names(rv$completed_metrics)
      all_keys <- names(rv$metric_config)
      eligible <- all_keys[sapply(all_keys, function(mk) {
        rv$metric_config[[mk]]$metric_family != "categorical"
      })]
      pending <- setdiff(eligible, completed_keys)

      if (length(pending) == 0) {
        return(div(class = "text-success", "All eligible metrics have been analyzed!"))
      }

      tags$ul(lapply(pending, function(mk) {
        mc <- rv$metric_config[[mk]]
        tags$li(mc$display_name %||% mk, " (", mc$metric_family, ")")
      }))
    })

    ## ── Visual summary ──────────────────────────────────────────────────────
    output$visual_summary_ui <- renderUI({
      completed <- rv$completed_metrics
      if (length(completed) == 0) {
        return(div(class = "text-muted", "Complete at least one metric to see the visual summary."))
      }

      tagList(
        plotOutput(ns("summary_graphic"), height = "400px"),
        div(
          class = "mt-2",
          downloadButton(ns("dl_summary_graphic"), "Download Summary Graphic",
                         class = "btn btn-outline-primary")
        )
      )
    })

    output$summary_graphic <- renderPlot({
      completed <- rv$completed_metrics
      req(length(completed) > 0)

      std <- purrr::keep(completed, ~ is.null(.x$type) || .x$type != "regional")
      if (length(std) == 0) return(NULL)

      ## Build summary data for plot
      summary_df <- purrr::map_dfr(names(std), function(mk) {
        entry <- std[[mk]]
        strat <- if (!is.null(entry$strat_decision) &&
                     entry$strat_decision$decision_type == "single") {
          entry$strat_decision$selected_strat
        } else {
          "None"
        }
        tibble::tibble(
          metric = rv$metric_config[[mk]]$display_name %||% mk,
          stratification = strat,
          has_model = !is.null(entry$model_selection),
          has_curve = !is.null(entry$reference_curve)
        )
      })

      ggplot2::ggplot(summary_df, ggplot2::aes(
        x = metric, y = 1, fill = stratification
      )) +
        ggplot2::geom_tile(color = "white", linewidth = 1, height = 0.8) +
        ggplot2::scale_fill_viridis_d(name = "Stratification") +
        ggplot2::labs(
          title = "Completed Metrics Summary",
          subtitle = paste0(nrow(summary_df), " metrics completed"),
          x = NULL, y = NULL
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid = ggplot2::element_blank()
        )
    })

    output$dl_summary_graphic <- downloadHandler(
      filename = function() paste0("summary_graphic_", format(Sys.time(), "%Y%m%d"), ".png"),
      content = function(file) {
        completed <- rv$completed_metrics
        std <- purrr::keep(completed, ~ is.null(.x$type) || .x$type != "regional")
        if (length(std) == 0) return()

        summary_df <- purrr::map_dfr(names(std), function(mk) {
          entry <- std[[mk]]
          strat <- if (!is.null(entry$strat_decision) &&
                       entry$strat_decision$decision_type == "single") {
            entry$strat_decision$selected_strat
          } else { "None" }
          tibble::tibble(metric = rv$metric_config[[mk]]$display_name %||% mk,
                         stratification = strat)
        })

        p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = metric, y = 1, fill = stratification)) +
          ggplot2::geom_tile(color = "white", linewidth = 1, height = 0.8) +
          ggplot2::scale_fill_viridis_d(name = "Stratification") +
          ggplot2::labs(title = "Completed Metrics Summary", x = NULL, y = NULL) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                         panel.grid = ggplot2::element_blank())
        ggplot2::ggsave(file, p, width = 12, height = 4, dpi = 300)
      }
    )

    ## ── Cross-metric analysis ─────────────────────────────────────────────────
    cross_results <- reactiveVal(NULL)

    observeEvent(input$run_cross, {
      result <- run_cross_metric_analysis(rv$data, rv$metric_config)
      cross_results(result)
    })

    output$cross_ui <- renderUI({
      res <- cross_results()
      req(res)

      tagList(
        if (!is.null(res$plots$correlation_heatmap)) {
          card(
            class = "mt-3",
            card_header("Correlation Heatmap"),
            card_body(plotOutput(ns("heatmap"), height = "500px"))
          )
        },

        if (nrow(res$results) > 0) {
          redundant <- res$results |> dplyr::filter(redundant_flag == TRUE)
          if (nrow(redundant) > 0) {
            card(
              class = "mt-3",
              card_header("Redundancy Flags (|r| > 0.80)"),
              card_body(DT::DTOutput(ns("redundancy_table")))
            )
          }
        },

        if (!is.null(res$plots$pca_biplot)) {
          card(
            class = "mt-3",
            card_header("PCA Biplot"),
            card_body(plotOutput(ns("pca_plot"), height = "450px"))
          )
        }
      )
    })

    output$heatmap <- renderPlot({
      res <- cross_results()
      req(res, !is.null(res$plots$correlation_heatmap))
      res$plots$correlation_heatmap
    })

    output$redundancy_table <- DT::renderDT({
      res <- cross_results()
      req(res)
      redundant <- res$results |>
        dplyr::filter(redundant_flag == TRUE) |>
        dplyr::select(display_1, display_2, pearson_r, spearman_rho) |>
        dplyr::mutate(
          pearson_r = round(pearson_r, 3),
          spearman_rho = round(spearman_rho, 3)
        )

      DT::datatable(
        redundant,
        options = list(dom = "t", paging = FALSE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    output$pca_plot <- renderPlot({
      res <- cross_results()
      req(res, !is.null(res$plots$pca_biplot))
      res$plots$pca_biplot
    })
  })
}
