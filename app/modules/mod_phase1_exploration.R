## ── Module: Phase 1 — Initial Exploration ────────────────────────────────────────
## Merges precheck, significance screening, and effect size into a single view.
## Per-metric: metric picker + screening + effect size + candidate marking.

library(shiny)
library(bslib)
library(DT)

mod_phase1_exploration_ui <- function(id, dialog_mode = FALSE) {
  ns <- NS(id)
  uiOutput(ns("phase1_page"))
}

mod_phase1_exploration_server <- function(id, rv, dialog_mode = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    prev_metric <- reactiveVal(NULL)

    ## ── Data gate: show alert or full page ────────────────────────────────────
    output$phase1_page <- renderUI({
      if (is.null(rv$data)) return(no_data_alert())

      phase1_body <- tagList(
        ## ── Sidebar ─────────────────────────────────────────────────────────────
        ## Sidebar omitted in dialog mode.



          ## Bulk screening — button only, no heading or description





        ## ── Main content ────────────────────────────────────────────────────────
        tagList(
          ## Instructional card
          explanation_card(
            "Phase 1: Initial Exploration",
            p("Which stratifications show meaningful group separation for this metric?"),
            p("Run screening to test for statistical significance (Kruskal-Wallis) and
               effect sizes (epsilon-squared) in a single pass. Review boxplots and
               statistics, then mark each stratification as Promising, Possible, or
               Not Promising."),
            p(tags$strong("Effect size thresholds:"), " <0.01 negligible, 0.01\u20130.06 small,
               0.06\u20130.14 medium, >0.14 large.")
          ),

          ## Metric control bar (picker + info + strat controls)
          card(
            fill = FALSE,
            card_body(
              class = "py-2",
              layout_column_wrap(
                width = 1 / 2,
                ## Left: metric selector + info table
                div(
                  if (!isTRUE(dialog_mode)) uiOutput(ns("metric_picker")),
                  uiOutput(ns("metric_info_table"))
                ),
                ## Right: stratifications to test + run/reset buttons
                uiOutput(ns("metric_info_controls"))
              )
            )
          ),

          ## Compact precheck summary
          card(
            fill = FALSE,
            card_header("Metric Summary"),
            card_body(
              layout_column_wrap(
                width = 1 / 2,
                heights_equal = "row",
                DT::DTOutput(ns("precheck_stats")),
                plotOutput(ns("precheck_hist"), height = "200px")
              )
            )
          ),

          uiOutput(ns("artifact_status")),

          ## Results section
          uiOutput(ns("results_ui")),

          ## Candidate marking table
          uiOutput(ns("candidate_ui")),

          ## Save button
          uiOutput(ns("save_ui"))
        )
      )

      if (isTRUE(dialog_mode)) {
        return(div(class = "workspace-phase-body", phase1_body))
      }

      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          title = "Phase 1: Explore",
          uiOutput(ns("bulk_progress")),
          actionButton(ns("run_all_screening"), "Run all metric screenings for Phase 1",
                       class = "btn btn-primary btn-sm w-100",
                       icon = icon("play-circle")),
          actionButton(ns("reset_all_metrics"), "Reset All Metrics",
                       class = "btn btn-outline-secondary btn-sm w-100 mt-1",
                       icon = icon("arrows-rotate"))
        ),
        phase1_body
      )
    })

    ## ── Build metric picker choices (grouped by family) ───────────────────────
    output$metric_picker <- renderUI({
      mc <- rv$metric_config

      families <- list()
      for (mk in names(mc)) {
        fam <- mc[[mk]]$metric_family
        if (fam == "categorical") next
        fam_label <- switch(fam,
          continuous  = "Continuous",
          proportion  = "Proportion",
          count       = "Count",
          fam
        )
        families[[fam_label]] <- c(families[[fam_label]], setNames(mk, mc[[mk]]$display_name))
      }

      selectInput(
        ns("metric_select"), "Select Metric:",
        choices = families,
        selected = rv$current_metric %||% "perRiffle"
      )
    })

    ## ── Metric change observer ────────────────────────────────────────────────
    observeEvent(input$metric_select, {
      old_metric <- prev_metric()
      new_metric <- input$metric_select

      if (!is.null(old_metric) && identical(old_metric, new_metric)) {
        return(invisible(NULL))
      }

      ## SAVE outgoing metric's state
      if (!is.null(old_metric) && old_metric != "") {
        save_metric_phase_state(rv, old_metric)
      }

      ## Switch
      rv$current_metric <- new_metric
      prev_metric(new_metric)

      ## RESTORE incoming metric's state
      restore_metric_phase_state(rv, new_metric)
      load_screening_results(new_metric)
      if (!isTRUE(dialog_mode)) {
        refresh_screening_artifacts(new_metric, defer = FALSE)
      }
    }, ignoreInit = TRUE)

    ## ── Metric info table (left column) ────────────────────────────────────────
    output$metric_info_table <- renderUI({
      req(rv$current_metric)
      mc <- rv$metric_config[[rv$current_metric]]
      req(mc)

      precheck <- rv$precheck_df |>
        dplyr::filter(metric == rv$current_metric)

      completed <- rv$current_metric %in% names(rv$completed_metrics)

      div(
        class = "metric-info-card",
        tags$table(
          class = "table table-sm mb-0",
          tags$tbody(
            tags$tr(
              tags$td(class = "info-label", "Family:"),
              tags$td(class = "info-value", mc$metric_family)
            ),
            tags$tr(
              tags$td(class = "info-label", "Units:"),
              tags$td(class = "info-value", mc$units)
            ),
            tags$tr(
              tags$td(class = "info-label", "Direction:"),
              tags$td(class = "info-value",
                       if (isTRUE(mc$higher_is_better)) "Higher is better"
                       else if (isFALSE(mc$higher_is_better)) "Lower is better"
                       else "Neutral")
            ),
            if (nrow(precheck) > 0) {
              tagList(
                tags$tr(
                  tags$td(class = "info-label", "n obs:"),
                  tags$td(class = "info-value", precheck$n_obs)
                ),
                tags$tr(
                  tags$td(class = "info-label", "Status:"),
                  tags$td(status_badge(precheck$precheck_status))
                )
              )
            }
          )
        ),
        if (completed) {
          div(class = "mt-1", status_badge("pass", "COMPLETED"))
        }
      )
    })

    ## ── Metric info controls (right column) ─────────────────────────────────────
    output$metric_info_controls <- renderUI({
      req(rv$current_metric)
      mc <- rv$metric_config[[rv$current_metric]]
      req(mc)

      ## Build strat checkbox choices
      allowed <- get_metric_allowed_strats(rv, rv$current_metric)
      choices <- setNames(allowed, sapply(allowed, function(sk) {
        sc <- rv$strat_config[[sk]]
        if (!is.null(sc)) sc$display_name else sk
      }))

      div(
        shinyWidgets::pickerInput(
          ns("strat_checks"), "Stratifications to Test:",
          choices = choices, selected = allowed, multiple = TRUE,
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE, liveSearch = TRUE,
            selectedTextFormat = "count > 3",
            countSelectedText = "{0} of {1} selected",
            noneSelectedText = "None selected"
          )
        ),
        actionButton(ns("run_screening"), "Run this metric screening",
                     class = "btn btn-primary btn-sm w-100",
                     icon = icon("play")),
        actionButton(ns("reset_metric"), "Reset This Metric",
                     class = "btn btn-outline-danger btn-sm w-100 mt-1",
                     icon = icon("arrow-rotate-left"))
      )
    })

    ## ── Compact precheck ──────────────────────────────────────────────────────
    output$precheck_stats <- DT::renderDT({
      req(rv$current_metric)
      row <- rv$precheck_df |> dplyr::filter(metric == rv$current_metric)
      req(nrow(row) == 1)

      stats <- data.frame(
        Statistic = c("n", "Missing", "Min", "Median", "Mean", "Max", "SD"),
        Value = c(
          row$n_obs, row$n_missing,
          round(row$min, 2), round(row$median, 2),
          round(row$mean, 2), round(row$max, 2),
          round(row$sd, 2)
        ),
        stringsAsFactors = FALSE
      )

      DT::datatable(stats, options = list(dom = "t", paging = FALSE),
                     rownames = FALSE, class = "compact")
    })

    output$precheck_hist <- renderPlot({
      req(rv$current_metric)
      mc <- rv$metric_config[[rv$current_metric]]
      col <- mc$column_name
      req(col %in% names(rv$data))

      vals <- rv$data[[col]]
      vals <- vals[!is.na(vals)]
      req(length(vals) > 0)

      df <- data.frame(value = vals)
      ggplot2::ggplot(df, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(bins = 12, fill = "steelblue", alpha = 0.7, color = "white") +
        ggplot2::labs(x = mc$units, y = "Count") +
        ggplot2::theme_minimal(base_size = 10)
    })

    ## ── Run screening + effect size in single pass ────────────────────────────
    screening_results <- reactiveVal(NULL)
    artifacts_loading <- reactiveVal(FALSE)
    artifacts_error <- reactiveVal(NULL)

    set_screening_state <- function(metric = rv$current_metric) {
      state <- get_metric_phase1_display_state(rv, metric)
      if (!is.null(state) && nrow(state$results) > 0) {
        screening_results(state)
      } else {
        screening_results(NULL)
      }
      invisible(NULL)
    }

    refresh_screening_artifacts <- function(metric = rv$current_metric, defer = FALSE) {
      if (!metric_needs_phase1_artifact_refresh(rv, metric)) {
        artifacts_loading(FALSE)
        artifacts_error(NULL)
        set_screening_state(metric)
        return(invisible(FALSE))
      }

      artifacts_loading(TRUE)
      artifacts_error(NULL)

      run_refresh <- function() {
        tryCatch(
          {
            ensure_metric_phase1_artifacts(rv, metric)
            artifacts_error(NULL)
          },
          error = function(e) {
            artifacts_error(conditionMessage(e))
          },
          finally = {
            artifacts_loading(FALSE)
            set_screening_state(metric)
          }
        )
      }

      if (isTRUE(defer)) {
        target_metric <- metric
        session$onFlushed(function() {
          if (is.null(target_metric) || identical(target_metric, "")) return(invisible(NULL))
          shiny::isolate(run_refresh())
          invisible(NULL)
        }, once = TRUE)
      } else {
        withProgress(message = "Loading Phase 1 workspace...", value = 0, {
          incProgress(0, detail = "Regenerating screening plots and pairwise tables...")
          run_refresh()
          incProgress(1)
        })
      }

      invisible(TRUE)
    }

    load_screening_results <- function(metric = rv$current_metric) {
      artifacts_loading(FALSE)
      artifacts_error(NULL)
      set_screening_state(metric)
      invisible(NULL)
    }

    observeEvent(rv$workspace_modal_ready_nonce, {
      if (isTRUE(dialog_mode) && identical(rv$workspace_modal_type, "phase1")) {
        modal_metric <- rv$workspace_modal_metric %||% rv$current_metric
        if (is.null(modal_metric) || identical(modal_metric, "")) return(invisible(NULL))
        load_screening_results(modal_metric)
      }
    }, ignoreInit = TRUE)

    output$artifact_status <- renderUI({
      loading <- artifacts_loading()
      error_text <- artifacts_error()

      if (isTRUE(loading)) {
        return(div(
          class = "alert alert-info d-flex align-items-center gap-2",
          icon("spinner", class = "fa-spin"),
          tags$span("Loading full Phase 1 details. Summary results are available while plots and pairwise tables regenerate.")
        ))
      }

      if (!is.null(error_text) && nzchar(error_text)) {
        return(div(
          class = "alert alert-danger d-flex justify-content-between align-items-center flex-wrap gap-2",
          tags$span(paste0("Could not load full Phase 1 details: ", error_text)),
          actionButton(
            ns("retry_artifacts"),
            "Retry details",
            class = "btn btn-outline-danger btn-sm"
          )
        ))
      }

      NULL
    })

    observeEvent(input$retry_artifacts, {
      refresh_screening_artifacts(rv$current_metric, defer = FALSE)
    }, ignoreInit = TRUE)

    observeEvent(input$run_screening, {
      req(rv$current_metric, input$strat_checks)

      withProgress(message = "Running screening...", value = 0, {
        ## Screening
        results_list <- lapply(input$strat_checks, function(sk) {
          incProgress(0.5 / length(input$strat_checks))
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

        rv$phase1_screening <- list(
          results = result_rows,
          pairwise = pairwise_rows,
          plots = plots
        )

        ## Cache for cross-metric use
        rv$all_layer1_results[[rv$current_metric]] <- result_rows

        ## Effect sizes
        incProgress(0.3, detail = "Computing effect sizes...")
        strat_keys <- unique(result_rows$stratification)
        es <- compute_effect_sizes(
          rv$data, rv$current_metric, strat_keys,
          rv$metric_config, rv$strat_config
        )
        rv$phase1_effect_sizes <- es
        rv$all_layer2_results[[rv$current_metric]] <- es

        if (is.null(rv$metric_phase_cache[[rv$current_metric]])) {
          rv$metric_phase_cache[[rv$current_metric]] <- list()
        }
        rv$metric_phase_cache[[rv$current_metric]]$phase1_screening <- rv$phase1_screening
        rv$metric_phase_cache[[rv$current_metric]]$phase1_effect_sizes <- rv$phase1_effect_sizes
        rv$metric_phase_cache[[rv$current_metric]]$phase1_artifact_mode <- "full"
      })

      load_screening_results(rv$current_metric)
      notify_workspace_refresh(rv)
    })

    ## ── Results UI ────────────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      res <- screening_results()
      req(res)

      tagList(
        ## Combined stats table
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

        ## Effect size bar chart
        if (!is.null(res$effect_sizes) && nrow(res$effect_sizes) > 0) {
          card(
            card_header("Effect Size by Stratification"),
            card_body(plotOutput(ns("effect_bar"), height = "300px"))
          )
        },

        ## Pairwise details (collapsed)
        if (!is.null(res$pairwise) && nrow(res$pairwise) > 0) {
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

    ## Render combined results table (significance + effect size merged)
    output$results_table <- DT::renderDT({
      res <- screening_results()
      req(res)

      sig_df <- res$results |>
        dplyr::select(stratification, p_value, classification, min_group_n)

      es_df <- if (!is.null(res$effect_sizes) && nrow(res$effect_sizes) > 0) {
        res$effect_sizes |>
          dplyr::select(stratification, epsilon_squared, effect_size_label)
      } else {
        tibble::tibble(stratification = character(0),
                       epsilon_squared = numeric(0),
                       effect_size_label = character(0))
      }

      display_df <- sig_df |>
        dplyr::left_join(es_df, by = "stratification") |>
        dplyr::mutate(
          strat_display = sapply(stratification, function(sk) {
            rv$strat_config[[sk]]$display_name %||% sk
          }),
          p_value = round(p_value, 4),
          epsilon_squared = round(epsilon_squared, 4)
        ) |>
        dplyr::select(
          Stratification = strat_display,
          `p-value` = p_value,
          `Effect (eps-sq)` = epsilon_squared,
          `Effect Label` = effect_size_label,
          `Min Group n` = min_group_n,
          Classification = classification
        )

      DT::datatable(
        display_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      ) |>
        DT::formatStyle(
          "p-value",
          backgroundColor = DT::styleInterval(
            c(0.05, 0.10),
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)", "white")
          )
        )
    })

    ## Render pairwise table
    output$pairwise_table <- DT::renderDT({
      res <- screening_results()
      req(res, !is.null(res$pairwise), nrow(res$pairwise) > 0)

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

    ## Render effect size bar chart
    output$effect_bar <- renderPlot({
      res <- screening_results()
      req(res, !is.null(res$effect_sizes), nrow(res$effect_sizes) > 0)

      plot_df <- res$effect_sizes |>
        dplyr::filter(!is.na(epsilon_squared)) |>
        dplyr::mutate(
          strat_label = sapply(stratification, function(sk) {
            rv$strat_config[[sk]]$display_name %||% sk
          })
        )

      if (nrow(plot_df) == 0) return(NULL)

      ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(strat_label, epsilon_squared),
        y = epsilon_squared,
        fill = effect_size_label
      )) +
        ggplot2::geom_col(width = 0.6) +
        ggplot2::scale_fill_manual(
          values = c("large" = "#27ae60", "medium" = "#2980b9",
                     "small" = "#f39c12", "negligible" = "#95a5a6"),
          name = "Effect Size"
        ) +
        ggplot2::geom_hline(yintercept = c(0.01, 0.06, 0.14),
                            linetype = "dashed", color = "gray50", alpha = 0.7) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste0("Effect Size: ", rv$metric_config[[rv$current_metric]]$display_name),
          x = NULL,
          y = expression(epsilon^2 ~ "(Epsilon-squared)")
        ) +
        ggplot2::theme_minimal()
    })

    ## ── Bulk screening progress indicator ──────────────────────────────────────
    output$bulk_progress <- renderUI({
      n_total <- sum(sapply(names(rv$metric_config), function(mk) {
        rv$metric_config[[mk]]$metric_family != "categorical"
      }))
      n_done <- length(rv$all_layer1_results)
      if (n_done > 0 && n_done < n_total) {
        div(class = "alert alert-info py-1 px-2 mb-1",
            sprintf("%d / %d metrics screened", n_done, n_total))
      }
    })

    ## ── Bulk screening handler ──────────────────────────────────────────────────
    observeEvent(input$run_all_screening, {
      withProgress(message = "Running stratification screening...", value = 0, {
        metric_keys <- names(rv$metric_config)
        metric_keys <- metric_keys[sapply(metric_keys, function(mk) {
          rv$metric_config[[mk]]$metric_family != "categorical"
        })]
        n_metrics <- length(metric_keys)

        for (i in seq_along(metric_keys)) {
          mk <- metric_keys[i]
          mc <- rv$metric_config[[mk]]
          allowed <- get_metric_allowed_strats(rv, mk)
          if (is.null(allowed) || length(allowed) == 0) next

          incProgress(1 / n_metrics, detail = paste0("Metric: ", mc$display_name))

          ## Run screening for each allowed stratification
          results_list <- lapply(allowed, function(sk) {
            if (!sk %in% names(rv$strat_config)) return(NULL)
            tryCatch(
              screen_stratification(rv$data, mk, sk,
                                    rv$metric_config, rv$strat_config),
              error = function(e) NULL
            )
          })
          names(results_list) <- allowed
          results_list <- Filter(Negate(is.null), results_list)
          if (length(results_list) == 0) next

          result_rows <- dplyr::bind_rows(lapply(results_list, `[[`, "result_row"))
          pairwise_rows <- dplyr::bind_rows(
            purrr::keep(lapply(results_list, `[[`, "pairwise_df"), ~ nrow(.) > 0)
          )
          plots <- setNames(
            lapply(results_list, `[[`, "plot"),
            names(results_list)
          )
          plots <- Filter(Negate(is.null), plots)

          ## Store in global caches (for Phase 2)
          rv$all_layer1_results[[mk]] <- result_rows

          ## Compute effect sizes
          strat_keys_tested <- unique(result_rows$stratification)
          es <- tryCatch(
            compute_effect_sizes(rv$data, mk, strat_keys_tested,
                                 rv$metric_config, rv$strat_config),
            error = function(e) tibble::tibble()
          )
          if (nrow(es) > 0) {
            rv$all_layer2_results[[mk]] <- es
          }

          ## Auto-populate Phase 1 candidates (only if not already manually saved)
          if (is.null(rv$phase1_candidates[[mk]])) {
            auto_candidates <- purrr::map_dfr(strat_keys_tested, function(sk) {
              p_row <- result_rows |> dplyr::filter(stratification == sk)
              es_row <- if (nrow(es) > 0) {
                es |> dplyr::filter(stratification == sk)
              } else {
                tibble::tibble()
              }

              p_val <- if (nrow(p_row) > 0) p_row$p_value[1] else NA_real_
              es_label <- if (nrow(es_row) > 0) es_row$effect_size_label[1] else "negligible"

              auto_status <- if (!is.na(p_val) && p_val < 0.05 && es_label %in% c("medium", "large")) {
                "promising"
              } else if (!is.na(p_val) && p_val < 0.10) {
                "possible"
              } else {
                "not_promising"
              }

              tibble::tibble(
                metric = mk,
                stratification = sk,
                p_value = if (nrow(p_row) > 0) p_row$p_value[1] else NA_real_,
                epsilon_squared = if (nrow(es_row) > 0) es_row$epsilon_squared[1] else NA_real_,
                effect_size_label = if (nrow(es_row) > 0) es_row$effect_size_label[1] else NA_character_,
                min_group_n = if (nrow(p_row) > 0) p_row$min_group_n[1] else NA_integer_,
                candidate_status = auto_status,
                reviewer_note = ""
              )
            })
            rv$phase1_candidates[[mk]] <- auto_candidates
          }

          ## Store full Phase 1 state in metric_phase_cache
          screening_data <- list(
            results = result_rows,
            pairwise = pairwise_rows,
            plots = plots
          )

          if (is.null(rv$metric_phase_cache[[mk]])) {
            rv$metric_phase_cache[[mk]] <- list()
          }
          rv$metric_phase_cache[[mk]]$phase1_screening <- screening_data
          rv$metric_phase_cache[[mk]]$phase1_effect_sizes <- es
          rv$metric_phase_cache[[mk]]$phase1_artifact_mode <- "full"
        }
      })

      ## If current metric was screened, load its results into display
      mk <- rv$current_metric
      cached <- rv$metric_phase_cache[[mk]]
      if (!is.null(cached$phase1_screening)) {
        rv$phase1_screening <- cached$phase1_screening
        rv$phase1_effect_sizes <- cached$phase1_effect_sizes
        screening_results(list(
          results = cached$phase1_screening$results,
          pairwise = cached$phase1_screening$pairwise,
          plots = cached$phase1_screening$plots,
          effect_sizes = cached$phase1_effect_sizes
        ))
      }

      showNotification(
        paste0("Bulk screening complete for ", length(rv$all_layer1_results), " metrics."),
        type = "message", duration = 5
      )
    })

    ## ── Candidate marking UI ──────────────────────────────────────────────────
    output$candidate_ui <- renderUI({
      res <- screening_results()
      req(res)

      strats <- unique(res$results$stratification)

      card(
        card_header("Candidate Assessment"),
        card_body(
          p(class = "text-muted", "Rate each stratification based on significance and effect size."),
          lapply(strats, function(sk) {
            sc <- rv$strat_config[[sk]]
            saved_row <- rv$phase1_candidates[[rv$current_metric]]
            saved_row <- if (!is.null(saved_row) && nrow(saved_row) > 0) {
              saved_row |>
                dplyr::filter(stratification == sk) |>
                dplyr::slice(1)
            } else {
              tibble::tibble()
            }
            p_row <- res$results |> dplyr::filter(stratification == sk)
            es_row <- if (!is.null(res$effect_sizes)) {
              res$effect_sizes |> dplyr::filter(stratification == sk)
            } else {
              tibble::tibble()
            }

            ## Auto-default
            p_val <- if (nrow(p_row) > 0) p_row$p_value[1] else NA_real_
            es_label <- if (nrow(es_row) > 0) es_row$effect_size_label[1] else "negligible"

            auto_status <- if (!is.na(p_val) && p_val < 0.05 && es_label %in% c("medium", "large")) {
              "promising"
            } else if (!is.na(p_val) && p_val < 0.10) {
              "possible"
            } else {
              "not_promising"
            }

            selected_status <- if (nrow(saved_row) > 0 && "candidate_status" %in% names(saved_row)) {
              saved_row$candidate_status[1]
            } else {
              auto_status
            }

            row_class <- switch(selected_status,
              promising = "candidate-promising",
              possible = "candidate-possible",
              not_promising = "candidate-not-promising",
              ""
            )

            div(
              class = paste("d-flex align-items-center gap-3 mb-2 p-2 rounded", row_class),
              tags$strong(sc$display_name %||% sk, style = "min-width: 180px;"),
              if (!is.na(p_val)) p_value_badge(p_val),
              if (nrow(es_row) > 0) status_badge(
                switch(es_label, large = "pass", medium = "pass",
                       small = "caution", "not_applicable"),
                paste0("ES: ", es_label)
              ),
              radioButtons(
                ns(paste0("status_", sk)), NULL,
                choices = c("Promising" = "promising",
                            "Possible" = "possible",
                            "Not Promising" = "not_promising"),
                selected = selected_status,
                inline = TRUE
              ),
              textInput(ns(paste0("note_", sk)), NULL,
                        placeholder = "Note...", width = "200px",
                        value = if (nrow(saved_row) > 0 && "reviewer_note" %in% names(saved_row)) {
                          saved_row$reviewer_note[1] %||% ""
                        } else {
                          ""
                        })
            )
          })
        )
      )
    })

    ## ── Save Phase 1 Results button ──────────────────────────────────────────
    output$save_ui <- renderUI({
      res <- screening_results()
      if (is.null(res)) return(NULL)

      div(
        class = "d-flex justify-content-end mt-3",
        actionButton(ns("save_phase1"), "Save Phase 1 Results \u2713",
                     class = "btn btn-success btn-proceed",
                     icon = icon("check"))
      )
    })

    observeEvent(input$save_phase1, {
      res <- screening_results()
      req(res)

      strats <- unique(res$results$stratification)

      candidates <- purrr::map_dfr(strats, function(sk) {
        p_row <- res$results |> dplyr::filter(stratification == sk)
        es_row <- if (!is.null(res$effect_sizes)) {
          res$effect_sizes |> dplyr::filter(stratification == sk)
        } else {
          tibble::tibble()
        }

        tibble::tibble(
          metric = rv$current_metric,
          stratification = sk,
          p_value = if (nrow(p_row) > 0) p_row$p_value[1] else NA_real_,
          epsilon_squared = if (nrow(es_row) > 0) es_row$epsilon_squared[1] else NA_real_,
          effect_size_label = if (nrow(es_row) > 0) es_row$effect_size_label[1] else NA_character_,
          min_group_n = if (nrow(p_row) > 0) p_row$min_group_n[1] else NA_integer_,
          candidate_status = input[[paste0("status_", sk)]] %||% "not_promising",
          reviewer_note = input[[paste0("note_", sk)]] %||% ""
        )
      })

      rv$phase1_candidates[[rv$current_metric]] <- candidates

      ## Append to decision log
      new_entry <- tibble::tibble(
        entry_id = paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample(1000:9999, 1)),
        timestamp = Sys.time(),
        reviewer_name = "",
        metric = rv$current_metric,
        decision_stage = "phase1",
        phase = "phase1",
        selected_strat = NA_character_,
        auto_recommended = NA_character_,
        user_agreed = NA,
        strat_mode = NA_character_,
        layer1_p_value = NA_real_,
        layer2_effect_size = NA_real_,
        layer2_effect_label = NA_character_,
        layer3_stability = NA_character_,
        layer4_relevance_score = NA_real_,
        layer5_feasibility = NA_character_,
        selected_predictors = NA_character_,
        selected_bic = NA_real_,
        diagnostics_overall = NA_character_,
        rationale = paste0(
          sum(candidates$candidate_status == "promising"), " promising, ",
          sum(candidates$candidate_status == "possible"), " possible, ",
          sum(candidates$candidate_status == "not_promising"), " not promising"
        ),
        notes = ""
      )
      rv$decision_log <- dplyr::bind_rows(rv$decision_log, new_entry)

      showNotification(
        paste0("Phase 1 results saved for ", rv$metric_config[[rv$current_metric]]$display_name, "."),
        type = "message", duration = 3
      )
      notify_workspace_refresh(rv)
    })

    ## ── Reset this metric ─────────────────────────────────────────────────────
    observeEvent(input$reset_metric, {
      req(rv$current_metric)
      metric <- rv$current_metric

      for (field in PHASE_STATE_FIELDS) {
        rv[[field]] <- NULL
      }

      rv$metric_phase_cache[[metric]]   <- NULL
      rv$completed_metrics[[metric]]    <- NULL
      rv$all_layer1_results[[metric]]   <- NULL
      rv$all_layer2_results[[metric]]   <- NULL
      rv$phase1_candidates[[metric]]    <- NULL
      rv$phase3_verification[[metric]]  <- NULL

      if (!is.null(rv$decision_log) && nrow(rv$decision_log) > 0) {
        rv$decision_log <- rv$decision_log |> dplyr::filter(metric != !!metric)
      }

      screening_results(NULL)

      showNotification(
        paste0("Reset complete for ", metric, "."),
        type = "message", duration = 3
      )
      notify_workspace_refresh(rv)
    })

    ## ── Reset all metrics ─────────────────────────────────────────────────────
    observeEvent(input$reset_all_metrics, {
      showModal(modalDialog(
        title = "Reset All Metrics",
        "This will clear all completed work for every metric. This cannot be undone.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset_all"), "Reset Everything",
                       class = "btn btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_reset_all, {
      removeModal()
      reset_all_analysis(rv)
      screening_results(NULL)
      showNotification("All metrics reset \u2014 starting fresh.",
                       type = "message", duration = 3)
      notify_workspace_refresh(rv)
    })
  })
}
