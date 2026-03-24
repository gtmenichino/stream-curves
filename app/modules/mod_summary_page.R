## -- Module: Summary -----------------------------------------------------------
## Cross-metric tracking table for phase selections and reference curve outputs.

library(shiny)
library(bslib)

mod_summary_page_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("summary_page"))
}

mod_summary_page_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    registered_metrics <- reactiveVal(character(0))
    row_snapshot <- reactiveValues()
    row_expanded <- reactiveValues()
    row_busy <- reactiveValues()
    bulk_recompute_active <- reactiveVal(FALSE)

    summary_metrics <- reactive({
      eligible_summary_metrics(rv$metric_config)
    })

    current_nav <- reactive({
      session$rootScope()$input[["main_navbar"]] %||% NULL
    })

    same_set <- function(x, y) {
      identical(sort(unique(x %||% character(0))), sort(unique(y %||% character(0))))
    }

    summary_progress_detail <- function(stage, metric = NULL, index = 1L, total = 1L) {
      metric_label <- if (is.null(metric)) {
        NULL
      } else {
        rv$metric_config[[metric]]$display_name %||% metric
      }

      switch(
        stage,
        phase1 = paste0("Phase 1/4: Screening ", index, "/", total, " - ", metric_label),
        phase2 = "Phase 2/4: Updating cross-metric consistency",
        phase3 = paste0("Phase 3/4: Refreshing selections ", index, "/", total, " - ", metric_label),
        phase4 = paste0("Phase 4/4: Building reference curve ", index, "/", total, " - ", metric_label),
        "Working..."
      )
    }

    make_progress_notifier <- function(total_steps, message) {
      progress <- shiny::Progress$new(session, min = 0, max = total_steps)
      current_step <- 0L
      progress$set(message = message, detail = NULL, value = current_step)

      list(
        update = function(stage, metric = NULL, index = 1L, total = 1L, state = "start") {
          detail <- summary_progress_detail(stage, metric, index, total)
          if (identical(state, "start")) {
            progress$set(message = message, detail = detail, value = current_step)
          } else {
            current_step <<- current_step + 1L
            progress$set(message = message, detail = detail, value = current_step)
          }
          invisible(NULL)
        },
        close = function() {
          progress$close()
          invisible(NULL)
        }
      )
    }

    set_row_busy <- function(metric, value = TRUE) {
      row_busy[[metric]] <- isTRUE(value)
      invisible(NULL)
    }

    finish_row_update <- function(metric) {
      session$onFlushed(function() {
        row_busy[[metric]] <- FALSE
      }, once = TRUE)
    }

    refresh_metric_row <- function(metric) {
      if (!(metric %in% summary_metrics())) return(invisible(NULL))
      row_snapshot[[metric]] <- build_metric_summary_snapshot(rv, metric)
      invisible(row_snapshot[[metric]])
    }

    refresh_metric_rows <- function(metrics = summary_metrics()) {
      for (metric in metrics) {
        refresh_metric_row(metric)
      }
      invisible(NULL)
    }

    close_summary_picker_menus <- function(metric = NULL) {
      row_id <- if (is.null(metric)) NULL else ns(paste0("row_", metric))
      session$sendCustomMessage("summaryClosePickers", list(rowId = row_id))
      invisible(NULL)
    }

    compact_picker_options <- function(none_text) {
      shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        selectedTextFormat = "count > 1",
        countSelectedText = "{0} selected",
        noneSelectedText = none_text,
        container = "body"
      )
    }

    note_group_ui <- function(metric, phase_key, title, items) {
      button_id <- ns(paste0("open_", phase_key, "_", metric))

      div(
        class = "summary-note-group",
        div(
          class = "summary-note-heading",
          tags$h6(title),
          actionButton(
            button_id,
            "Open Phase",
            class = "btn btn-outline-secondary btn-sm summary-phase-open"
          )
        ),
        if (length(items) == 0) {
          div(class = "summary-note-empty", "No notes.")
        } else {
          tags$ul(
            class = "summary-note-list",
            lapply(items, function(item) {
              tags$li(
                class = paste("summary-note-item", paste0("summary-note-", item$level)),
                item$text
              )
            })
          )
        }
      )
    }

    plain_table_ui <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(div(class = "text-muted", "No values available."))
      }

      tags$table(
        class = "table table-sm table-striped summary-detail-table",
        tags$thead(
          tags$tr(lapply(names(df), tags$th))
        ),
        tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            tags$tr(lapply(df[i, , drop = TRUE], function(value) tags$td(as.character(value))))
          })
        )
      )
    }

    detail_ui <- function(metric, row_data) {
      notes <- row_data$notes
      curve_rows <- row_data$curve_rows
      completed <- row_data$completed
      plot_id <- ns(paste0("plot_", metric))

      plot_card <- card(
        class = "summary-detail-card mb-3",
        card_header("Scoring Curve (Phase 4)"),
        card_body(
          if (nrow(curve_rows) == 0) {
            div(class = "text-muted", "No current phase 4 outputs. Use Recompute to build the curve.")
          } else {
            plotOutput(plot_id, height = "420px")
          }
        )
      )

      detail_tables <- if (nrow(curve_rows) <= 1) {
        card(
          class = "summary-detail-card mb-0",
          card_header("Phase 4 Details"),
          card_body(
            plain_table_ui(data.frame(
              Statistic = c("n", "Min", "Q25", "Q75", "Max", "IQR", "SD"),
              Value = if (nrow(curve_rows) == 0) {
                rep("N/A", 7)
              } else {
                c(
                  curve_rows$n_reference[1],
                  format_summary_number(curve_rows$min_val[1]),
                  format_summary_number(curve_rows$q25[1]),
                  format_summary_number(curve_rows$q75[1]),
                  format_summary_number(curve_rows$max_val[1]),
                  format_summary_number(curve_rows$iqr[1]),
                  format_summary_number(curve_rows$sd_val[1])
                )
              },
              stringsAsFactors = FALSE,
              check.names = FALSE
            )),
            if (nrow(curve_rows) > 0) {
              tagList(
                tags$hr(),
                plain_table_ui(data.frame(
                  Category = c("Functioning", "Functioning-At-Risk", "Non-functioning"),
                  Range = c(
                    format_summary_range(curve_rows$functioning_min[1], curve_rows$functioning_max[1]),
                    format_summary_range(curve_rows$at_risk_min[1], curve_rows$at_risk_max[1]),
                    format_summary_range(curve_rows$not_functioning_min[1], curve_rows$not_functioning_max[1])
                  ),
                  stringsAsFactors = FALSE,
                  check.names = FALSE
                ))
              )
            }
          )
        )
      } else {
        card(
          class = "summary-detail-card mb-0",
          card_header("Phase 4 Details by Stratum"),
          card_body(
            plain_table_ui(build_stratified_stats_table(curve_rows)),
            tags$hr(),
            plain_table_ui(build_stratified_threshold_table(curve_rows))
          )
        )
      }

      note_card <- card(
        class = "summary-detail-card mb-3",
        card_header("Notes by Phase"),
        card_body(
          layout_column_wrap(
            width = 1 / 2,
            note_group_ui(metric, "phase1", "Phase 1", notes[["Phase 1"]]),
            note_group_ui(metric, "phase2", "Phase 2", notes[["Phase 2"]]),
            note_group_ui(metric, "phase3", "Phase 3", notes[["Phase 3"]]),
            note_group_ui(metric, "phase4", "Phase 4", notes[["Phase 4"]])
          )
        )
      )

      selected_strat_label <- NULL
      if (!is.null(completed) && !is.null(completed$strat_decision) &&
          nrow(completed$strat_decision) > 0 &&
          completed$strat_decision$decision_type[1] == "single") {
        selected_key <- completed$strat_decision$selected_strat[1]
        selected_strat_label <- row_data$strat_label_map[[selected_key]] %||%
          get_strat_display_name(rv, selected_key)
      }

      tagList(
        note_card,
        layout_column_wrap(
          width = 1 / 2,
          plot_card,
          detail_tables
        ),
        if (!is.null(selected_strat_label)) {
          div(
            class = "summary-detail-footer",
            tags$strong("Selected stratification: "),
            selected_strat_label
          )
        }
      )
    }

    render_metric_row <- function(metric, row_data) {
      is_expanded <- isTRUE(row_expanded[[metric]])
      is_busy <- isTRUE(row_busy[[metric]])
      is_locked <- is_busy || isTRUE(bulk_recompute_active())

      available_choices <- make_named_strat_choices(
        row_data$available_choices,
        row_data$strat_label_map
      )
      row_choices <- make_named_strat_choices(
        row_data$available_selected,
        row_data$strat_label_map
      )
      phase3_choices <- c(
        "None" = "none",
        make_named_strat_choices(row_data$phase3_choices, row_data$strat_label_map)
      )

      tagList(
        tags$tr(
          class = "summary-main-row",
          tags$td(
            class = "summary-cell-expand",
            actionButton(
              ns(paste0("toggle_", metric)),
              if (is_expanded) "\u25be" else "\u25b8",
              class = "btn btn-outline-secondary btn-sm summary-expand-btn"
            )
          ),
          tags$td(tags$strong(row_data$display_name)),
          tags$td(row_data$family),
          tags$td(row_data$units),
          tags$td(row_data$direction),
          tags$td(row_data$n_obs),
          tags$td(
            class = "summary-col-status",
            tags$span(
              class = paste("summary-status-text", row_data$status$summary_class),
              row_data$status$summary_label
            )
          ),
          tags$td(
            class = "summary-select-cell summary-select-cell-compact summary-picker-cell summary-col-available",
            tags$fieldset(
              disabled = if (is_locked) "disabled" else NULL,
              shinyWidgets::pickerInput(
                ns(paste0("available_", metric)),
                label = NULL,
                choices = available_choices,
                selected = row_data$available_selected,
                multiple = TRUE,
                width = "100%",
                options = compact_picker_options("None")
              )
            )
          ),
          tags$td(
            class = "summary-select-cell summary-select-cell-compact summary-picker-cell summary-col-phase1",
            tags$fieldset(
              disabled = if (is_locked) "disabled" else NULL,
              shinyWidgets::pickerInput(
                ns(paste0("p1_", metric)),
                label = NULL,
                choices = row_choices,
                selected = row_data$phase1_selected,
                multiple = TRUE,
                width = "100%",
                options = compact_picker_options("None")
              )
            )
          ),
          tags$td(
            class = "summary-select-cell summary-select-cell-compact summary-picker-cell summary-col-phase2",
            tags$fieldset(
              disabled = if (is_locked) "disabled" else NULL,
              shinyWidgets::pickerInput(
                ns(paste0("p2_", metric)),
                label = NULL,
                choices = row_choices,
                selected = row_data$phase2_selected,
                multiple = TRUE,
                width = "100%",
                options = compact_picker_options("None")
              )
            )
          ),
          tags$td(
            class = "summary-select-cell summary-col-phase3",
            tags$fieldset(
              disabled = if (is_locked) "disabled" else NULL,
              selectInput(
                ns(paste0("p3_", metric)),
                label = NULL,
                choices = phase3_choices,
                selected = row_data$phase3_selected,
                width = "100%"
              )
            )
          ),
          tags$td(
            div(
              class = "d-flex flex-column gap-1",
              actionButton(
                ns(paste0("recompute_", metric)),
                "Recompute Row",
                class = "btn btn-primary btn-sm",
                disabled = if (is_locked) "disabled" else NULL
              ),
              if (is_busy) {
                div(class = "text-muted small", "Applying...")
              }
            )
          )
        ),
        tags$tr(
          class = if (is_expanded) "summary-detail-row" else "summary-detail-row d-none",
          tags$td(
            colspan = 12,
            if (is_expanded) detail_ui(metric, row_data)
          )
        )
      )
    }

    observeEvent(summary_metrics(), {
      metrics <- summary_metrics()
      new_metrics <- setdiff(metrics, registered_metrics())

      if (length(new_metrics) > 0) {
        for (mk in new_metrics) {
          local({
            metric <- mk

            row_expanded[[metric]] <- FALSE
            row_busy[[metric]] <- FALSE

            observeEvent(input[[paste0("toggle_", metric)]], {
              row_expanded[[metric]] <- !isTRUE(row_expanded[[metric]])
            }, ignoreInit = TRUE)

            observeEvent(input[[paste0("available_", metric)]], {
              selected <- input[[paste0("available_", metric)]] %||% character(0)
              current <- row_snapshot[[metric]]$available_selected %||% character(0)
              if (same_set(selected, current)) return(invisible(NULL))

              set_row_busy(metric, TRUE)
              close_summary_picker_menus(metric)
              set_metric_available_strats(rv, metric, selected)
              refresh_metric_row(metric)
              finish_row_update(metric)
            }, ignoreInit = TRUE)

            observeEvent(input[[paste0("p1_", metric)]], {
              selected <- input[[paste0("p1_", metric)]] %||% character(0)
              current <- row_snapshot[[metric]]$phase1_selected %||% character(0)
              if (same_set(selected, current)) return(invisible(NULL))

              set_row_busy(metric, TRUE)
              close_summary_picker_menus(metric)
              set_metric_phase1_candidates(rv, metric, selected)
              ensure_metric_phase3_valid(rv, metric)
              refresh_metric_row(metric)
              finish_row_update(metric)
            }, ignoreInit = TRUE)

            observeEvent(input[[paste0("p2_", metric)]], {
              selected <- input[[paste0("p2_", metric)]] %||% character(0)
              current <- row_snapshot[[metric]]$phase2_selected %||% character(0)
              if (same_set(selected, current)) return(invisible(NULL))

              set_row_busy(metric, TRUE)
              close_summary_picker_menus(metric)
              set_metric_phase2_override(rv, metric, selected)
              ensure_metric_phase3_valid(rv, metric)
              refresh_metric_row(metric)
              finish_row_update(metric)
            }, ignoreInit = TRUE)

            observeEvent(input[[paste0("p3_", metric)]], {
              old_value <- row_snapshot[[metric]]$phase3_selected %||% "none"
              new_value <- input[[paste0("p3_", metric)]] %||% "none"
              if (identical(old_value, new_value)) return(invisible(NULL))

              set_row_busy(metric, TRUE)
              set_metric_phase3_selected(rv, metric, new_value)
              clear_metric_phase4_results(rv, metric)
              clear_metric_summary_edit_notes(rv, metric, "Phase 3")
              clear_metric_summary_edit_notes(rv, metric, "Phase 4")
              refresh_metric_row(metric)
              finish_row_update(metric)
            }, ignoreInit = TRUE)

            observeEvent(input[[paste0("recompute_", metric)]], {
              req(!isTRUE(bulk_recompute_active()))
              set_row_busy(metric, TRUE)
              progress <- make_progress_notifier(
                total_steps = 4L,
                message = paste("Recomputing", rv$metric_config[[metric]]$display_name %||% metric)
              )
              tryCatch({
                recompute_metric_from_summary(
                  rv,
                  metric,
                  refresh_phase2 = TRUE,
                  mode = "summary",
                  progress_cb = progress$update
                )
                clear_metric_summary_edit_notes(rv, metric, "Phase 4")
              }, error = function(e) {
                showNotification(
                  paste0(
                    "Recompute failed for ",
                    rv$metric_config[[metric]]$display_name %||% metric,
                    ": ",
                    conditionMessage(e)
                  ),
                  type = "error",
                  duration = 8
                )
              }, finally = {
                progress$close()
              })

              refresh_metric_rows(summary_metrics())
              finish_row_update(metric)
            }, ignoreInit = TRUE)

            observeEvent(input[[paste0("open_phase1_", metric)]], {
              launch_workspace_modal(rv, "phase1", metric)
            }, ignoreInit = TRUE)

            observeEvent(input[[paste0("open_phase2_", metric)]], {
              launch_workspace_modal(rv, "phase2")
            }, ignoreInit = TRUE)

            observeEvent(input[[paste0("open_phase3_", metric)]], {
              launch_workspace_modal(rv, "phase3", metric)
            }, ignoreInit = TRUE)

            observeEvent(input[[paste0("open_phase4_", metric)]], {
              launch_workspace_modal(rv, "phase4", metric)
            }, ignoreInit = TRUE)

            output[[paste0("row_", metric)]] <- renderUI({
              row_data <- row_snapshot[[metric]]
              req(!is.null(row_data))
              render_metric_row(metric, row_data)
            })

            output[[paste0("plot_", metric)]] <- renderPlot({
              row_data <- row_snapshot[[metric]]
              req(!is.null(row_data), nrow(row_data$curve_rows) > 0)

              entry <- row_data$completed
              curve_rows <- row_data$curve_rows
              req(entry)

              if (nrow(curve_rows) > 1) {
                overlay <- build_overlay_curve_plot(curve_rows, rv$metric_config)
                if (!is.null(overlay)) return(overlay)

                for (lvl in names(entry$stratum_results %||% list())) {
                  curve_plot <- entry$stratum_results[[lvl]]$reference_curve$curve_plot %||% NULL
                  if (!is.null(curve_plot)) return(curve_plot)
                }
                return(NULL)
              }

              if (!is.null(entry$reference_curve$curve_plot)) {
                entry$reference_curve$curve_plot
              } else if (!is.null(entry$stratum_results)) {
                first_result <- purrr::detect(entry$stratum_results, ~ !is.null(.x$reference_curve$curve_plot))
                first_result$reference_curve$curve_plot %||% NULL
              } else {
                NULL
              }
            })
          })
        }

        registered_metrics(union(registered_metrics(), new_metrics))
      }

      refresh_metric_rows(metrics)
    }, ignoreInit = FALSE)

    observeEvent(rv$data, {
      if (!is.null(rv$data)) {
        refresh_metric_rows(summary_metrics())
      }
    }, ignoreInit = TRUE)

    observeEvent(rv$precheck_df, {
      refresh_metric_rows(summary_metrics())
    }, ignoreInit = TRUE)

    observeEvent(rv$strat_config, {
      refresh_metric_rows(summary_metrics())
    }, ignoreInit = TRUE)

    observeEvent(current_nav(), {
      if (identical(current_nav(), "Reference Curves")) {
        refresh_metric_rows(summary_metrics())
      }
    }, ignoreInit = TRUE)

    observeEvent(rv$workspace_modal_nonce, {
      refresh_metric_rows(summary_metrics())
    }, ignoreInit = TRUE)

    observeEvent(rv$workspace_refresh_nonce, {
      refresh_metric_rows(summary_metrics())
    }, ignoreInit = TRUE)

    observeEvent(input$recompute_all, {
      req(!isTRUE(bulk_recompute_active()))
      metrics <- summary_metrics()
      req(length(metrics) > 0)

      bulk_recompute_active(TRUE)
      on.exit(bulk_recompute_active(FALSE), add = TRUE)

      for (metric in metrics) {
        set_row_busy(metric, TRUE)
      }

      progress <- make_progress_notifier(
        total_steps = length(metrics) * 3L + 1L,
        message = "Recomputing reference curves"
      )
      tryCatch({
        recompute_metrics_from_summary(
          rv,
          metrics,
          mode = "summary",
          progress_cb = progress$update,
          on_metric_done = function(metric) {
            clear_metric_summary_edit_notes(rv, metric, "Phase 4")
            refresh_metric_row(metric)
            row_busy[[metric]] <- FALSE
          }
        )
      }, error = function(e) {
        showNotification(
          paste0("Recompute all failed: ", conditionMessage(e)),
          type = "error",
          duration = 8
        )
      }, finally = {
        progress$close()
      })

      refresh_metric_rows(metrics)
      for (metric in metrics) {
        row_busy[[metric]] <- FALSE
      }
    })

    observeEvent(input$open_summary_export, {
      launch_workspace_modal(rv, "summary_export")
    }, ignoreInit = TRUE)

    output$summary_page <- renderUI({
      if (is.null(rv$data)) return(no_data_alert())

      metrics <- summary_metrics()

      tagList(
        explanation_card(
          "Reference Curves",
          p("Track progress across all reference curve metrics in one place."),
          p("Edit available stratifications and the Phase 1, Phase 2, and Phase 3 selections directly in the table."),
          p("Expand a row to review phase-grouped notes, open the phase workspace for that metric, and inspect the Phase 4 scoring curve and detailed statistics.")
        ),
        card(
          class = "summary-shell",
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
              tags$div(
                tags$strong("Cross-metric summary table"),
                tags$div(class = "text-muted small", paste(length(metrics), "metrics"))
              ),
              div(
                class = "d-flex align-items-center gap-2 flex-wrap",
                actionButton(
                  ns("open_summary_export"),
                  "Summary / Export",
                  class = "btn btn-outline-secondary"
                ),
                actionButton(
                  ns("recompute_all"),
                  "Recompute All Rows",
                  class = "btn btn-primary",
                  disabled = if (isTRUE(bulk_recompute_active())) "disabled" else NULL
                )
              )
            )
          ),
          card_body(
            div(
              class = "summary-table-wrapper",
              tags$table(
                class = "table table-sm summary-progress-table align-middle",
                tags$thead(
                  tags$tr(
                    tags$th("Details"),
                    tags$th("Metric"),
                    tags$th("Family"),
                    tags$th("Units"),
                    tags$th("Direction"),
                    tags$th("n obs"),
                    tags$th(class = "summary-col-status", "Status"),
                    tags$th(class = "summary-col-available", "Stratifications Available"),
                    tags$th(class = "summary-col-phase1", "Phase 1 Candidates"),
                    tags$th(class = "summary-col-phase2", "Phase 2 Passed"),
                    tags$th(class = "summary-col-phase3", "Phase 3 Selected"),
                    tags$th("Recompute")
                  )
                ),
                lapply(metrics, function(metric) {
                  htmlOutput(
                    ns(paste0("row_", metric)),
                    container = function(...) tags$tbody(...)
                  )
                })
              )
            )
          )
        )
      )
    })
  })
}
