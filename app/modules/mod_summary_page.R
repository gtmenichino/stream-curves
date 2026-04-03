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
    row_snapshot_signature <- reactiveValues()
    row_expanded <- reactiveValues()
    row_busy <- reactiveValues()
    bulk_recompute_active <- reactiveVal(FALSE)
    bulk_recompute_phase <- reactiveVal("idle")
    pending_bulk_recompute <- reactiveVal(NULL)
    pending_row_recompute <- reactiveVal(NULL)

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
        phase1 = paste0("Exploratory 1/4: Screening ", index, "/", total, " - ", metric_label),
        phase2 = "Cross-Metric Analysis 2/4: Updating consistency results",
        phase3 = paste0("Verification 3/4: Refreshing diagnostics ", index, "/", total, " - ", metric_label),
        phase4 = paste0("Reference Curves 4/4: Building curve ", index, "/", total, " - ", metric_label),
        "Working..."
      )
    }

    make_progress_notifier <- function(total_steps, message) {
      progress <- shiny::Progress$new(session, min = 0, max = total_steps)
      current_step <- 0L
      current_message <- message
      current_detail <- NULL
      progress$set(message = current_message, detail = current_detail, value = current_step)

      list(
        update = function(stage, metric = NULL, index = 1L, total = 1L, state = "start") {
          current_detail <<- summary_progress_detail(stage, metric, index, total)
          if (identical(state, "start")) {
            progress$set(message = current_message, detail = current_detail, value = current_step)
          } else {
            current_step <<- current_step + 1L
            progress$set(message = current_message, detail = current_detail, value = current_step)
          }
          invisible(NULL)
        },
        set_status = function(message = NULL, detail = current_detail) {
          if (!is.null(message)) {
            current_message <<- message
          }
          current_detail <<- detail
          progress$set(message = current_message, detail = current_detail, value = current_step)
          invisible(NULL)
        },
        close = function() {
          progress$close()
          invisible(NULL)
        }
      )
    }

    show_bulk_refresh_final_loading <- function(detail) {
      show_final_loading_notification(
        session,
        ns("bulk_refresh_final_loading"),
        "Recomputing reference curves",
        detail
      )
      invisible(NULL)
    }

    remove_bulk_refresh_final_loading <- function() {
      remove_final_loading_notification(session, ns("bulk_refresh_final_loading"))
      invisible(NULL)
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

    clear_reference_curve_notes <- function(metrics) {
      for (metric in metrics %||% character(0)) {
        clear_metric_summary_edit_notes(rv, metric, "Reference Curves")
      }
      invisible(NULL)
    }

    refresh_metric_row <- function(metric, context = NULL, force = FALSE) {
      if (!(metric %in% summary_metrics())) return(invisible(NULL))
      resolved_context <- context %||% build_summary_snapshot_context(rv)
      next_signature <- build_metric_summary_snapshot_signature(
        rv,
        metric,
        context = resolved_context
      )

      if (!isTRUE(force) && identical(row_snapshot_signature[[metric]], next_signature)) {
        return(invisible(row_snapshot[[metric]]))
      }

      row_snapshot[[metric]] <- build_metric_summary_snapshot(
        rv,
        metric,
        context = resolved_context
      )
      row_snapshot_signature[[metric]] <- next_signature
      invisible(row_snapshot[[metric]])
    }

    refresh_metric_rows <- function(metrics = summary_metrics(), force = FALSE) {
      resolved_context <- build_summary_snapshot_context(rv)
      for (metric in metrics) {
        refresh_metric_row(metric, context = resolved_context, force = force)
      }
      invisible(NULL)
    }

    run_row_recompute <- function(metric) {
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
        clear_reference_curve_notes(metric)
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
      invisible(NULL)
    }

    run_bulk_recompute <- function(metrics) {
      metrics <- intersect(metrics %||% character(0), summary_metrics())
      if (length(metrics) == 0) {
        showNotification(
          "No rows selected for recompute. Manual curves were preserved.",
          type = "message",
          duration = 4
        )
        refresh_metric_rows(summary_metrics())
        return(invisible(NULL))
      }

      remove_bulk_refresh_final_loading()
      bulk_recompute_active(TRUE)
      bulk_recompute_phase("running")

      for (metric in metrics) {
        set_row_busy(metric, TRUE)
      }

      progress <- make_progress_notifier(
        total_steps = length(metrics) * 3L + 1L,
        message = "Recomputing reference curves"
      )

      finalize_bulk_recompute <- function() {
        bulk_recompute_phase("refreshing")
        progress$close()
        show_bulk_refresh_final_loading(
          "Loading page, please wait. Refreshing reference curve analysis table."
        )

        session$onFlushed(function() {
          remove_bulk_refresh_final_loading()
          bulk_recompute_active(FALSE)
          bulk_recompute_phase("idle")
          for (metric in metrics) {
            row_busy[[metric]] <- FALSE
          }
        }, once = TRUE)

        tryCatch({
          refresh_metric_rows(summary_metrics())
        }, error = function(e) {
          showNotification(
            paste0("Reference Curves refresh failed: ", conditionMessage(e)),
            type = "error",
            duration = 8
          )
        })

        invisible(NULL)
      }

      tryCatch({
        recompute_metrics_from_summary(
          rv,
          metrics,
          mode = "summary",
          progress_cb = progress$update,
          on_metric_done = function(metric) {
            clear_reference_curve_notes(metric)
            row_busy[[metric]] <- FALSE
          }
        )
      }, error = function(e) {
        showNotification(
          paste0("Recompute all failed: ", conditionMessage(e)),
          type = "error",
          duration = 8
        )
      })

      finalize_bulk_recompute()
      invisible(NULL)
    }

    show_bulk_manual_recompute_modal <- function(plan) {
      pending_bulk_recompute(plan)

      manual_choices <- stats::setNames(
        plan$manual_info$metric,
        plan$manual_info$selection_label
      )

      showModal(modalDialog(
        title = "Manual Curves Will Be Overwritten",
        tags$p(
          "Select which manual-curve metrics you want to recompute. ",
          "Selected metrics will be overwritten with fresh auto-generated reference curves. ",
          "Unchecked manual-curve metrics will be skipped and preserved."
        ),
        if (length(plan$auto_metrics) > 0) {
          tags$p(
            class = "text-muted mb-3",
            paste0(
              length(plan$auto_metrics),
              " metric(s) without manual curves will still be recomputed automatically."
            )
          )
        },
        checkboxGroupInput(
          ns("bulk_manual_recompute_metrics"),
          "Manual-curve metrics to overwrite",
          choices = manual_choices,
          selected = character(0)
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_bulk_recompute"),
            "Recompute Selected Rows",
            class = "btn btn-primary"
          )
        ),
        size = "l"
      ))
    }

    show_row_manual_recompute_modal <- function(metric, manual_info) {
      pending_row_recompute(list(metric = metric, manual_info = manual_info))

      showModal(modalDialog(
        title = "Overwrite Manual Curve?",
        tags$p(
          paste0(
            manual_info$display_name,
            " has ",
            tolower(manual_info$summary_label %||% "a manual curve"),
            "."
          )
        ),
        tags$p("Recomputing this row will replace the current manual reference curve output with a fresh auto-generated curve."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_row_recompute"),
            "Overwrite And Recompute",
            class = "btn btn-primary"
          )
        )
      ))
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

    note_group_ui <- function(title, items) {
      div(
        class = "summary-note-group",
        div(
          class = "summary-note-heading",
          tags$h6(title)
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
      phase4 <- row_data$phase4 %||% list()
      plot_id <- ns(paste0("plot_", metric))

      plot_card <- card(
        class = "summary-detail-card mb-3",
        card_header("Reference Curve"),
        card_body(
          if (nrow(curve_rows) == 0) {
            div(class = "text-muted", "No current reference curve outputs. Use Recompute to build the curve.")
          } else {
            plotOutput(plot_id, height = "420px")
          }
        )
      )

      detail_tables <- if (nrow(curve_rows) <= 1) {
        card(
          class = "summary-detail-card mb-0",
          card_header("Reference Curve Details"),
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
                  `Metric Range(s)` = c(
                    reference_curve_row_range_display(curve_rows, "functioning"),
                    reference_curve_row_range_display(curve_rows, "at_risk"),
                    reference_curve_row_range_display(curve_rows, "not_functioning")
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
          card_header("Reference Curve Details by Stratum"),
          card_body(
            plain_table_ui(build_stratified_stats_table(curve_rows)),
            tags$hr(),
            plain_table_ui(build_stratified_threshold_table(curve_rows))
          )
        )
      }

      note_card <- card(
        class = "summary-detail-card mb-3",
        card_header("Analysis Summary"),
        card_body(
          navset_card_tab(
            id = ns(paste0("detail_tabs_", metric)),
            nav_panel("Exploratory", note_group_ui("Exploratory", notes[["Exploratory"]])),
            nav_panel("Cross-Metric Analysis", note_group_ui("Cross-Metric Analysis", notes[["Cross-Metric Analysis"]])),
            nav_panel("Verification", note_group_ui("Verification", notes[["Verification"]])),
            nav_panel("Reference Curves", note_group_ui("Reference Curves", notes[["Reference Curves"]]))
          )
        )
      )

      selected_strat_label <- get_metric_curve_strat_label(rv, metric, row_data$curve_strat_used)

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
            tags$strong("Stratification used for curves: "),
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
          tags$td(
            div(
              tags$strong(row_data$display_name),
              if (isTRUE(row_data$has_manual_curve)) {
                div(
                  class = "text-warning small mt-1",
                  row_data$manual_curve_label
                )
              }
            )
          ),
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
            class = "summary-select-cell summary-col-curve",
            tags$fieldset(
              disabled = if (is_locked) "disabled" else NULL,
              selectInput(
                ns(paste0("curve_", metric)),
                label = NULL,
                choices = row_data$curve_strat_choices,
                selected = row_data$curve_strat_used,
                width = "100%"
              )
            )
          ),
          tags$td(
            actionButton(
              ns(paste0("open_analysis_", metric)),
              "Open Analysis",
              class = "btn btn-outline-secondary btn-sm",
              disabled = if (is_locked) "disabled" else NULL
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
            colspan = 11,
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

            observeEvent(input[[paste0("curve_", metric)]], {
              old_value <- row_snapshot[[metric]]$curve_strat_used %||% "none"
              new_value <- input[[paste0("curve_", metric)]] %||% "none"
              if (identical(old_value, new_value)) return(invisible(NULL))

              set_row_busy(metric, TRUE)
              set_metric_curve_stratification(rv, metric, new_value)
              clear_metric_summary_edit_notes(rv, metric, "Reference Curves")
              refresh_metric_row(metric)
              finish_row_update(metric)
            }, ignoreInit = TRUE)

            observeEvent(input[[paste0("recompute_", metric)]], {
              req(!isTRUE(bulk_recompute_active()))
              manual_info <- get_metric_phase4_manual_curve_info(rv, metric)

              if (isTRUE(manual_info$has_manual_curve)) {
                show_row_manual_recompute_modal(metric, manual_info)
                return(invisible(NULL))
              }

              run_row_recompute(metric)
            }, ignoreInit = TRUE)

            observeEvent(input[[paste0("open_analysis_", metric)]], {
              request_id <- next_workspace_modal_request_id(rv)
              metric_label <- rv$metric_config[[metric]]$display_name %||% metric
              root_session <- session$rootScope() %||% session

              show_analysis_launch_spinner_notification(
                root_session,
                request_id,
                paste0("Opening Analysis", if (!is.null(metric_label)) paste0(" - ", metric_label) else ""),
                "Loading page, please wait."
              )

              launch_workspace_modal(rv, "analysis", metric, request_id = request_id)
            }, ignoreInit = TRUE)

            output[[paste0("row_", metric)]] <- renderUI({
              row_data <- row_snapshot[[metric]]
              req(!is.null(row_data))
              render_metric_row(metric, row_data)
            })

            output[[paste0("plot_", metric)]] <- renderPlot({
              row_data <- row_snapshot[[metric]]
              req(!is.null(row_data), nrow(row_data$curve_rows) > 0)

              phase4 <- row_data$phase4 %||% list()
              curve_rows <- row_data$curve_rows

              if (nrow(curve_rows) > 1) {
                overlay <- build_overlay_curve_plot(curve_rows, rv$metric_config)
                if (!is.null(overlay)) return(overlay)

                for (lvl in names(phase4$stratum_results %||% list())) {
                  curve_plot <- phase4$stratum_results[[lvl]]$reference_curve$curve_plot %||% NULL
                  if (!is.null(curve_plot)) return(curve_plot)
                }
                return(NULL)
              }

              if (!is.null(phase4$reference_curve$curve_plot)) {
                phase4$reference_curve$curve_plot
              } else if (!is.null(phase4$stratum_results)) {
                first_result <- purrr::detect(phase4$stratum_results, ~ !is.null(.x$reference_curve$curve_plot))
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

    observeEvent(rv$workspace_refresh_nonce, {
      refresh_metric_rows(summary_metrics())
    }, ignoreInit = TRUE)

    observeEvent(input$recompute_all, {
      req(!isTRUE(bulk_recompute_active()))
      plan <- build_summary_recompute_plan(rv, summary_metrics())
      if (length(plan$manual_metrics) == 0) {
        run_bulk_recompute(summary_metrics())
        return(invisible(NULL))
      }

      show_bulk_manual_recompute_modal(plan)
    })

    observeEvent(input$confirm_bulk_recompute, {
      plan <- pending_bulk_recompute()
      req(plan)

      selected_manual_metrics <- input$bulk_manual_recompute_metrics %||% character(0)
      pending_bulk_recompute(NULL)
      removeModal()

      run_bulk_recompute(resolve_summary_recompute_metrics(
        auto_metrics = plan$auto_metrics,
        manual_metrics = plan$manual_metrics,
        selected_manual_metrics = selected_manual_metrics
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_row_recompute, {
      pending <- pending_row_recompute()
      req(pending, pending$metric)

      metric <- pending$metric
      pending_row_recompute(NULL)
      removeModal()

      run_row_recompute(metric)
    }, ignoreInit = TRUE)

    observeEvent(input$open_summary_export, {
      launch_workspace_modal(rv, "summary_export")
    }, ignoreInit = TRUE)

    output$bulk_refresh_status <- renderUI({
      if (!identical(bulk_recompute_phase(), "refreshing")) {
        return(NULL)
      }

      div(
        class = "alert alert-info d-flex align-items-center gap-2 mb-3",
        icon("spinner", class = "fa-spin"),
        tags$span("Loading page, please wait.")
      )
    })

    output$summary_page <- renderUI({
      if (is.null(rv$data)) return(no_data_alert())

      metrics <- summary_metrics()

      tagList(
        explanation_card(
          "Reference Curves",
          p("Track the continuous analysis workflow for all reference curve metrics in one place."),
          p("Edit the available stratifications and the stratification used for curves directly in the table."),
          p("Expand a row to review tabbed analysis summaries and inspect the current reference curve outputs for that metric.")
        ),
        card(
          class = "summary-shell",
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
              tags$div(
                tags$strong("Reference curve analysis table"),
                tags$div(class = "text-muted small", paste(length(metrics), "metrics"))
              ),
              div(
                class = "d-flex align-items-center gap-2 flex-wrap",
                actionButton(
                  ns("open_summary_export"),
                  "Export",
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
            uiOutput(ns("bulk_refresh_status")),
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
                    tags$th(class = "summary-col-curve", "Stratification Used for Curves"),
                    tags$th("Analysis"),
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
