## -- Module: Reference Curve ---------------------------------------------------
## IQR-seeded piecewise-linear scoring curve with optional manual point editing.

library(shiny)
library(bslib)
library(DT)

reference_curve_editor_table_df <- function(curve_points) {
  points <- normalize_reference_curve_points(curve_points)

  data.frame(
    point_order = seq_len(nrow(points)),
    metric_value = points$metric_value,
    index_score = points$index_score,
    stringsAsFactors = FALSE
  )
}

reference_curve_editor_points_from_table <- function(table_df) {
  if (is.null(table_df) || nrow(table_df) == 0) {
    return(empty_reference_curve_points())
  }

  tibble::tibble(
    point_order = seq_len(nrow(table_df)),
    metric_value = suppressWarnings(as.numeric(table_df$metric_value)),
    index_score = suppressWarnings(as.numeric(table_df$index_score))
  )
}

reference_curve_editor_move_row <- function(table_df, selected_row, direction = c("up", "down")) {
  direction <- match.arg(direction)

  if (is.null(table_df) || nrow(table_df) == 0) {
    return(list(
      table_df = table_df,
      selected_row = integer(0),
      changed = FALSE,
      status = "empty"
    ))
  }

  selected_row <- suppressWarnings(as.integer(selected_row %||% integer(0)))
  selected_row <- selected_row[!is.na(selected_row)]
  if (length(selected_row) == 0) {
    return(list(
      table_df = table_df,
      selected_row = integer(0),
      changed = FALSE,
      status = "no_selection"
    ))
  }

  current_row <- selected_row[[1]]
  if (current_row < 1L || current_row > nrow(table_df)) {
    return(list(
      table_df = table_df,
      selected_row = integer(0),
      changed = FALSE,
      status = "no_selection"
    ))
  }

  target_row <- if (identical(direction, "up")) current_row - 1L else current_row + 1L
  if (target_row < 1L || target_row > nrow(table_df)) {
    return(list(
      table_df = table_df,
      selected_row = current_row,
      changed = FALSE,
      status = "boundary"
    ))
  }

  row_index <- seq_len(nrow(table_df))
  row_index[c(current_row, target_row)] <- row_index[c(target_row, current_row)]
  moved_df <- table_df[row_index, , drop = FALSE]
  moved_df$point_order <- seq_len(nrow(moved_df))

  list(
    table_df = moved_df,
    selected_row = target_row,
    changed = TRUE,
    status = "moved"
  )
}

reference_curve_editor_seed_points <- function(result, higher_is_better) {
  if (!is.null(result$curve_points) &&
      nrow(normalize_reference_curve_points(result$curve_points)) >= 2) {
    return(normalize_reference_curve_points(result$curve_points))
  }

  if (!is.null(result$curve_row) && nrow(result$curve_row) > 0) {
    return(reference_curve_points_from_row(result$curve_row, higher_is_better))
  }

  empty_reference_curve_points()
}

mod_reference_curve_editor_ui <- function(id, title = "Manual Curve Editor") {
  ns <- NS(id)

  card(
    class = "mb-3",
    card_header(title),
    card_body(
      div(
        class = "text-muted small mb-3",
        "Edit metric-score and index-score points, then click Apply Curve Edits. ",
        "Row order is preserved when applied. ",
        "Index scores may move freely. ",
        "Metric scores must be non-decreasing from top to bottom, and equal consecutive values create a step. ",
        "Threshold tables can show multiple metric ranges when the curve crosses a threshold twice."
      ),
      uiOutput(ns("curve_source_note")),
      DT::DTOutput(ns("points_table")),
      uiOutput(ns("validation_message")),
      div(
        class = "d-flex flex-wrap gap-2 mt-3",
        actionButton(ns("add_point"), "Add Point", class = "btn btn-outline-secondary btn-sm"),
        actionButton(ns("remove_point"), "Remove Selected Point", class = "btn btn-outline-secondary btn-sm"),
        actionButton(ns("move_point_up"), "Move Selected Point Up", class = "btn btn-outline-secondary btn-sm"),
        actionButton(ns("move_point_down"), "Move Selected Point Down", class = "btn btn-outline-secondary btn-sm"),
        actionButton(ns("apply_points"), "Apply Curve Edits", class = "btn btn-primary btn-sm"),
        actionButton(ns("reset_points"), "Reset to Auto", class = "btn btn-outline-danger btn-sm")
      )
    )
  )
}

mod_reference_curve_editor_server <- function(id, current_result, higher_is_better,
                                              on_apply, on_reset) {
  moduleServer(id, function(input, output, session) {
    editor_table <- reactiveVal(reference_curve_editor_table_df(NULL))
    validation_message <- reactiveVal(NULL)
    selected_row <- reactiveVal(integer(0))
    proxy <- DT::dataTableProxy("points_table", session = session)

    set_selected_row <- function(row = integer(0)) {
      row <- suppressWarnings(as.integer(row %||% integer(0)))
      row <- row[!is.na(row) & row >= 1L]
      row <- if (length(row) > 0) row[[1]] else integer(0)
      selected_row(row)
      session$onFlushed(function() {
        try(DT::selectRows(proxy, if (length(row) == 0) NULL else row), silent = TRUE)
      }, once = TRUE)
      invisible(row)
    }

    observeEvent(current_result(), {
      result <- current_result()
      if (is.null(result)) {
        editor_table(reference_curve_editor_table_df(NULL))
        validation_message(NULL)
        set_selected_row(integer(0))
        return(invisible(NULL))
      }

      seed_points <- reference_curve_editor_seed_points(result, higher_is_better())
      editor_table(reference_curve_editor_table_df(seed_points))
      validation_message(NULL)
      set_selected_row(integer(0))
      invisible(NULL)
    }, ignoreInit = FALSE)

    observeEvent(input$points_table_rows_selected, {
      selected <- suppressWarnings(as.integer(input$points_table_rows_selected %||% integer(0)))
      selected <- selected[!is.na(selected)]
      selected_row(if (length(selected) > 0) selected[[1]] else integer(0))
      invisible(NULL)
    }, ignoreInit = FALSE)

    output$curve_source_note <- renderUI({
      result <- current_result()
      if (is.null(result)) {
        return(NULL)
      }

      curve_status <- result$curve_row$curve_status[1] %||% "complete"
      if (identical(curve_status, "insufficient_data")) {
        return(div(
          class = "alert alert-warning py-2 mb-3",
          "Manual editing is unavailable until this curve has enough reference data to seed a baseline curve."
        ))
      }

      source_label <- if (identical(result$curve_source %||% "auto", "manual")) {
        "Manual edits active"
      } else {
        "Auto-generated curve"
      }

      div(
        class = if (identical(result$curve_source %||% "auto", "manual")) {
          "alert alert-warning py-2 mb-3"
        } else {
          "alert alert-secondary py-2 mb-3"
        },
        tags$strong("Current source: "),
        source_label,
        if (identical(result$curve_source %||% "auto", "manual")) {
          tags$span(" Plots, thresholds, downloads, and summary views use the edited curve.")
        }
      )
    })

    output$points_table <- DT::renderDT({
      table_df <- editor_table()

      DT::datatable(
        table_df,
        rownames = FALSE,
        selection = "single",
        editable = list(target = "cell", disable = list(columns = c(0))),
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          searching = FALSE
        ),
        class = "compact table table-sm",
        colnames = c("Point", "Metric Score", "Index Score")
      )
    })

    observeEvent(input$points_table_cell_edit, {
      updated <- DT::editData(
        editor_table(),
        input$points_table_cell_edit,
        proxy,
        rownames = FALSE,
        resetPaging = FALSE
      )
      updated$point_order <- seq_len(nrow(updated))
      editor_table(updated)
      validation_message(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$add_point, {
      table_df <- editor_table()
      table_df <- rbind(
        table_df,
        data.frame(
          point_order = nrow(table_df) + 1L,
          metric_value = NA_real_,
          index_score = NA_real_,
          stringsAsFactors = FALSE
        )
      )
      editor_table(table_df)
      validation_message("Fill in the new point and click Apply Curve Edits.")
    }, ignoreInit = TRUE)

    observeEvent(input$remove_point, {
      selected <- selected_row()
      if (length(selected) == 0) {
        showNotification("Select a point row to remove.", type = "warning", duration = 4)
        return()
      }

      table_df <- editor_table()
      if (nrow(table_df) <= 2) {
        validation_message("At least 2 curve points are required.")
        return()
      }

      table_df <- table_df[-selected[1], , drop = FALSE]
      table_df$point_order <- seq_len(nrow(table_df))
      editor_table(table_df)
      validation_message(NULL)
      set_selected_row(min(selected[1], nrow(table_df)))
    }, ignoreInit = TRUE)

    observeEvent(input$move_point_up, {
      move_result <- reference_curve_editor_move_row(
        editor_table(),
        selected_row(),
        direction = "up"
      )

      if (identical(move_result$status, "no_selection")) {
        showNotification("Select a point row to move.", type = "warning", duration = 4)
        return()
      }

      if (identical(move_result$status, "boundary")) {
        showNotification("Selected point is already at the top.", type = "warning", duration = 4)
        set_selected_row(move_result$selected_row)
        return()
      }

      editor_table(move_result$table_df)
      validation_message(NULL)
      set_selected_row(move_result$selected_row)
    }, ignoreInit = TRUE)

    observeEvent(input$move_point_down, {
      move_result <- reference_curve_editor_move_row(
        editor_table(),
        selected_row(),
        direction = "down"
      )

      if (identical(move_result$status, "no_selection")) {
        showNotification("Select a point row to move.", type = "warning", duration = 4)
        return()
      }

      if (identical(move_result$status, "boundary")) {
        showNotification("Selected point is already at the bottom.", type = "warning", duration = 4)
        set_selected_row(move_result$selected_row)
        return()
      }

      editor_table(move_result$table_df)
      validation_message(NULL)
      set_selected_row(move_result$selected_row)
    }, ignoreInit = TRUE)

    output$validation_message <- renderUI({
      msg <- validation_message()
      if (is.null(msg) || !nzchar(msg)) {
        return(NULL)
      }

      div(class = "alert alert-danger py-2 mt-3 mb-0", msg)
    })

    observeEvent(input$apply_points, {
      result <- current_result()
      if (is.null(result) || identical(result$curve_row$curve_status[1] %||% "", "insufficient_data")) {
        validation_message("Manual editing is unavailable until the curve has enough reference data.")
        return()
      }

      points <- reference_curve_editor_points_from_table(editor_table())
      validation <- validate_reference_curve_points(points, higher_is_better())

      if (!validation$valid) {
        validation_message(paste(validation$errors, collapse = " "))
        return()
      }

      tryCatch({
        on_apply(validation$points)
        validation_message(NULL)
      }, error = function(e) {
        validation_message(conditionMessage(e))
      })
    }, ignoreInit = TRUE)

    observeEvent(input$reset_points, {
      validation_message(NULL)
      tryCatch({
        on_reset()
      }, error = function(e) {
        validation_message(conditionMessage(e))
      })
    }, ignoreInit = TRUE)
  })
}

mod_ref_curve_ui <- function(id) {
  ns <- NS(id)

  tagList(
    explanation_card(
      "Empirical Scoring Curve",
      p("The reference curve is seeded from the empirical distribution of reference-standard sites and can be manually revised below by editing metric/index score points."),
      p("Functional categories: ",
        tags$span(class = "text-success fw-bold", "Functioning (0.70-1.00)"), " | ",
        tags$span(class = "text-warning fw-bold", "At-Risk (0.30-0.69)"), " | ",
        tags$span(class = "text-danger fw-bold", "Not Functioning (0.00-0.29)"))
    ),

    uiOutput(ns("curve_ui"))
  )
}

mod_ref_curve_server <- function(id, rv, workspace_scope = "standalone") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    workspace_scope <- match.arg(workspace_scope, c("standalone", "analysis"))

    ref_curve_workspace_active <- function(isolate_state = FALSE) {
      workspace_scope_is_active(
        rv,
        workspace_scope = workspace_scope,
        standalone_modal_type = "phase4",
        isolate_state = isolate_state
      )
    }

    persist_reference_curve_result <- function(result) {
      if (!isTRUE(ref_curve_workspace_active(isolate_state = TRUE))) {
        return(invisible(NULL))
      }

      metric <- shiny::isolate(rv$current_metric)
      if (is.null(metric) || identical(metric, "")) {
        return(invisible(NULL))
      }
      decision_tbl <- shiny::isolate(get_metric_phase4_decision_state(rv, metric))
      normalized_result <- hydrate_reference_curve_result(
        result,
        shiny::isolate(rv$phase4_data %||% rv$data),
        metric,
        shiny::isolate(rv$metric_config),
        stratum_label = shiny::isolate(rv$current_stratum_level),
        artifact_mode = "full"
      )

      rv$reference_curve <- normalized_result
      curve_rows <- extract_metric_phase4_curve_rows(list(reference_curve = normalized_result))
      phase4_signature <- cache_metric_phase4_results(
        rv,
        metric,
        decision_tbl = decision_tbl,
        reference_curve = normalized_result,
        artifact_mode = "full"
      )

      if (metric %in% names(rv$completed_metrics)) {
        update_metric_phase4_completed_entry(rv, metric, list(
          strat_decision = rv$strat_decision_user,
          reference_curve = normalized_result,
          phase4_signature = phase4_signature,
          phase4_artifact_mode = "full",
          phase4_curve_rows = curve_rows
        ))
      }

      invisible(normalized_result)
    }

    build_auto_curve_result <- function() {
      if (!isTRUE(ref_curve_workspace_active(isolate_state = TRUE))) {
        return(NULL)
      }

      metric <- shiny::isolate(rv$current_metric)
      analysis_data <- shiny::isolate(rv$phase4_data %||% rv$data)

      build_reference_curve(
        analysis_data,
        metric,
        shiny::isolate(rv$metric_config),
        stratum_label = shiny::isolate(rv$current_stratum_level)
      )
    }

    build_manual_curve_result <- function(points) {
      if (!isTRUE(ref_curve_workspace_active(isolate_state = TRUE))) {
        return(NULL)
      }

      metric <- shiny::isolate(rv$current_metric)
      analysis_data <- shiny::isolate(rv$phase4_data %||% rv$data)

      build_reference_curve_from_points(
        analysis_data,
        metric,
        shiny::isolate(rv$metric_config),
        curve_points = points,
        stratum_label = shiny::isolate(rv$current_stratum_level)
      )
    }

    curve_results <- reactive({
      req(isTRUE(ref_curve_workspace_active()))
      req(rv$current_metric)
      metric <- rv$current_metric
      decision_tbl <- get_metric_phase4_decision_state(rv, metric)

      if (is.null(rv$current_stratum_level) &&
          metric_has_phase4_cache(rv, metric, decision_tbl, artifact_mode = "summary")) {
        cached <- get_metric_phase4_cached_result(rv, metric)
        if (!is.null(cached$reference_curve)) {
          return(hydrate_reference_curve_result(
            cached$reference_curve,
            rv$phase4_data %||% rv$data,
            metric,
            rv$metric_config,
            stratum_label = rv$current_stratum_level,
            artifact_mode = cached$artifact_mode %||% "full"
          ))
        }
      }

      hydrate_reference_curve_result(
        build_auto_curve_result(),
        rv$phase4_data %||% rv$data,
        metric,
        rv$metric_config,
        stratum_label = rv$current_stratum_level
      )
    })

    mod_reference_curve_editor_server(
      "curve_editor",
      current_result = reactive(curve_results()),
      higher_is_better = reactive({
        req(rv$current_metric)
        rv$metric_config[[rv$current_metric]]$higher_is_better
      }),
      on_apply = function(points) {
        result <- build_manual_curve_result(points)
        persist_reference_curve_result(result)
        notify_workspace_refresh(rv)
        showNotification("Manual curve edits applied.", type = "message", duration = 4)
      },
      on_reset = function() {
        result <- build_auto_curve_result()
        persist_reference_curve_result(result)
        notify_workspace_refresh(rv)
        showNotification("Reference curve reset to the auto-generated baseline.", type = "message", duration = 4)
      }
    )

    output$curve_ui <- renderUI({
      if (!isTRUE(ref_curve_workspace_active())) {
        return(NULL)
      }

      res <- curve_results()
      req(res)

      mc <- rv$metric_config[[rv$current_metric]]

      tagList(
        layout_column_wrap(
          width = 1 / 2,

          if (!is.null(res$bar_chart_plot)) {
            card(
              card_header("Reference Distribution"),
              card_body(plotOutput(ns("bar_chart"), height = "400px"))
            )
          },

          if (!is.null(res$curve_plot)) {
            card(
              card_header("Scoring Curve"),
              card_body(plotOutput(ns("curve_plot"), height = "400px"))
            )
          }
        ),

        mod_reference_curve_editor_ui(ns("curve_editor")),

        card(
          card_header("Descriptive Statistics"),
          card_body(
            layout_column_wrap(
              width = 1 / 4,
              div(tags$strong("n"), tags$br(), tags$span(class = "fs-4", res$curve_row$n_reference)),
              div(tags$strong("Min"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$min_val, 2))),
              div(tags$strong("Q25"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$q25, 2))),
              div(tags$strong("Median"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$median_val, 2))),
              div(tags$strong("Q75"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$q75, 2))),
              div(tags$strong("Max"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$max_val, 2))),
              div(tags$strong("IQR"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$iqr, 2))),
              div(tags$strong("SD"), tags$br(), tags$span(class = "fs-5", round(res$curve_row$sd_val, 2)))
            )
          )
        ),

        card(
          card_header("Key Statistics"),
          card_body(
            layout_column_wrap(
              width = 1 / 4,
              div(
                tags$strong("Sample Size"), tags$br(),
                tags$span(class = "fs-5", res$curve_row$n_reference)
              ),
              div(
                tags$strong("Stratification"), tags$br(),
                tags$span(class = "fs-5", {
                  if (!is.null(rv$current_stratum_level)) {
                    strat <- rv$strat_decision_user
                    strat_name <- if (!is.null(strat) && strat$decision_type == "single") {
                      rv$strat_config[[strat$selected_strat]]$display_name %||% strat$selected_strat
                    } else {
                      "Subset"
                    }
                    paste0(strat_name, ": ", rv$current_stratum_level)
                  } else {
                    strat <- rv$strat_decision_user
                    if (!is.null(strat) && strat$decision_type == "single") {
                      rv$strat_config[[strat$selected_strat]]$display_name %||% strat$selected_strat
                    } else {
                      "None"
                    }
                  }
                })
              ),
              div(
                tags$strong("Curve Source"), tags$br(),
                tags$span(class = "fs-5", if (identical(res$curve_source %||% "auto", "manual")) "Manual" else "Auto")
              ),
              div(
                tags$strong("Curve Status"), tags$br(),
                status_badge(
                  if (res$curve_row$curve_status == "complete") "pass" else "fail",
                  res$curve_row$curve_status
                )
              )
            )
          )
        ),

        card(
          card_header("Scoring Thresholds"),
          card_body(DT::DTOutput(ns("threshold_table")))
        ),

        card(
          card_header("Curve Details"),
          card_body(
            tags$table(
              class = "table table-sm table-bordered",
              tags$tbody(
                tags$tr(tags$td(tags$strong("Q25")), tags$td(round(res$curve_row$q25, 3))),
                tags$tr(tags$td(tags$strong("Q75")), tags$td(round(res$curve_row$q75, 3))),
                tags$tr(tags$td(tags$strong("IQR")), tags$td(round(res$curve_row$iqr, 3))),
                tags$tr(tags$td(tags$strong("Direction")),
                        tags$td(if (isTRUE(mc$higher_is_better)) "Higher is better" else "Lower is better")),
                tags$tr(tags$td(tags$strong("Curve points")), tags$td(res$curve_row$curve_n_points)),
                tags$tr(tags$td(tags$strong("Source")), tags$td(if (identical(res$curve_source %||% "auto", "manual")) "Manual" else "Auto")),
                tags$tr(tags$td(tags$strong("n (reference)")), tags$td(res$curve_row$n_reference)),
                tags$tr(tags$td(tags$strong("Status")),
                        tags$td(status_badge(
                          if (res$curve_row$curve_status == "complete") "pass" else "fail",
                          res$curve_row$curve_status
                        )))
              )
            )
          )
        ),

        div(
          class = "d-flex gap-2 mb-3",
          downloadButton(ns("dl_curve_plot"), "Download Curve Plot (PNG)",
                         class = "btn btn-outline-secondary btn-sm"),
          downloadButton(ns("dl_curve_table"), "Download Curve Table (CSV)",
                         class = "btn btn-outline-secondary btn-sm")
        ),

        div(
          class = "d-flex justify-content-end mt-3",
          actionButton(ns("mark_complete"), "Mark Metric Complete ✓",
                       class = "btn btn-success btn-proceed",
                       icon = icon("check"))
        )
      )
    })

    output$bar_chart <- renderPlot({
      req(isTRUE(ref_curve_workspace_active()))
      res <- curve_results()
      req(res, !is.null(res$bar_chart_plot))
      res$bar_chart_plot
    })

    output$curve_plot <- renderPlot({
      req(isTRUE(ref_curve_workspace_active()))
      res <- curve_results()
      req(res, !is.null(res$curve_plot))
      res$curve_plot
    })

    output$threshold_table <- DT::renderDT({
      req(isTRUE(ref_curve_workspace_active()))
      res <- curve_results()
      req(res)
      cr <- res$curve_row

      threshold_df <- data.frame(
        Category = c("Functioning", "At-Risk", "Not Functioning"),
        `Score Range` = c("0.70 - 1.00", "0.30 - 0.69", "0.00 - 0.29"),
        `Metric Range(s)` = c(
          reference_curve_row_range_display(cr, "functioning"),
          reference_curve_row_range_display(cr, "at_risk"),
          reference_curve_row_range_display(cr, "not_functioning")
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      DT::datatable(
        threshold_df,
        options = list(dom = "t", paging = FALSE),
        rownames = FALSE,
        class = "compact"
      ) |>
        DT::formatStyle(
          "Category",
          backgroundColor = DT::styleEqual(
            c("Functioning", "At-Risk", "Not Functioning"),
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)", "rgba(231,76,60,0.15)")
          )
        )
    })

    output$dl_curve_plot <- downloadHandler(
      filename = function() {
        paste0(rv$current_metric, "_reference_curve_", format(Sys.time(), "%Y%m%d"), ".png")
      },
      content = function(file) {
        res <- curve_results()
        if (!is.null(res) && !is.null(res$curve_plot)) {
          ggplot2::ggsave(file, res$curve_plot, width = 8, height = 6, dpi = 300)
        }
      }
    )

    output$dl_curve_table <- downloadHandler(
      filename = function() {
        paste0(rv$current_metric, "_reference_curve_", format(Sys.time(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        res <- curve_results()
        if (!is.null(res) && !is.null(res$curve_row)) {
          write.csv(reference_curve_rows_for_export(res$curve_row), file, row.names = FALSE)
        }
      }
    )

    observeEvent(input$mark_complete, {
      req(rv$current_metric)
      mk <- rv$current_metric
      current_result <- curve_results()
      req(current_result)

      if (!is.null(rv$current_stratum_level)) {
        showNotification(
          paste0(rv$metric_config[[mk]]$display_name, " - ",
                 rv$current_stratum_level, " stratum curve complete!"),
          type = "message", duration = 3
        )
        return()
      }

      phase4_signature <- cache_metric_phase4_results(
        rv,
        mk,
        decision_tbl = get_metric_phase4_decision_state(rv, mk),
        reference_curve = current_result,
        artifact_mode = "full"
      )
      curve_rows <- extract_metric_phase4_curve_rows(list(reference_curve = current_result))
      update_metric_phase4_completed_entry(rv, mk, list(
        strat_decision = rv$strat_decision_user,
        reference_curve = current_result,
        phase4_signature = phase4_signature,
        phase4_artifact_mode = "full",
        phase4_curve_rows = curve_rows
      ))

      showNotification(
        paste0(rv$metric_config[[mk]]$display_name, " marked complete!"),
        type = "message", duration = 3
      )
      notify_workspace_refresh(rv)
    })

    list(
      complete = reactive(input$mark_complete),
      back = reactive(NULL)
    )
  })
}
