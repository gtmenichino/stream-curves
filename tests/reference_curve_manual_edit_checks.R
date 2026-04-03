suppressPackageStartupMessages({
  library(shiny)
  library(tibble)
  library(dplyr)
})

source("R/00_plot_theme.R", local = TRUE)
source("R/10_reference_curves.R", local = TRUE)
source("app/modules/mod_ref_curve.R", local = TRUE)

metric_config <- list(
  metric_hi = list(
    display_name = "Metric Hi",
    column_name = "metric_hi",
    higher_is_better = TRUE,
    units = "%"
  ),
  metric_lo = list(
    display_name = "Metric Lo",
    column_name = "metric_lo",
    higher_is_better = FALSE,
    units = "%"
  )
)

data <- tibble(
  metric_hi = seq(5, 60, by = 5),
  metric_lo = seq(5, 60, by = 5)
)

check_manual_higher_curve_thresholds <- function() {
  points <- tibble(
    point_order = 1:5,
    metric_value = c(0, 10, 20, 30, 40),
    index_score = c(0.00, 0.30, 0.70, 1.00, 1.00)
  )

  result <- build_reference_curve_from_points(data, "metric_hi", metric_config, points)
  row <- result$curve_row

  stopifnot(identical(result$curve_source, "manual"))
  stopifnot(row$curve_n_points[[1]] == 5L)
  stopifnot(identical(row$curve_source[[1]], "manual"))
  stopifnot(identical(row$score_30_metric[[1]], 10))
  stopifnot(identical(row$score_70_metric[[1]], 20))
  stopifnot(identical(row$score_30_crossings[[1]], 10))
  stopifnot(identical(row$score_70_crossings[[1]], 20))
  stopifnot(identical(row$functioning_min[[1]], 20))
  stopifnot(identical(row$functioning_max[[1]], 30))
  stopifnot(identical(row$at_risk_min[[1]], 10))
  stopifnot(identical(row$at_risk_max[[1]], 20))
  stopifnot(identical(row$not_functioning_min[[1]], 0))
  stopifnot(identical(row$not_functioning_max[[1]], 10))
  stopifnot(identical(row$functioning_ranges_display[[1]], "20.00 - 40.00"))
  stopifnot(identical(row$at_risk_ranges_display[[1]], "10.00 - 20.00"))
  stopifnot(identical(row$not_functioning_ranges_display[[1]], "0.00 - 10.00"))
}

check_manual_lower_curve_thresholds <- function() {
  points <- tibble(
    point_order = 1:5,
    metric_value = c(0, 10, 20, 30, 40),
    index_score = c(1.00, 1.00, 0.70, 0.30, 0.00)
  )

  result <- build_reference_curve_from_points(data, "metric_lo", metric_config, points)
  row <- result$curve_row

  stopifnot(identical(row$score_70_metric[[1]], 20))
  stopifnot(identical(row$score_30_metric[[1]], 30))
  stopifnot(identical(row$score_70_crossings[[1]], 20))
  stopifnot(identical(row$score_30_crossings[[1]], 30))
  stopifnot(identical(row$functioning_min[[1]], 10))
  stopifnot(identical(row$functioning_max[[1]], 20))
  stopifnot(identical(row$at_risk_min[[1]], 20))
  stopifnot(identical(row$at_risk_max[[1]], 30))
  stopifnot(identical(row$not_functioning_min[[1]], 30))
  stopifnot(identical(row$not_functioning_max[[1]], 40))
  stopifnot(identical(row$functioning_ranges_display[[1]], "0.00 - 20.00"))
  stopifnot(identical(row$at_risk_ranges_display[[1]], "20.00 - 30.00"))
  stopifnot(identical(row$not_functioning_ranges_display[[1]], "30.00 - 40.00"))
}

check_curve_normalization_preserves_manual_order <- function() {
  points <- tibble(
    point_order = 1:4,
    metric_value = c(20, 10, 10, 40),
    index_score = c(0.70, 0.30, 0.70, 1.00)
  )

  normalized <- normalize_reference_curve_points(points)

  stopifnot(identical(normalized$point_order, 1:4))
  stopifnot(identical(normalized$metric_value, c(20, 10, 10, 40)))
  stopifnot(identical(normalized$index_score, c(0.70, 0.30, 0.70, 1.00)))
}

check_step_curve_thresholds <- function() {
  points <- tibble(
    point_order = 1:5,
    metric_value = c(0, 10, 20, 20, 40),
    index_score = c(0.00, 0.30, 0.70, 1.00, 1.00)
  )

  validation <- validate_reference_curve_points(points, TRUE)
  stopifnot(isTRUE(validation$valid))

  result <- build_reference_curve_from_points(data, "metric_hi", metric_config, points)
  row <- result$curve_row

  stopifnot(identical(row$curve_n_points[[1]], 5L))
  stopifnot(identical(row$score_30_metric[[1]], 10))
  stopifnot(identical(row$score_70_metric[[1]], 20))
  stopifnot(identical(row$functioning_min[[1]], 20))
  stopifnot(identical(row$functioning_max[[1]], 20))
  stopifnot(identical(reference_curve_row_range_display(row, "functioning"), "20.00 - 40.00"))
  stopifnot(identical(row$functioning_ranges_display[[1]], "20.00 - 40.00"))
}

check_double_crossing_curve_ranges <- function() {
  points <- tibble(
    point_order = 1:3,
    metric_value = c(0, 10, 20),
    index_score = c(0.00, 1.00, 0.00)
  )

  validation <- validate_reference_curve_points(points, TRUE)
  stopifnot(isTRUE(validation$valid))

  result <- build_reference_curve_from_points(data, "metric_hi", metric_config, points)
  row <- result$curve_row

  stopifnot(identical(row$score_30_crossing_count[[1]], 2L))
  stopifnot(identical(row$score_70_crossing_count[[1]], 2L))
  stopifnot(isTRUE(all.equal(row$score_30_crossings[[1]], c(3, 17))))
  stopifnot(isTRUE(all.equal(row$score_70_crossings[[1]], c(7, 13))))
  stopifnot(identical(row$functioning_ranges_display[[1]], "7.00 - 13.00"))
  stopifnot(identical(row$at_risk_ranges_display[[1]], "3.00 - 7.00, 13.00 - 17.00"))
  stopifnot(identical(row$not_functioning_ranges_display[[1]], "0.00 - 3.00, 17.00 - 20.00"))
}

check_curve_validation_rejects_bad_points <- function() {
  non_monotone_index <- tibble(
    metric_value = c(0, 10, 20, 30, 40),
    index_score = c(0.00, 0.85, 0.75, 0.92, 1.00)
  )

  monotone_check <- validate_reference_curve_points(non_monotone_index, TRUE)
  stopifnot(isTRUE(monotone_check$valid))

  bad_threshold_span <- tibble(
    metric_value = c(0, 10, 20),
    index_score = c(0.40, 0.60, 0.80)
  )

  span_check <- validate_reference_curve_points(bad_threshold_span, TRUE)
  stopifnot(!isTRUE(span_check$valid))
  stopifnot(any(grepl("0.30 and 0.70", span_check$errors, fixed = TRUE)))

  bad_metric_order <- tibble(
    metric_value = c(0, 20, 10),
    index_score = c(0.00, 0.70, 0.80)
  )

  order_check <- validate_reference_curve_points(bad_metric_order, TRUE)
  stopifnot(!isTRUE(order_check$valid))
  stopifnot(any(grepl("Metric score must be non-decreasing from top to bottom", order_check$errors, fixed = TRUE)))

  too_many_crossings <- tibble(
    metric_value = c(0, 10, 20, 30, 40),
    index_score = c(0.00, 1.00, 0.00, 1.00, 0.00)
  )

  multi_cross_check <- validate_reference_curve_points(too_many_crossings, TRUE)
  stopifnot(!isTRUE(multi_cross_check$valid))
  stopifnot(any(grepl("index score 0.30 at most twice", multi_cross_check$errors, fixed = TRUE)))
}

check_unsupported_multi_crossing_status <- function() {
  points <- tibble(
    point_order = 1:5,
    metric_value = c(0, 10, 20, 30, 40),
    index_score = c(0.00, 1.00, 0.00, 1.00, 0.00)
  )

  result <- build_reference_curve_from_components(
    data = data,
    metric_key = "metric_hi",
    metric_config = metric_config,
    curve_points = points,
    curve_source = "auto",
    curve_status = "complete"
  )

  stopifnot(identical(result$curve_row$curve_status[[1]], "unsupported_multi_crossing"))
}

check_export_rows_drop_curve_points <- function() {
  result <- build_reference_curve_from_points(
    data,
    "metric_hi",
    metric_config,
    tibble(
      point_order = 1:3,
      metric_value = c(0, 10, 20),
      index_score = c(0.00, 1.00, 0.00)
    )
  )
  export_rows <- reference_curve_rows_for_export(result$curve_row)

  stopifnot(!("curve_points" %in% names(export_rows)))
  stopifnot("curve_n_points" %in% names(export_rows))
  stopifnot(is.character(export_rows$score_30_crossings))
  stopifnot(is.character(export_rows$functioning_ranges))
  stopifnot(identical(export_rows$score_30_crossings[[1]], "3.00, 17.00"))
  stopifnot(identical(export_rows$functioning_ranges[[1]], "7.00 - 13.00"))
}

check_editor_move_row_helper <- function() {
  table_df <- data.frame(
    point_order = 1:4,
    metric_value = c(0, 10, 20, 30),
    index_score = c(0.00, 0.30, 0.70, 1.00),
    stringsAsFactors = FALSE
  )

  moved_up <- reference_curve_editor_move_row(table_df, 3L, direction = "up")
  stopifnot(isTRUE(moved_up$changed))
  stopifnot(identical(moved_up$selected_row, 2L))
  stopifnot(identical(moved_up$table_df$metric_value, c(0, 20, 10, 30)))

  moved_down <- reference_curve_editor_move_row(table_df, 2L, direction = "down")
  stopifnot(isTRUE(moved_down$changed))
  stopifnot(identical(moved_down$selected_row, 3L))
  stopifnot(identical(moved_down$table_df$metric_value, c(0, 20, 10, 30)))

  top_boundary <- reference_curve_editor_move_row(table_df, 1L, direction = "up")
  stopifnot(!isTRUE(top_boundary$changed))
  stopifnot(identical(top_boundary$status, "boundary"))
  stopifnot(identical(top_boundary$table_df$metric_value, table_df$metric_value))

  no_selection <- reference_curve_editor_move_row(table_df, integer(0), direction = "down")
  stopifnot(!isTRUE(no_selection$changed))
  stopifnot(identical(no_selection$status, "no_selection"))
}

check_reference_curve_editor_server_moves_and_applies_rows <- function() {
  current_result_val <- shiny::reactiveVal(build_reference_curve_from_points(
    data,
    "metric_hi",
    metric_config,
    tibble(
      point_order = 1:5,
      metric_value = c(0, 10, 20, 30, 40),
      index_score = c(0.00, 0.30, 0.70, 1.00, 1.00)
    )
  ))
  applied_points <- NULL
  reset_calls <- 0L

  shiny::testServer(
    mod_reference_curve_editor_server,
    args = list(
      current_result = reactive(current_result_val()),
      higher_is_better = reactive(TRUE),
      on_apply = function(points) {
        applied_points <<- points
      },
      on_reset = function() {
        reset_calls <<- reset_calls + 1L
      }
    ),
    {
      session$flushReact()

      stopifnot(identical(editor_table()$metric_value, c(0, 10, 20, 30, 40)))
      stopifnot(identical(selected_row(), integer(0)))

      session$setInputs(points_table_rows_selected = 3L)
      session$flushReact()
      stopifnot(identical(selected_row(), 3L))

      session$setInputs(move_point_up = 1L)
      session$flushReact()
      stopifnot(identical(editor_table()$metric_value, c(0, 20, 10, 30, 40)))
      stopifnot(identical(selected_row(), 2L))
      stopifnot(is.null(applied_points))
      stopifnot(identical(current_result_val()$curve_points$metric_value, c(0, 10, 20, 30, 40)))

      session$setInputs(apply_points = 1L)
      session$flushReact()
      stopifnot(is.null(applied_points))
      stopifnot(grepl(
        "Metric score must be non-decreasing from top to bottom",
        validation_message(),
        fixed = TRUE
      ))

      session$setInputs(move_point_down = 1L)
      session$flushReact()
      stopifnot(identical(editor_table()$metric_value, c(0, 10, 20, 30, 40)))
      stopifnot(identical(selected_row(), 3L))

      session$setInputs(apply_points = 2L)
      session$flushReact()
      stopifnot(!is.null(applied_points))
      stopifnot(identical(applied_points$metric_value, c(0, 10, 20, 30, 40)))
      stopifnot(is.null(validation_message()))

      editor_table(data.frame(
        point_order = 1:5,
        metric_value = c(0, 10, 20, 30, 40),
        index_score = c(0.00, 1.00, 0.00, 1.00, 0.00),
        stringsAsFactors = FALSE
      ))
      validation_message(NULL)
      set_selected_row(integer(0))
      session$flushReact()

      session$setInputs(apply_points = 3L)
      session$flushReact()
      stopifnot(grepl(
        "index score 0.30 at most twice",
        validation_message(),
        fixed = TRUE
      ))

      session$setInputs(points_table_rows_selected = 1L)
      session$flushReact()
      session$setInputs(move_point_up = 2L)
      session$flushReact()
      stopifnot(identical(editor_table()$metric_value, c(0, 10, 20, 30, 40)))
      stopifnot(identical(selected_row(), 1L))

      session$setInputs(points_table_rows_selected = 5L)
      session$flushReact()
      session$setInputs(move_point_down = 2L)
      session$flushReact()
      stopifnot(identical(editor_table()$metric_value, c(0, 10, 20, 30, 40)))
      stopifnot(identical(selected_row(), 5L))

      session$setInputs(points_table_rows_selected = integer(0))
      session$flushReact()
      session$setInputs(move_point_up = 3L)
      session$flushReact()
      stopifnot(identical(editor_table()$metric_value, c(0, 10, 20, 30, 40)))
      stopifnot(identical(selected_row(), integer(0)))

      session$setInputs(reset_points = 1L)
      session$flushReact()
      stopifnot(identical(reset_calls, 1L))
    }
  )
}

check_ui_wiring_static <- function() {
  ref_curve_text <- paste(readLines("app/modules/mod_ref_curve.R", warn = FALSE), collapse = "\n")
  phase4_text <- paste(readLines("app/modules/mod_phase4_finalization.R", warn = FALSE), collapse = "\n")

  stopifnot(grepl("Apply Curve Edits", ref_curve_text, fixed = TRUE))
  stopifnot(grepl("Reset to Auto", ref_curve_text, fixed = TRUE))
  stopifnot(grepl("Move Selected Point Up", ref_curve_text, fixed = TRUE))
  stopifnot(grepl("Move Selected Point Down", ref_curve_text, fixed = TRUE))
  stopifnot(grepl("Row order is preserved when applied.", ref_curve_text, fixed = TRUE))
  stopifnot(grepl("Index scores may move freely.", ref_curve_text, fixed = TRUE))
  stopifnot(grepl("Equal consecutive values create a step.", ref_curve_text, ignore.case = TRUE))
  stopifnot(grepl("Threshold tables can show multiple metric ranges", ref_curve_text, fixed = TRUE))
  stopifnot(grepl("Metric Range\\(s\\)", ref_curve_text, perl = TRUE))
  stopifnot(grepl("build_reference_curve_from_points", ref_curve_text, fixed = TRUE))
  stopifnot(grepl("Manual Curve Editors", phase4_text, fixed = TRUE))
  stopifnot(grepl("mod_reference_curve_editor_ui", phase4_text, fixed = TRUE))
  stopifnot(grepl("stratum_editor_ids <- reactiveVal", phase4_text, fixed = TRUE))
  stopifnot(grepl("allocate_stratum_editor_ids <- function", phase4_text, fixed = TRUE))
  stopifnot(!grepl('gsub\\("\\[\\^A-Za-z0-9\\]\\+", "_", tolower\\(level\\)\\)', phase4_text, perl = TRUE))
  stopifnot(grepl('mod_reference_curve_editor_ui\\(ns\\(editor_id_map\\[\\[lvl\\]\\]\\)', phase4_text, perl = TRUE))
  stopifnot(!grepl('mod_reference_curve_editor_ui\\(editor_id_map\\[\\[lvl\\]\\]', phase4_text, perl = TRUE))
}

check_manual_higher_curve_thresholds()
check_manual_lower_curve_thresholds()
check_curve_normalization_preserves_manual_order()
check_step_curve_thresholds()
check_double_crossing_curve_ranges()
check_curve_validation_rejects_bad_points()
check_unsupported_multi_crossing_status()
check_export_rows_drop_curve_points()
check_editor_move_row_helper()
check_reference_curve_editor_server_moves_and_applies_rows()
check_ui_wiring_static()

cat("reference_curve_manual_edit_checks: OK\n")
