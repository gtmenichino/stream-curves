suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
})

project_root <- normalizePath(".", winslash = "/")
connect_cloud_runtime <- FALSE

source("R/00_input_workbook.R", local = TRUE)
source("app/helpers/notifications.R", local = TRUE)
source("app/helpers/phase_tracker.R", local = TRUE)
source("app/modules/mod_data_overview.R", local = TRUE)

normalize_link_table <- function(df, key_col, value_col) {
  if (nrow(df) == 0) {
    return(df)
  }

  df |>
    mutate(sort_order = suppressWarnings(as.numeric(.data$sort_order))) |>
    arrange(.data[[key_col]], .data$sort_order, .data[[value_col]]) |>
    select(all_of(c(key_col, value_col)))
}

check_metrics_editor_round_trip <- function() {
  input_bundle <- read_input_workbook(".local/test_workbook.xlsx")
  editor_df <- build_metrics_editor_df(input_bundle$metadata)

  stopifnot(all(c("allowed_predictors", "allowed_stratifications") %in% names(editor_df)))
  stopifnot(
    editor_df$allowed_stratifications[editor_df$metric_key == "perRiffle"] ==
      "Ecoregion|Ecoregion_grouped|DACAT|Slope_per_grouped|Ecoregion_x_DACAT|StreamType2|valleytype|Bedmaterial"
  )

  rebuilt_tables <- apply_metrics_editor_df(input_bundle$metadata, editor_df)

  stopifnot(identical(
    normalize_link_table(rebuilt_tables$metric_predictors, "metric_key", "predictor_key"),
    normalize_link_table(input_bundle$metadata$metric_predictors, "metric_key", "predictor_key")
  ))
  stopifnot(identical(
    normalize_link_table(rebuilt_tables$metric_stratifications, "metric_key", "strat_key"),
    normalize_link_table(input_bundle$metadata$metric_stratifications, "metric_key", "strat_key")
  ))
}

check_workbook_export_round_trip <- function() {
  input_bundle <- read_input_workbook(".local/test_workbook.xlsx")
  out_path <- tempfile("streamcurves_editor_", fileext = ".xlsx")
  on.exit(unlink(out_path, force = TRUE), add = TRUE)

  write_input_workbook(
    input_bundle$metadata,
    out_path,
    script_path = file.path(project_root, "scripts", "write_workbook_from_json.py")
  )

  round_trip <- read_input_workbook(out_path)

  stopifnot(nrow(round_trip$raw_data) == nrow(input_bundle$raw_data))
  stopifnot(length(round_trip$metric_config) == length(input_bundle$metric_config))
  stopifnot(length(round_trip$strat_config) == length(input_bundle$strat_config))
  stopifnot(length(round_trip$predictor_config) == length(input_bundle$predictor_config))
  stopifnot(length(round_trip$factor_recode_config) == length(input_bundle$factor_recode_config))
  stopifnot(identical(round_trip$site_mask_config$site_label_column, "ID"))
  stopifnot(length(round_trip$site_mask_config$masked_site_ids) == 0L)
}

check_site_mask_editor_round_trip <- function() {
  input_bundle <- read_input_workbook(".local/test_workbook.xlsx")
  updated_tables <- apply_site_mask_selection_to_tables(
    input_bundle$metadata,
    c(2L, 5L, 9L),
    "Name"
  )

  stopifnot(identical(updated_tables$site_mask_settings$site_label_column[[1]], "Name"))
  stopifnot(identical(as.integer(updated_tables$site_masks$masked_sites), c(2L, 5L, 9L)))
  stopifnot(identical(
    as.character(updated_tables$site_masks$site_label),
    as.character(updated_tables$data$Name[c(2L, 5L, 9L)])
  ))
}

check_metrics_editor_round_trip()
check_workbook_export_round_trip()
check_site_mask_editor_round_trip()

cat("workbook_editor_checks: OK\n")
