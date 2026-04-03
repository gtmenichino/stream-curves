suppressPackageStartupMessages({
  library(shiny)
})

project_root <- normalizePath(".", winslash = "/")
connect_cloud_runtime <- FALSE

source("tests/local_workbook_helper.R", local = TRUE)
source("R/00_input_workbook.R", local = TRUE)
source("app/helpers/notifications.R", local = TRUE)
source("app/helpers/phase_tracker.R", local = TRUE)
source("app/modules/mod_data_overview.R", local = TRUE)

workbook_path <- require_streamcurves_test_workbook("metadata_editor_ui_checks", project_root = project_root)

check_metric_dropdown_specs <- function(tables) {
  metrics_df <- editor_df_for_tab(tables, "metrics")
  per_riffle <- metrics_df[metrics_df$metric_key == "perRiffle", , drop = FALSE]

  allowed_pred_spec <- metadata_field_spec("metrics", "allowed_predictors", tables, per_riffle)
  stopifnot(identical(allowed_pred_spec$editor, "multiselect"))
  stopifnot(length(allowed_pred_spec$choices) == nrow(tables$predictors))

  family_spec <- metadata_field_spec("metrics", "metric_family", tables, per_riffle)
  stopifnot(identical(family_spec$editor, "select"))
  stopifnot(identical(unname(family_spec$choices), c("continuous", "proportion", "count", "categorical")))
}

check_stratification_specs <- function(tables) {
  strat_df <- editor_df_for_tab(tables, "stratifications")
  raw_row <- strat_df[strat_df$strat_key == "Ecoregion", , drop = FALSE]
  paired_row <- strat_df[strat_df$strat_key == "Ecoregion_x_DACAT", , drop = FALSE]

  raw_primary_spec <- metadata_field_spec("stratifications", "primary_strat_key", tables, raw_row)
  stopifnot(isFALSE(raw_primary_spec$enabled))

  paired_primary_spec <- metadata_field_spec("stratifications", "primary_strat_key", tables, paired_row)
  stopifnot(isTRUE(paired_primary_spec$enabled))
  stopifnot(identical(paired_primary_spec$editor, "select"))
}

check_recode_and_group_specs <- function(tables) {
  recode_df <- editor_df_for_tab(tables, "factor_recodes")
  recode_row <- recode_df[recode_df$recode_key == "StreamType2", , drop = FALSE]
  recode_spec <- metadata_field_spec("factor_recodes", "source_values", tables, recode_row)
  stopifnot(identical(recode_spec$editor, "multiselect"))
  stopifnot(length(recode_spec$choices) >= 2L)

  group_df <- editor_df_for_tab(tables, "custom_groups")
  group_row <- group_df[group_df$strat_key == "Ecoregion_grouped", , drop = FALSE]
  group_spec <- metadata_field_spec("custom_groups", "source_values", tables, group_row)
  stopifnot(isTRUE(group_spec$enabled))
  stopifnot(identical(group_spec$editor, "multiselect"))
}

check_site_mask_helpers <- function(tables) {
  stopifnot(identical(site_mask_label_column_from_tables(tables), "ID"))

  choices <- site_mask_choice_vector(tables)
  stopifnot(length(choices) == nrow(tables$data))
  stopifnot(any(grepl("^1 - ", names(choices))))

  updated_tables <- apply_site_mask_selection_to_tables(tables, c(1L, 3L), "Name")
  stopifnot(identical(updated_tables$site_mask_settings$site_label_column[[1]], "Name"))
  stopifnot(identical(as.integer(updated_tables$site_masks$masked_sites), c(1L, 3L)))
}

check_wrapped_headers_and_display_values <- function(tables) {
  labels <- metadata_column_labels("metrics", tables)
  stopifnot(grepl("<br>", labels[["allowed_predictors"]], fixed = TRUE))

  display_df <- metadata_display_df_for_tab("metrics", tables)
  per_riffle_display <- display_df$allowed_predictors[display_df$metric_key == "perRiffle"]
  stopifnot(grepl(" | ", per_riffle_display, fixed = TRUE))
}

check_default_visible_columns <- function(tables) {
  metric_defaults <- default_visible_metadata_columns("metrics", tables)
  stopifnot(length(metric_defaults) == 6L)
  stopifnot(!("notes" %in% metric_defaults))
  stopifnot(identical(
    metric_defaults,
    c("metric_key", "display_name", "column_name", "units", "metric_family", "higher_is_better")
  ))

  recode_defaults <- default_visible_metadata_columns("factor_recodes", tables)
  stopifnot(!("notes" %in% recode_defaults))
  stopifnot(identical(
    recode_defaults,
    c("recode_key", "source_column", "target_column", "target_level", "source_values")
  ))

  site_mask_defaults <- default_visible_metadata_columns("site_masks", tables)
  stopifnot(identical(site_mask_defaults, c("masked_sites", "site_label")))

  normalized <- normalize_visible_metadata_columns("metrics", c("notes", "metric_key", "missing_col"), tables)
  stopifnot(identical(normalized, c("notes", "metric_key")))
}

check_picker_commit_and_layout_wiring <- function() {
  module_text <- paste(readLines("app/modules/mod_data_overview.R", warn = FALSE), collapse = "\n")
  css_text <- paste(readLines("app/www/custom.css", warn = FALSE), collapse = "\n")
  js_text <- paste(readLines("app/www/custom.js", warn = FALSE), collapse = "\n")

  stopifnot(grepl("register_column_picker_commit <- function", module_text, fixed = TRUE))
  stopifnot(grepl("stateInput = TRUE", module_text, fixed = TRUE))
  stopifnot(grepl("open_input_name <- paste0\\(input_name, \"_open\"\\)", module_text))
  stopifnot(!grepl("metadata_visible_columns_commit", module_text, fixed = TRUE))
  stopifnot(!grepl("data-commit-input-id", module_text, fixed = TRUE))
  stopifnot(!grepl("Open Editor Popup", module_text, fixed = TRUE))
  stopifnot(grepl("Upload Session", module_text, fixed = TRUE))
  stopifnot(grepl("Save Session", module_text, fixed = TRUE))
  stopifnot(grepl("session_tooltip_content <- function", module_text, fixed = TRUE))
  stopifnot(length(gregexpr("col-12 col-lg-4", module_text, fixed = TRUE)[[1]]) >= 3L)
  stopifnot(!grepl("btn btn-outline-danger w-100", module_text, fixed = TRUE))
  stopifnot(grepl("data-setup-card-header", module_text, fixed = TRUE))
  stopifnot(grepl('card_header\\(\\s*class = "data-setup-card-header"', module_text))
  stopifnot(!grepl("data-setup-card-header-actions", module_text, fixed = TRUE))
  stopifnot(grepl("value = \"workbook_metadata\"", module_text, fixed = TRUE))
  stopifnot(grepl("open = FALSE", module_text, fixed = TRUE))
  stopifnot(!grepl("open = \"workbook_metadata\"", module_text, fixed = TRUE))
  stopifnot(grepl("metadata_accordion_content", module_text, fixed = TRUE))
  stopifnot(grepl("refreshMetadataAccordion", module_text, fixed = TRUE))
  stopifnot(grepl("Site Masks", module_text, fixed = TRUE))
  stopifnot(grepl("manage_site_masks", module_text, fixed = TRUE))
  stopifnot(grepl("delete_site_mask_rows", module_text, fixed = TRUE))
  stopifnot(grepl("site_mask_settings", module_text, fixed = TRUE))
  stopifnot(grepl("download_session", module_text, fixed = TRUE))
  stopifnot(grepl("save-session-row", module_text, fixed = TRUE))
  stopifnot(grepl("save-session-download-btn", module_text, fixed = TRUE))
  stopifnot(grepl("save-session-title", module_text, fixed = TRUE))
  stopifnot(grepl('label = "Download Session"', module_text, fixed = TRUE))
  stopifnot(!grepl('label = icon\\("download"\\)', module_text))
  stopifnot(grepl('`aria-label` = "What a saved session includes"', module_text, fixed = TRUE))
  stopifnot(grepl('"i"', module_text, fixed = TRUE))
  stopifnot(grepl("Download Session disabled", module_text, fixed = TRUE))
  stopifnot(grepl("Load a workbook or session to enable session download.", module_text, fixed = TRUE))
  stopifnot(grepl("Current derived analysis data, workbook tables/metadata, site masks, current app selections/settings, saved results/caches, reference-curve choices/results, and decision history.", module_text, fixed = TRUE))
  stopifnot(grepl("Stores the workbook tables and upload filename, but not the original .xlsx file itself as an attached binary.", module_text, fixed = TRUE))
  stopifnot(grepl("Uploading the .rds restores the saved workspace state.", module_text, fixed = TRUE))
  stopifnot(grepl("load_session_file", module_text, fixed = TRUE))
  stopifnot(!grepl("load_session_select", module_text, fixed = TRUE))
  stopifnot(!grepl("load_session_btn", module_text, fixed = TRUE))
  stopifnot(!grepl('\"Download Session\",\\s*class = \"btn btn-outline-primary btn-sm w-100\"', module_text))
  stopifnot(grepl("data_setup_controls_card\\(ns,", module_text))
  stopifnot(grepl("clear_session_upload_input <- function", module_text, fixed = TRUE))
  stopifnot(grepl('updateTextInput\\(\\s*session,\\s*"session_name"', module_text))

  stopifnot(!grepl("metadata-column-picker", js_text, fixed = TRUE))
  stopifnot(!grepl("metadata-editor-modal", js_text, fixed = TRUE))
  stopifnot(grepl("clearFileInput", js_text, fixed = TRUE))
  stopifnot(grepl("refreshMetadataAccordion", js_text, fixed = TRUE))
  stopifnot(grepl("shown.bs.tab", js_text, fixed = TRUE))
  stopifnot(grepl("shown.bs.collapse", js_text, fixed = TRUE))

  stopifnot(grepl("data-setup-card-header", css_text, fixed = TRUE))
  stopifnot(grepl("width: 100%;", css_text, fixed = TRUE))
  stopifnot(grepl("\\.data-setup-card-header > \\.shiny-html-output", css_text))
  stopifnot(!grepl("data-setup-card-header-actions", css_text, fixed = TRUE))
  stopifnot(grepl("save-session-row", css_text, fixed = TRUE))
  stopifnot(grepl("save-session-download", css_text, fixed = TRUE))
  stopifnot(grepl("save-session-help", css_text, fixed = TRUE))
  stopifnot(grepl("metadata-accordion-content", css_text, fixed = TRUE))
  stopifnot(grepl("padding: 0.75rem 0.8rem;", css_text, fixed = TRUE))
  stopifnot(grepl("overflow-y: auto;", css_text, fixed = TRUE))
  stopifnot(grepl("data-setup-control-panel", css_text, fixed = TRUE))
  stopifnot(!grepl("metadata-editor-modal", css_text, fixed = TRUE))
  stopifnot(!grepl("min-height: 38rem;", css_text, fixed = TRUE))
}

input_bundle <- read_input_workbook(workbook_path)
tables <- input_bundle$metadata

check_metric_dropdown_specs(tables)
check_stratification_specs(tables)
check_recode_and_group_specs(tables)
check_site_mask_helpers(tables)
check_wrapped_headers_and_display_values(tables)
check_default_visible_columns(tables)
check_picker_commit_and_layout_wiring()

cat("metadata_editor_ui_checks: OK\n")
