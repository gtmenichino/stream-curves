suppressPackageStartupMessages({
  library(tibble)
})

project_root <- normalizePath(".", winslash = "/")

source("tests/local_workbook_helper.R", local = TRUE)
source("R/00_input_workbook.R", local = TRUE)
source("R/02_clean_data.R", local = TRUE)
source("R/03_derive_variables.R", local = TRUE)
source("R/04_metric_precheck.R", local = TRUE)

workbook_path <- require_streamcurves_test_workbook("workbook_input_checks", project_root = project_root)

check_bundled_workbook <- function() {
  input_bundle <- read_input_workbook(workbook_path)

  stopifnot(length(input_bundle$metric_config) == 22L)
  stopifnot(length(input_bundle$strat_config) == 11L)
  stopifnot(length(input_bundle$predictor_config) == 16L)
  stopifnot(length(input_bundle$factor_recode_config) == 3L)
  stopifnot(identical(input_bundle$site_mask_config$site_label_column, "ID"))
  stopifnot(length(input_bundle$site_mask_config$masked_site_ids) == 0L)

  stopifnot(identical(
    input_bundle$metric_config[["perRiffle"]]$allowed_stratifications,
    c(
      "Ecoregion",
      "Ecoregion_grouped",
      "DACAT",
      "Slope_per_grouped",
      "Ecoregion_x_DACAT",
      "StreamType2",
      "valleytype",
      "Bedmaterial"
    )
  ))
  stopifnot(identical(
    input_bundle$metric_config[["perRiffle"]]$allowed_predictors,
    c("DA_km2", "Slope_per", "LWD_frequency", "L_Bufferwidth", "R_Bufferwidth", "Per_erodingbank", "d50")
  ))
  stopifnot(all(vapply(
    input_bundle$metric_config,
    function(mc) all(c("Ecoregion_grouped", "Slope_per_grouped") %in% mc$allowed_stratifications),
    logical(1)
  )))

  clean_result <- clean_data(
    input_bundle$raw_data,
    input_bundle$metric_config,
    input_bundle$strat_config,
    input_bundle$factor_recode_config
  )
  data <- derive_variables(
    clean_result$data,
    input_bundle$factor_recode_config,
    input_bundle$predictor_config,
    input_bundle$strat_config
  )

  stopifnot(all(c(
    "DA_km2", "Slope_unitless", "streampower", "bufferwidth",
    "StreamType2", "BEHIcombined", "BEHIeroding",
    "Ecoregion_grouped", "Slope_per_grouped",
    streamcurves_site_id_column, streamcurves_site_label_column
  ) %in% names(data)))
  stopifnot(identical(data[[streamcurves_site_id_column]], seq_len(nrow(data))))
  stopifnot(identical(as.character(data[[streamcurves_site_label_column]]), as.character(data$ID)))
  stopifnot(identical(
    as.integer(table(data$Ecoregion_grouped)[c("ECBP/HELP", "IP")]),
    c(24L, 15L)
  ))
  stopifnot(identical(
    as.integer(table(data$Slope_per_grouped)[c("<=1", ">1")]),
    c(24L, 15L)
  ))

  precheck <- run_metric_precheck(data, input_bundle$metric_config)
  stopifnot(nrow(precheck) == 22L)
}

check_categorical_custom_group <- function() {
  data <- tibble(source = c("A", "B", "C", "D", NA))
  strat_config <- list(
    source_grouped = list(
      column_name = "source_grouped",
      source_column = "source",
      source_data_type = "categorical",
      type = "single",
      levels = c("Group 1", "Group 2"),
      is_custom_grouping = TRUE,
      group_definitions = list(
        list(group_label = "Group 1", source_values = c("A", "B"), rule_expression = NA_character_, sort_order = 1),
        list(group_label = "Group 2", source_values = c("C", "D"), rule_expression = NA_character_, sort_order = 2)
      )
    )
  )

  out <- materialize_custom_stratifications(data, strat_config)
  stopifnot(identical(
    as.character(out$source_grouped),
    c("Group 1", "Group 1", "Group 2", "Group 2", NA_character_)
  ))
}

check_continuous_custom_group <- function() {
  data <- tibble(source = c(0.5, 1, 1.5, 5, 6, NA))
  strat_config <- list(
    source_binned = list(
      column_name = "source_binned",
      source_column = "source",
      source_data_type = "continuous",
      type = "single",
      levels = c("<=1", ">1_to_5", ">5"),
      is_custom_grouping = TRUE,
      group_definitions = list(
        list(group_label = "<=1", source_values = character(0), rule_expression = "<= 1", sort_order = 1),
        list(group_label = ">1_to_5", source_values = character(0), rule_expression = "> 1 & <= 5", sort_order = 2),
        list(group_label = ">5", source_values = character(0), rule_expression = "> 5", sort_order = 3)
      )
    )
  )

  out <- materialize_custom_stratifications(data, strat_config)
  stopifnot(identical(
    as.character(out$source_binned),
    c("<=1", "<=1", ">1_to_5", ">1_to_5", ">5", NA_character_)
  ))
}

check_continuous_overlap_error <- function() {
  data <- tibble(source = c(1, 2))
  strat_config <- list(
    overlap_bins = list(
      column_name = "overlap_bins",
      source_column = "source",
      source_data_type = "continuous",
      type = "single",
      levels = c("low", "high"),
      is_custom_grouping = TRUE,
      group_definitions = list(
        list(group_label = "low", source_values = character(0), rule_expression = "<= 1", sort_order = 1),
        list(group_label = "high", source_values = character(0), rule_expression = ">= 1", sort_order = 2)
      )
    )
  )

  err <- tryCatch(
    {
      materialize_custom_stratifications(data, strat_config)
      NULL
    },
    error = function(e) e
  )

  stopifnot(inherits(err, "error"))
  stopifnot(grepl("overlapping", err$message, fixed = TRUE))
}

check_bundled_workbook()
check_categorical_custom_group()
check_continuous_custom_group()
check_continuous_overlap_error()

cat("workbook_input_checks: OK\n")
