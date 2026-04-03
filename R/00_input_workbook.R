## -- 00: Workbook Input & Metadata Parsing ------------------------------------
## Reads the StreamCurves workbook format and rebuilds runtime configs.

library(readxl)
library(dplyr)
library(tibble)
library(purrr)
library(stringr)
library(cli)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

streamcurves_site_id_column <- "..streamcurves_site_id"
streamcurves_site_label_column <- "..streamcurves_site_label"

required_workbook_sheets <- function() {
  c(
    "data",
    "metrics",
    "metric_predictors",
    "metric_stratifications",
    "stratifications",
    "strat_groups",
    "predictors",
    "factor_recodes"
  )
}

optional_workbook_sheets <- function() {
  c("site_masks", "site_mask_settings")
}

workbook_sheet_specs <- function() {
  list(
    data = list(required = character(0)),
    metrics = list(required = c(
      "metric_key", "display_name", "column_name", "metric_family",
      "higher_is_better", "monotonic_linear", "preferred_transform",
      "min_sample_size", "best_subsets_allowed", "count_model",
      "stratification_mode", "include_in_summary"
    )),
    metric_predictors = list(required = c("metric_key", "predictor_key")),
    metric_stratifications = list(required = c("metric_key", "strat_key")),
    stratifications = list(required = c(
      "strat_key", "display_name", "strat_type", "min_group_size"
    )),
    strat_groups = list(required = c("strat_key", "group_label")),
    predictors = list(required = c(
      "predictor_key", "display_name", "column_name", "type", "derived"
    )),
    factor_recodes = list(required = c(
      "recode_key", "source_column", "target_column", "target_level", "source_values"
    )),
    site_masks = list(required = c("masked_sites", "site_label")),
    site_mask_settings = list(required = c("site_label_column"))
  )
}

workbook_sheet_columns <- function() {
  list(
    data = NULL,
    metrics = c(
      "metric_key", "display_name", "column_name", "units", "metric_family",
      "higher_is_better", "monotonic_linear", "preferred_transform",
      "min_sample_size", "best_subsets_allowed", "count_model",
      "stratification_mode", "include_in_summary", "missing_data_rule", "notes"
    ),
    metric_predictors = c("metric_key", "predictor_key", "sort_order"),
    metric_stratifications = c("metric_key", "strat_key", "sort_order"),
    stratifications = c(
      "strat_key", "display_name", "strat_type", "source_column",
      "source_data_type", "primary_strat_key", "secondary_strat_key",
      "derived_column_name", "levels", "pairwise_comparisons",
      "min_group_size", "notes"
    ),
    strat_groups = c(
      "strat_key", "group_label", "sort_order", "source_values", "rule_expression"
    ),
    predictors = c(
      "predictor_key", "display_name", "column_name", "type", "derived",
      "derivation_method", "source_columns", "constant", "expected_min",
      "expected_max", "missing_data_rule", "notes"
    ),
    factor_recodes = c(
      "recode_key", "source_column", "target_column", "target_level",
      "source_values", "notes"
    ),
    site_masks = c("masked_sites", "site_label"),
    site_mask_settings = c("site_label_column")
  )
}

ensure_workbook_sheet_columns <- function(df, sheet_name) {
  if (is.null(df)) {
    df <- data.frame()
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <- trim_character_columns(df)

  desired <- workbook_sheet_columns()[[sheet_name]]
  if (is.null(desired)) {
    return(df)
  }

  missing_cols <- setdiff(desired, names(df))
  for (col_name in missing_cols) {
    df[[col_name]] <- if (nrow(df) == 0) {
      logical(0)
    } else {
      rep(NA, nrow(df))
    }
  }

  extra_cols <- setdiff(names(df), desired)
  df[, c(desired, extra_cols), drop = FALSE]
}

normalize_workbook_tables <- function(tables) {
  normalized <- list()
  for (sheet_name in names(workbook_sheet_specs())) {
    normalized[[sheet_name]] <- ensure_workbook_sheet_columns(
      tables[[sheet_name]] %||% data.frame(),
      sheet_name
    )
  }
  normalized
}

trim_character_columns <- function(df) {
  df |>
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim))
}

compact_chr <- function(x) {
  vals <- as.character(x %||% character(0))
  vals <- stringr::str_trim(vals)
  vals[!is.na(vals) & nzchar(vals)]
}

scalar_text <- function(value, default = NA_character_) {
  vals <- compact_chr(value)
  if (length(vals) == 0) {
    return(default)
  }
  vals[[1]]
}

scalar_number <- function(value, default = NA_real_) {
  out <- coerce_optional_numeric(value)
  if (is.na(out)) {
    return(default)
  }
  out
}

parse_pipe_values <- function(value) {
  if (length(value) == 0 || is.null(value) || all(is.na(value))) {
    return(character(0))
  }

  parts <- strsplit(as.character(value[[1]]), "\\|", fixed = FALSE)[[1]]
  compact_chr(parts)
}

parse_pairwise_values <- function(value) {
  pairs <- parse_pipe_values(value)
  if (length(pairs) == 0) {
    return(list())
  }

  purrr::map(pairs, function(pair_text) {
    pair <- compact_chr(strsplit(pair_text, "~", fixed = TRUE)[[1]])
    if (length(pair) != 2) {
      stop(
        paste0("Invalid pairwise comparison entry: '", pair_text, "'. Use group1~group2."),
        call. = FALSE
      )
    }
    pair
  })
}

auto_pairwise_values <- function(levels) {
  levels <- compact_chr(levels)
  if (length(levels) < 2) {
    return(list())
  }

  combn_mat <- utils::combn(levels, 2)
  lapply(seq_len(ncol(combn_mat)), function(idx) combn_mat[, idx])
}

coerce_flag <- function(value, default = FALSE) {
  if (length(value) == 0 || is.null(value) || all(is.na(value))) {
    return(default)
  }

  raw <- as.character(value[[1]])
  raw <- stringr::str_to_lower(stringr::str_trim(raw))
  if (!nzchar(raw)) {
    return(default)
  }

  if (raw %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (raw %in% c("false", "f", "0", "no", "n")) return(FALSE)

  stop(paste0("Could not parse logical value '", value[[1]], "'."), call. = FALSE)
}

coerce_optional_numeric <- function(value) {
  if (length(value) == 0 || is.null(value) || all(is.na(value))) {
    return(NA_real_)
  }

  out <- suppressWarnings(as.numeric(value[[1]]))
  if (is.na(out) && !is.na(value[[1]]) && nzchar(as.character(value[[1]]))) {
    stop(paste0("Could not parse numeric value '", value[[1]], "'."), call. = FALSE)
  }
  out
}

read_required_sheet <- function(input_path, sheet_name, required_cols) {
  df <- readxl::read_excel(input_path, sheet = sheet_name)
  df <- trim_character_columns(df)

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Sheet '", sheet_name, "' is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  df
}

validate_unique_keys <- function(df, column_name, sheet_name) {
  blanks <- df |>
    dplyr::filter(is.na(.data[[column_name]]) | !nzchar(trimws(as.character(.data[[column_name]]))))

  if (nrow(blanks) > 0) {
    stop(
      paste0("Sheet '", sheet_name, "' contains blank values in column '", column_name, "'."),
      call. = FALSE
    )
  }

  dupes <- df |>
    dplyr::count(.data[[column_name]], name = "n") |>
    dplyr::filter(!is.na(.data[[column_name]]), n > 1)

  if (nrow(dupes) > 0) {
    stop(
      paste0(
        "Sheet '", sheet_name, "' has duplicate ", column_name, " values: ",
        paste(dupes[[column_name]], collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

resolve_levels_from_data <- function(data, column_name) {
  if (is.null(column_name) || !nzchar(column_name) || !column_name %in% names(data)) {
    return(character(0))
  }

  vals <- data[[column_name]]
  if (is.factor(vals)) {
    return(levels(vals))
  }

  sort(unique(as.character(vals[!is.na(vals)])))
}

default_site_label_source_column <- function(raw_data) {
  column_names <- names(raw_data %||% data.frame())
  if (length(column_names) == 0) {
    stop("Sheet 'data' must include at least one column to support site labels.", call. = FALSE)
  }
  column_names[[1]]
}

resolve_site_label_values <- function(raw_data, site_ids, label_column) {
  if (length(site_ids) == 0) {
    return(character(0))
  }

  label_values <- as.character(raw_data[[label_column]][site_ids])
  fallback_labels <- paste("Site", site_ids)
  label_values[is.na(label_values) | !nzchar(trimws(label_values))] <- fallback_labels[is.na(label_values) | !nzchar(trimws(label_values))]
  label_values
}

build_site_mask_config_from_workbook <- function(site_masks_tbl,
                                                 site_mask_settings_tbl,
                                                 raw_data) {
  default_label_column <- default_site_label_source_column(raw_data)
  label_column_values <- compact_chr(site_mask_settings_tbl$site_label_column)

  if (length(unique(label_column_values)) > 1) {
    stop(
      "Sheet 'site_mask_settings' must contain at most one distinct site_label_column value.",
      call. = FALSE
    )
  }

  label_column <- if (length(label_column_values) > 0) {
    label_column_values[[1]]
  } else {
    default_label_column
  }
  if (!label_column %in% names(raw_data)) {
    stop(
      paste0(
        "Sheet 'site_mask_settings' references missing site_label_column '",
        label_column,
        "'."
      ),
      call. = FALSE
    )
  }

  site_id_values <- compact_chr(site_masks_tbl$masked_sites)
  site_ids <- integer(0)
  if (length(site_id_values) > 0) {
    numeric_ids <- suppressWarnings(as.integer(site_id_values))
    bad_idx <- which(is.na(numeric_ids) | numeric_ids <= 0L | numeric_ids > nrow(raw_data))
    if (length(bad_idx) > 0) {
      stop(
        paste0(
          "Sheet 'site_masks' contains invalid masked_sites values: ",
          paste(unique(site_id_values[bad_idx]), collapse = ", ")
        ),
        call. = FALSE
      )
    }

    if (anyDuplicated(numeric_ids)) {
      dup_ids <- unique(numeric_ids[duplicated(numeric_ids)])
      stop(
        paste0(
          "Sheet 'site_masks' contains duplicate masked_sites values: ",
          paste(dup_ids, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    site_ids <- sort(unique(numeric_ids))
  }

  list(
    site_label_column = label_column,
    masked_site_ids = site_ids,
    site_labels = resolve_site_label_values(raw_data, site_ids, label_column)
  )
}

site_mask_tables_from_config <- function(raw_data, site_mask_config) {
  masked_ids <- as.integer(site_mask_config$masked_site_ids %||% integer(0))
  label_column <- site_mask_config$site_label_column %||% default_site_label_source_column(raw_data)
  site_labels <- resolve_site_label_values(raw_data, masked_ids, label_column)

  site_masks_tbl <- tibble::tibble(
    masked_sites = if (length(masked_ids) == 0) integer(0) else masked_ids,
    site_label = if (length(site_labels) == 0) character(0) else site_labels
  )

  site_mask_settings_tbl <- tibble::tibble(
    site_label_column = label_column
  )

  list(
    site_masks = ensure_workbook_sheet_columns(site_masks_tbl, "site_masks"),
    site_mask_settings = ensure_workbook_sheet_columns(site_mask_settings_tbl, "site_mask_settings")
  )
}

annotate_site_identity_columns <- function(raw_data, site_mask_config) {
  data <- as.data.frame(raw_data, stringsAsFactors = FALSE)
  label_column <- site_mask_config$site_label_column %||% default_site_label_source_column(data)

  data[[streamcurves_site_id_column]] <- seq_len(nrow(data))
  data[[streamcurves_site_label_column]] <- resolve_site_label_values(
    data,
    data[[streamcurves_site_id_column]],
    label_column
  )
  data
}

apply_global_site_masks <- function(raw_data, site_mask_config) {
  masked_ids <- as.integer(site_mask_config$masked_site_ids %||% integer(0))
  if (length(masked_ids) == 0) {
    return(raw_data)
  }

  raw_data |>
    dplyr::filter(!(.data[[streamcurves_site_id_column]] %in% masked_ids))
}

validate_foreign_keys <- function(metrics_tbl,
                                  metric_predictors_tbl,
                                  metric_stratifications_tbl,
                                  predictors_tbl,
                                  stratifications_tbl) {
  metric_keys <- compact_chr(metrics_tbl$metric_key)
  predictor_keys <- compact_chr(predictors_tbl$predictor_key)
  strat_keys <- compact_chr(stratifications_tbl$strat_key)

  bad_metric_predictors <- metric_predictors_tbl |>
    dplyr::filter(
      !(.data$metric_key %in% metric_keys) |
        !(.data$predictor_key %in% predictor_keys)
    )
  if (nrow(bad_metric_predictors) > 0) {
    stop("Sheet 'metric_predictors' contains unknown metric_key or predictor_key values.", call. = FALSE)
  }

  bad_metric_strats <- metric_stratifications_tbl |>
    dplyr::filter(
      !(.data$metric_key %in% metric_keys) |
        !(.data$strat_key %in% strat_keys)
    )
  if (nrow(bad_metric_strats) > 0) {
    stop("Sheet 'metric_stratifications' contains unknown metric_key or strat_key values.", call. = FALSE)
  }
}

build_predictor_config_from_workbook <- function(predictors_tbl) {
  predictor_rows <- split(
    predictors_tbl,
    factor(predictors_tbl$predictor_key, levels = unique(predictors_tbl$predictor_key))
  )

  purrr::imap(predictor_rows, function(row_df, predictor_key) {
    row <- row_df[1, , drop = FALSE]
    col_name <- scalar_text(row$column_name, NA_character_)
    if (is.na(col_name)) {
      stop(paste0("Predictor '", predictor_key, "' is missing column_name."), call. = FALSE)
    }

    expected_min <- if ("expected_min" %in% names(row)) coerce_optional_numeric(row$expected_min) else NA_real_
    expected_max <- if ("expected_max" %in% names(row)) coerce_optional_numeric(row$expected_max) else NA_real_

    list(
      display_name = scalar_text(row$display_name, predictor_key),
      column_name = col_name,
      type = scalar_text(row$type, "continuous"),
      derived = coerce_flag(row$derived, default = FALSE),
      derivation_method = if ("derivation_method" %in% names(row)) {
        scalar_text(row$derivation_method, "none")
      } else {
        "none"
      },
      source_columns = if ("source_columns" %in% names(row)) parse_pipe_values(row$source_columns) else character(0),
      constant = if ("constant" %in% names(row)) scalar_number(row$constant, NA_real_) else NA_real_,
      expected_range = c(expected_min, expected_max),
      missing_data_rule = if ("missing_data_rule" %in% names(row)) {
        scalar_text(row$missing_data_rule, "error")
      } else {
        "error"
      },
      notes = if ("notes" %in% names(row)) scalar_text(row$notes, "") else ""
    )
  })
}

build_factor_recode_config_from_workbook <- function(factor_recodes_tbl) {
  if (nrow(factor_recodes_tbl) == 0) {
    return(list())
  }

  recode_rows <- split(
    factor_recodes_tbl,
    factor(factor_recodes_tbl$recode_key, levels = unique(factor_recodes_tbl$recode_key))
  )

  purrr::imap(recode_rows, function(row_df, recode_key) {
    target_rows <- split(
      row_df,
      factor(row_df$target_level, levels = unique(row_df$target_level))
    )
    collapse_map <- purrr::imap(target_rows, function(target_df, target_level) {
      values <- unique(unlist(lapply(target_df$source_values, parse_pipe_values), use.names = FALSE))
      compact_chr(values)
    })

    first_row <- row_df[1, , drop = FALSE]
    source_column <- scalar_text(first_row$source_column, NA_character_)
    target_column <- scalar_text(first_row$target_column, NA_character_)
    if (is.na(source_column)) {
      stop(paste0("Factor recode '", recode_key, "' is missing source_column."), call. = FALSE)
    }
    if (is.na(target_column)) {
      stop(paste0("Factor recode '", recode_key, "' is missing target_column."), call. = FALSE)
    }
    list(
      source_column = source_column,
      target_column = target_column,
      collapse_map = collapse_map,
      notes = if ("notes" %in% names(first_row)) scalar_text(first_row$notes, "") else ""
    )
  })
}

build_strat_config_from_workbook <- function(stratifications_tbl,
                                             strat_groups_tbl,
                                             raw_data,
                                             factor_recode_config = list()) {
  strat_rows <- split(
    stratifications_tbl,
    factor(stratifications_tbl$strat_key, levels = unique(stratifications_tbl$strat_key))
  )

  purrr::imap(strat_rows, function(row_df, strat_key) {
    current_strat_key <- strat_key
    row <- row_df[1, , drop = FALSE]
    strat_type <- compact_chr(row$strat_type)[1] %||% stop(
      paste0("Stratification '", strat_key, "' is missing strat_type."),
      call. = FALSE
    )

    min_group_size <- if ("min_group_size" %in% names(row)) {
      as.integer(scalar_number(row$min_group_size, 5))
    } else {
      5L
    }

    notes <- if ("notes" %in% names(row)) scalar_text(row$notes, "") else ""
    levels <- if ("levels" %in% names(row)) parse_pipe_values(row$levels) else character(0)
    pairwise <- if ("pairwise_comparisons" %in% names(row)) {
      parse_pairwise_values(row$pairwise_comparisons)
    } else {
      list()
    }
    source_column <- if ("source_column" %in% names(row)) compact_chr(row$source_column)[1] %||% NA_character_ else NA_character_
    source_data_type <- if ("source_data_type" %in% names(row)) compact_chr(row$source_data_type)[1] %||% NA_character_ else NA_character_
    derived_column_name <- if ("derived_column_name" %in% names(row)) compact_chr(row$derived_column_name)[1] %||% NA_character_ else NA_character_
    recode_target_columns <- unique(purrr::map_chr(
      names(factor_recode_config %||% list()),
      function(recode_key) factor_recode_config[[recode_key]]$target_column %||% NA_character_
    ))

    if (identical(strat_type, "paired")) {
      primary <- if ("primary_strat_key" %in% names(row)) compact_chr(row$primary_strat_key)[1] %||% NA_character_ else NA_character_
      secondary <- if ("secondary_strat_key" %in% names(row)) compact_chr(row$secondary_strat_key)[1] %||% NA_character_ else NA_character_
      if (is.na(primary) || is.na(secondary)) {
        stop(
          paste0("Paired stratification '", strat_key, "' must provide primary_strat_key and secondary_strat_key."),
          call. = FALSE
        )
      }

      return(list(
        display_name = scalar_text(row$display_name, strat_key),
        column_name = NULL,
        type = "paired",
        primary = primary,
        secondary = secondary,
        min_group_size = min_group_size,
        levels = character(0),
        pairwise_comparisons = list(),
        notes = notes
      ))
    }

    if (identical(strat_type, "raw_single")) {
      if (is.na(source_column) || !nzchar(source_column)) {
        stop(
          paste0("Raw stratification '", strat_key, "' must provide source_column."),
          call. = FALSE
        )
      }

      if (!source_column %in% names(raw_data) && !source_column %in% recode_target_columns) {
        stop(
          paste0("Raw stratification '", strat_key, "' references missing data column '", source_column, "'."),
          call. = FALSE
        )
      }

      if (length(levels) == 0) {
        if (source_column %in% names(raw_data)) {
          levels <- resolve_levels_from_data(raw_data, source_column)
        } else {
          matching_recode <- purrr::detect(
            factor_recode_config %||% list(),
            function(recode) identical(recode$target_column %||% NA_character_, source_column)
          )
          levels <- names(matching_recode$collapse_map %||% list())
        }
      }

      if (length(pairwise) == 0) {
        pairwise <- auto_pairwise_values(levels)
      }

      return(list(
        display_name = scalar_text(row$display_name, strat_key),
        column_name = source_column,
        type = "single",
        source_column = source_column,
        source_data_type = if (!is.na(source_data_type)) source_data_type else "categorical",
        min_group_size = min_group_size,
        levels = levels,
        pairwise_comparisons = pairwise,
        notes = notes
      ))
    }

    if (!identical(strat_type, "custom_group")) {
      stop(
        paste0("Unsupported strat_type '", strat_type, "' for stratification '", strat_key, "'."),
        call. = FALSE
      )
    }

    if (is.na(source_column) || !nzchar(source_column)) {
      stop(
        paste0("Custom grouping '", strat_key, "' must provide source_column."),
        call. = FALSE
      )
    }

    if (is.na(derived_column_name) || !nzchar(derived_column_name)) {
      derived_column_name <- strat_key
    }

    if (!source_column %in% names(raw_data)) {
      stop(
        paste0("Custom grouping '", strat_key, "' references missing data column '", source_column, "'."),
        call. = FALSE
      )
    }

    if (!identical(source_data_type, "categorical") && !identical(source_data_type, "continuous")) {
      stop(
        paste0(
          "Custom grouping '", strat_key,
          "' must set source_data_type to 'categorical' or 'continuous'."
        ),
        call. = FALSE
      )
    }

    if (derived_column_name %in% names(raw_data)) {
      stop(
        paste0(
          "Custom grouping '", strat_key,
          "' would overwrite existing data column '", derived_column_name, "'."
        ),
        call. = FALSE
      )
    }

    group_rows <- strat_groups_tbl |>
      dplyr::filter(.data$strat_key == current_strat_key)

    if (nrow(group_rows) == 0) {
      stop(
        paste0("Custom grouping '", current_strat_key, "' has no rows in sheet 'strat_groups'."),
        call. = FALSE
      )
    }

    if ("sort_order" %in% names(group_rows)) {
      group_rows <- group_rows |>
        dplyr::mutate(sort_order = suppressWarnings(as.numeric(.data$sort_order))) |>
        dplyr::arrange(.data$sort_order, .data$group_label)
    } else {
      group_rows <- group_rows |>
        dplyr::arrange(.data$group_label)
    }

    group_definitions <- purrr::pmap(group_rows, function(...) {
      entry <- list(...)
      list(
        group_label = entry$group_label %||% NA_character_,
        source_values = parse_pipe_values(entry$source_values %||% NA_character_),
        rule_expression = scalar_text(entry$rule_expression %||% NA_character_, NA_character_),
        sort_order = suppressWarnings(as.numeric(entry$sort_order %||% NA_real_))
      )
    })

    if (anyDuplicated(vapply(group_definitions, `[[`, character(1), "group_label"))) {
      stop(
        paste0("Custom grouping '", current_strat_key, "' has duplicate group_label values."),
        call. = FALSE
      )
    }

    if (length(levels) == 0) {
      levels <- vapply(group_definitions, `[[`, character(1), "group_label")
    }

    if (length(pairwise) == 0) {
      pairwise <- auto_pairwise_values(levels)
    }

    list(
      display_name = scalar_text(row$display_name, strat_key),
      column_name = derived_column_name,
      type = "single",
      source_column = source_column,
      source_data_type = if (!is.na(source_data_type)) source_data_type else "categorical",
      min_group_size = min_group_size,
      levels = levels,
      pairwise_comparisons = pairwise,
      notes = notes,
      is_custom_grouping = TRUE,
      group_definitions = group_definitions
    )
  })
}

build_metric_config_from_workbook <- function(metrics_tbl,
                                              metric_predictors_tbl,
                                              metric_stratifications_tbl) {
  metric_rows <- split(
    metrics_tbl,
    factor(metrics_tbl$metric_key, levels = unique(metrics_tbl$metric_key))
  )

  purrr::imap(metric_rows, function(row_df, metric_key) {
    current_metric_key <- metric_key
    row <- row_df[1, , drop = FALSE]

    predictor_rows <- metric_predictors_tbl |>
      dplyr::filter(.data$metric_key == current_metric_key)
    if ("sort_order" %in% names(predictor_rows)) {
      predictor_rows <- predictor_rows |>
        dplyr::mutate(sort_order = suppressWarnings(as.numeric(.data$sort_order))) |>
        dplyr::arrange(.data$sort_order, .data$predictor_key)
    }

    strat_rows <- metric_stratifications_tbl |>
      dplyr::filter(.data$metric_key == current_metric_key)
    if ("sort_order" %in% names(strat_rows)) {
      strat_rows <- strat_rows |>
        dplyr::mutate(sort_order = suppressWarnings(as.numeric(.data$sort_order))) |>
        dplyr::arrange(.data$sort_order, .data$strat_key)
    }

    column_name <- scalar_text(row$column_name, NA_character_)
    if (is.na(column_name)) {
      stop(paste0("Metric '", metric_key, "' is missing column_name."), call. = FALSE)
    }

    list(
      display_name = scalar_text(row$display_name, metric_key),
      column_name = column_name,
      units = if ("units" %in% names(row)) scalar_text(row$units, "") else "",
      metric_family = scalar_text(row$metric_family, "continuous"),
      higher_is_better = coerce_flag(row$higher_is_better, default = TRUE),
      monotonic_linear = coerce_flag(row$monotonic_linear, default = TRUE),
      allowed_predictors = compact_chr(predictor_rows$predictor_key),
      allowed_stratifications = compact_chr(strat_rows$strat_key),
      preferred_transform = scalar_text(row$preferred_transform, "none"),
      min_sample_size = as.integer(scalar_number(row$min_sample_size, 10)),
      best_subsets_allowed = coerce_flag(row$best_subsets_allowed, default = TRUE),
      count_model = coerce_flag(row$count_model, default = FALSE),
      stratification_mode = scalar_text(row$stratification_mode, "subset"),
      include_in_summary = coerce_flag(row$include_in_summary, default = TRUE),
      missing_data_rule = if ("missing_data_rule" %in% names(row)) scalar_text(row$missing_data_rule, NA_character_) else NA_character_,
      notes = if ("notes" %in% names(row)) scalar_text(row$notes, "") else ""
    )
  })
}

build_input_bundle_from_tables <- function(tables) {
  tables <- normalize_workbook_tables(tables)
  validate_unique_keys(tables$metrics, "metric_key", "metrics")
  validate_unique_keys(tables$stratifications, "strat_key", "stratifications")
  validate_unique_keys(tables$predictors, "predictor_key", "predictors")
  if (nrow(tables$factor_recodes) > 0) {
    invalid_recode_rows <- tables$factor_recodes |>
      dplyr::filter(is.na(.data$recode_key) | is.na(.data$target_level))
    if (nrow(invalid_recode_rows) > 0) {
      stop("Sheet 'factor_recodes' contains blank recode_key or target_level rows.", call. = FALSE)
    }
  }

  validate_foreign_keys(
    tables$metrics,
    tables$metric_predictors,
    tables$metric_stratifications,
    tables$predictors,
    tables$stratifications
  )

  raw_data_unmasked <- tables$data
  site_mask_config <- build_site_mask_config_from_workbook(
    tables$site_masks,
    tables$site_mask_settings,
    raw_data_unmasked
  )
  site_mask_tables <- site_mask_tables_from_config(raw_data_unmasked, site_mask_config)
  tables$site_masks <- site_mask_tables$site_masks
  tables$site_mask_settings <- site_mask_tables$site_mask_settings

  predictor_config <- build_predictor_config_from_workbook(tables$predictors)
  factor_recode_config <- build_factor_recode_config_from_workbook(tables$factor_recodes)
  strat_config <- build_strat_config_from_workbook(
    tables$stratifications,
    tables$strat_groups,
    raw_data_unmasked,
    factor_recode_config
  )
  bad_paired <- purrr::keep(names(strat_config), function(strat_key) {
    sc <- strat_config[[strat_key]]
    identical(sc$type, "paired") &&
      (!(sc$primary %in% names(strat_config)) || !(sc$secondary %in% names(strat_config)))
  })
  if (length(bad_paired) > 0) {
    stop(
      paste0(
        "Paired stratifications reference unknown base stratification keys: ",
        paste(bad_paired, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  metric_config <- build_metric_config_from_workbook(
    tables$metrics,
    tables$metric_predictors,
    tables$metric_stratifications
  )

  raw_data <- raw_data_unmasked |>
    annotate_site_identity_columns(site_mask_config) |>
    apply_global_site_masks(site_mask_config)

  cli::cli_alert_success(
    "Workbook loaded: {nrow(raw_data)} analysis rows x {ncol(raw_data)} columns | {length(metric_config)} metrics | {length(strat_config)} stratifications | {length(predictor_config)} predictors | {length(factor_recode_config)} recodes | {length(site_mask_config$masked_site_ids %||% integer(0))} masked sites"
  )

  list(
    raw_data = raw_data,
    metric_config = metric_config,
    strat_config = strat_config,
    predictor_config = predictor_config,
    factor_recode_config = factor_recode_config,
    site_mask_config = site_mask_config,
    metadata = tables
  )
}

table_to_workbook_payload <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  rows <- if (nrow(df) == 0) {
    list()
  } else {
    lapply(seq_len(nrow(df)), function(idx) {
      row <- df[idx, , drop = FALSE]
      values <- as.list(row)
      for (col_name in names(values)) {
        cell_value <- values[[col_name]]
        values[[col_name]] <- if (length(cell_value) == 0 || is.na(cell_value[[1]])) {
          NULL
        } else {
          cell_value[[1]]
        }
      }
      values
    })
  }

  list(
    columns = names(df),
    rows = rows
  )
}

write_input_workbook <- function(tables,
                                 output_path,
                                 script_path = file.path(getwd(), "scripts", "write_workbook_from_json.py"),
                                 python = Sys.which("python")) {
  tables <- normalize_workbook_tables(tables)

  if (!nzchar(python)) {
    stop("Python was not found on PATH. Workbook export requires Python.", call. = FALSE)
  }

  if (!file.exists(script_path)) {
    stop(paste0("Workbook export script not found: ", script_path), call. = FALSE)
  }

  payload <- purrr::imap(tables, function(df, sheet_name) {
    table_to_workbook_payload(df)
  })

  payload_path <- tempfile("streamcurves_workbook_", fileext = ".json")
  on.exit(unlink(payload_path, force = TRUE), add = TRUE)

  jsonlite::write_json(
    payload,
    path = payload_path,
    auto_unbox = TRUE,
    pretty = FALSE,
    null = "null",
    na = "null"
  )

  result <- system2(
    python,
    c(script_path, payload_path, output_path),
    stdout = TRUE,
    stderr = TRUE
  )

  exit_status <- attr(result, "status") %||% 0L
  if (!identical(exit_status, 0L)) {
    stop(
      paste0(
        "Workbook export failed",
        if (length(result) > 0) paste0(": ", paste(result, collapse = "\n")) else "."
      ),
      call. = FALSE
    )
  }

  invisible(output_path)
}

read_input_workbook <- function(input_path) {
  cli::cli_alert_info("Loading workbook input from {.file {input_path}}")

  if (!file.exists(input_path)) {
    stop(paste0("Input workbook not found: ", input_path), call. = FALSE)
  }

  if (!tolower(tools::file_ext(input_path)) %in% c("xlsx")) {
    stop("Input workbook must be an .xlsx file.", call. = FALSE)
  }

  sheets <- readxl::excel_sheets(input_path)
  missing_sheets <- setdiff(required_workbook_sheets(), sheets)
  if (length(missing_sheets) > 0) {
    stop(
      paste0(
        "Workbook is missing required sheets: ",
        paste(missing_sheets, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  specs <- workbook_sheet_specs()
  tables <- purrr::imap(specs, function(spec, sheet_name) {
    if (!(sheet_name %in% sheets)) {
      return(ensure_workbook_sheet_columns(data.frame(), sheet_name))
    }

    read_required_sheet(input_path, sheet_name, spec$required)
  })

  build_input_bundle_from_tables(tables)
}
