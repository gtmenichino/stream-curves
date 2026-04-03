## -- 02: Data Cleaning --------------------------------------------------------
## Standardizes factor handling and runs QA checks from workbook metadata.

library(dplyr)
library(cli)

factor_columns_from_metadata <- function(strat_config, factor_recode_config) {
  strat_factor_cols <- purrr::map_chr(names(strat_config), function(sk) {
    sc <- strat_config[[sk]]
    if (!identical(sc$type, "single")) {
      return(NA_character_)
    }

    if (!identical(sc$source_data_type %||% NA_character_, "categorical")) {
      return(NA_character_)
    }

    sc$source_column %||% sc$column_name %||% NA_character_
  })

  recode_source_cols <- purrr::map_chr(names(factor_recode_config %||% list()), function(rk) {
    factor_recode_config[[rk]]$source_column %||% NA_character_
  })

  unique(stats::na.omit(c(strat_factor_cols, recode_source_cols)))
}

factor_recode_target_columns <- function(factor_recode_config) {
  if (length(factor_recode_config %||% list()) == 0) {
    return(character(0))
  }

  unique(stats::na.omit(purrr::map_chr(
    names(factor_recode_config),
    function(rk) factor_recode_config[[rk]]$target_column %||% NA_character_
  )))
}

expected_levels_for_column <- function(strat_config, column_name) {
  matches <- purrr::keep(strat_config, function(sc) {
    identical(sc$type, "single") &&
      !isTRUE(sc$is_custom_grouping) &&
      identical(sc$column_name %||% NA_character_, column_name) &&
      length(sc$levels %||% character(0)) > 0
  })

  if (length(matches) == 0) {
    return(character(0))
  }

  unique(unlist(purrr::map(matches, "levels"), use.names = FALSE))
}

#' Clean and validate data
#'
#' @param raw_data Tibble from load_data()
#' @param metric_config Parsed metric config
#' @param strat_config Parsed stratification config
#' @param factor_recode_config Parsed factor recode config
#' @return List with components: data (cleaned tibble), qa_log (tibble of QA entries)
clean_data <- function(raw_data,
                       metric_config,
                       strat_config,
                       factor_recode_config = list()) {

  cli::cli_alert_info("Cleaning data...")
  qa_log <- tibble::tibble(step = character(), message = character(), level = character())

  data <- raw_data |>
    trim_character_columns()

  factor_vars <- factor_columns_from_metadata(strat_config, factor_recode_config)
  derived_factor_targets <- factor_recode_target_columns(factor_recode_config)

  for (var in factor_vars) {
    if (!var %in% names(data)) {
      if (var %in% derived_factor_targets) {
        next
      }

      msg <- paste0("Expected categorical column '", var, "' not found in data")
      qa_log <- dplyr::bind_rows(qa_log, tibble::tibble(
        step = "factor_conversion", message = msg, level = "warning"
      ))
      cli::cli_alert_warning(msg)
      next
    }

    expected_levels <- expected_levels_for_column(strat_config, var)
    if (length(expected_levels) > 0) {
      actual_levels <- sort(unique(as.character(data[[var]][!is.na(data[[var]])])))
      unexpected <- setdiff(actual_levels, expected_levels)
      if (length(unexpected) > 0) {
        msg <- paste0(var, ": unexpected levels found: ", paste(unexpected, collapse = ", "))
        qa_log <- dplyr::bind_rows(qa_log, tibble::tibble(
          step = "factor_conversion", message = msg, level = "warning"
        ))
        cli::cli_alert_warning(msg)
      }
    }

    data[[var]] <- factor(data[[var]])
    cli::cli_alert_info("Converted {.var {var}} to factor ({length(levels(data[[var]]))} levels)")
  }

  missing_summary <- sapply(data, function(x) sum(is.na(x)))
  cols_with_missing <- missing_summary[missing_summary > 0]

  if (length(cols_with_missing) > 0) {
    cli::cli_alert_info("Columns with missing data:")
    for (col_name in names(cols_with_missing)) {
      n_miss <- cols_with_missing[[col_name]]
      pct <- round(100 * n_miss / nrow(data), 1)
      cli::cli_alert_info("  {col_name}: {n_miss} ({pct}%)")
      qa_log <- dplyr::bind_rows(qa_log, tibble::tibble(
        step = "missing_data",
        message = paste0(col_name, ": ", n_miss, " missing (", pct, "%)"),
        level = if (pct > 50) "warning" else "info"
      ))
    }
  }

  cli::cli_alert_success("Clean data: {nrow(data)} rows x {ncol(data)} columns")

  qa_log <- dplyr::bind_rows(qa_log, tibble::tibble(
    step = "clean_summary",
    message = paste0("Cleaned data: ", nrow(data), " rows x ", ncol(data), " columns"),
    level = "info"
  ))

  list(data = data, qa_log = qa_log)
}
