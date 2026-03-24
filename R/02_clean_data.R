## ── 02: Data Cleaning ───────────────────────────────────────────────────────
## Standardizes names, converts factors, runs QA checks.

library(dplyr)
library(forcats)
library(cli)

#' Clean and validate data
#'
#' @param raw_data Tibble from load_data()
#' @param metric_config Parsed metric_registry.yaml
#' @param strat_config Parsed stratification_registry.yaml
#' @return List with components: data (cleaned tibble), qa_log (tibble of QA entries)
clean_data <- function(raw_data, metric_config, strat_config) {

  cli::cli_alert_info("Cleaning data...")
  qa_log <- tibble::tibble(step = character(), message = character(), level = character())

  data <- raw_data

  ## ── Convert grouping variables to factors ─────────────────────────────────
  factor_vars <- c("Ecoregion", "DACAT", "Bedmaterial", "FlowRegime",
                    "valleytype", "BEHI_NBS", "StreamType", "CFPs", "AB")

  for (var in factor_vars) {
    if (var %in% names(data)) {
      ## Get expected levels from strat_config if available
      if (var %in% names(strat_config) && !is.null(strat_config[[var]]$levels)) {
        expected_levels <- strat_config[[var]]$levels
        actual_levels <- unique(data[[var]])
        unexpected <- setdiff(actual_levels, c(expected_levels, NA))
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
    } else {
      msg <- paste0("Expected column '", var, "' not found in data")
      qa_log <- dplyr::bind_rows(qa_log, tibble::tibble(
        step = "factor_conversion", message = msg, level = "warning"
      ))
      cli::cli_alert_warning(msg)
    }
  }

  ## ── Log missing data per column ───────────────────────────────────────────
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

  ## ── Row/column summary ────────────────────────────────────────────────────
  cli::cli_alert_success("Clean data: {nrow(data)} rows x {ncol(data)} columns")

  qa_log <- dplyr::bind_rows(qa_log, tibble::tibble(
    step = "clean_summary",
    message = paste0("Cleaned data: ", nrow(data), " rows x ", ncol(data), " columns"),
    level = "info"
  ))

  list(data = data, qa_log = qa_log)
}
