## ── 01: Data Loading ────────────────────────────────────────────────────────
## Imports raw data from CSV (and optionally XLSX).

library(readr)
library(stringr)
library(cli)

#' Load summary data from CSV
#'
#' @param csv_path Path to a CSV data file
#' @param xlsx_path Optional path to an XLSX file (for reference)
#' @return A tibble of raw data with trimmed character columns
load_data <- function(csv_path, xlsx_path = NULL) {

  cli::cli_alert_info("Loading data from {.file {csv_path}}")

  ## Read CSV
  raw_data <- readr::read_csv(csv_path, show_col_types = FALSE)

  ## Trim whitespace from all character columns
  raw_data <- raw_data |>
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim))

  cli::cli_alert_success(
    "Loaded {nrow(raw_data)} rows x {ncol(raw_data)} columns"
  )

  ## Log column names
  cli::cli_alert_info("Columns: {paste(names(raw_data), collapse = ', ')}")

  ## Optionally load XLSX for cross-reference

  if (!is.null(xlsx_path) && file.exists(xlsx_path)) {
    cli::cli_alert_info("XLSX reference file available at {.file {xlsx_path}}")
  }

  raw_data
}
