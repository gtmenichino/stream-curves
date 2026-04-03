## -- 01: Workbook Loading -----------------------------------------------------
## Thin wrapper around the shared workbook parser.

library(cli)

#' Load a StreamCurves workbook
#'
#' @param input_path Path to an input .xlsx workbook
#' @return Named list with raw_data, configs, and metadata tables
load_data <- function(input_path) {
  read_input_workbook(input_path)
}
