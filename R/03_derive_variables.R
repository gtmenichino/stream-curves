## ── 03: Derived Variables & Factor Recoding ─────────────────────────────────
## Computes derived variables and applies factor collapsing from config.

library(dplyr)
library(forcats)
library(cli)

#' Derive computed variables and apply factor recoding
#'
#' @param data Cleaned tibble from clean_data()
#' @param factor_recode_config Parsed factor_recode_registry.yaml
#' @param predictor_config Parsed predictor_registry.yaml
#' @return Tibble with derived variables and recoded factors added
derive_variables <- function(data, factor_recode_config, predictor_config) {

  cli::cli_alert_info("Deriving variables and recoding factors...")

  ## ── Compute derived continuous variables ──────────────────────────────────
  ## Each derivation is guarded: skipped if source columns are missing.

  derived_msgs <- character(0)

  if (all(c("L_Bufferwidth", "R_Bufferwidth") %in% names(data))) {
    data <- data |> dplyr::mutate(bufferwidth = L_Bufferwidth + R_Bufferwidth)
    derived_msgs <- c(derived_msgs, "bufferwidth")
  } else {
    cli::cli_alert_info("Skipping {.var bufferwidth}: requires L_Bufferwidth and R_Bufferwidth")
  }

  if ("DA_mi2" %in% names(data)) {
    data <- data |> dplyr::mutate(DA_km2 = DA_mi2 * 2.58999)
    derived_msgs <- c(derived_msgs, "DA_km2")
  } else {
    cli::cli_alert_info("Skipping {.var DA_km2}: requires DA_mi2")
  }

  if ("Slope_per" %in% names(data)) {
    data <- data |> dplyr::mutate(Slope_unitless = Slope_per / 100)
    derived_msgs <- c(derived_msgs, "Slope_unitless")
  } else {
    cli::cli_alert_info("Skipping {.var Slope_unitless}: requires Slope_per")
  }

  if (all(c("DA_km2", "Slope_unitless") %in% names(data)) ||
      (all(c("DA_mi2", "Slope_per") %in% names(data)))) {
    ## Ensure prerequisites exist (they may have just been created above)
    if ("DA_km2" %in% names(data) && "Slope_unitless" %in% names(data)) {
      data <- data |> dplyr::mutate(streampower = DA_km2 * Slope_unitless)
      derived_msgs <- c(derived_msgs, "streampower")
    }
  } else {
    cli::cli_alert_info("Skipping {.var streampower}: requires DA_km2 and Slope_unitless")
  }

  if (length(derived_msgs) > 0) {
    cli::cli_alert_success("Derived: {paste(derived_msgs, collapse = ', ')}")
  }

  ## ── Apply factor collapsing from config ───────────────────────────────────

  for (recode_name in names(factor_recode_config)) {
    recode <- factor_recode_config[[recode_name]]
    src_col <- recode$source_column
    tgt_col <- recode$target_column

    if (!src_col %in% names(data)) {
      cli::cli_alert_warning("Source column {.var {src_col}} not found for {recode_name}, skipping")
      next
    }

    ## Ensure source is a factor
    if (!is.factor(data[[src_col]])) {
      data[[src_col]] <- factor(data[[src_col]])
    }

    ## Build collapse map (list of character vectors)
    collapse_map <- recode$collapse_map

    ## Apply fct_collapse
    data[[tgt_col]] <- forcats::fct_collapse(data[[src_col]], !!!collapse_map)

    n_levels <- length(levels(data[[tgt_col]]))
    cli::cli_alert_success(
      "Recoded {.var {src_col}} -> {.var {tgt_col}} ({n_levels} levels: {paste(levels(data[[tgt_col]]), collapse = ', ')})"
    )
  }

  ## ── Validate derived columns ──────────────────────────────────────────────

  derived_cols <- c("bufferwidth", "DA_km2", "Slope_unitless", "streampower")
  for (col in derived_cols) {
    if (col %in% names(data)) {
      n_na <- sum(is.na(data[[col]]))
      if (n_na > 0) {
        cli::cli_alert_warning("Derived column {.var {col}} has {n_na} unexpected NAs")
      }
    }
  }

  cli::cli_alert_success("Variable derivation complete: {ncol(data)} total columns")

  data
}
