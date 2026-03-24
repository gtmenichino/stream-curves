## ── 04: Metric Precheck ─────────────────────────────────────────────────────
## Per-metric summary statistics and quality flags.

library(dplyr)
library(tibble)
library(cli)

#' Run precheck summary for all metrics
#'
#' @param data Tibble with derived variables
#' @param metric_config Parsed metric_registry.yaml
#' @return Tibble with one row per metric containing summary stats and flags
run_metric_precheck <- function(data, metric_config) {

  cli::cli_alert_info("Running metric precheck...")

  results <- purrr::map_dfr(names(metric_config), function(metric_key) {

    mc <- metric_config[[metric_key]]
    col_name <- mc$column_name

    ## Skip if column not in data
    if (!col_name %in% names(data)) {
      cli::cli_alert_warning("Column {.var {col_name}} not found for metric {metric_key}")
      return(tibble::tibble(
        metric       = metric_key,
        display_name = mc$display_name,
        column_name  = col_name,
        metric_family = mc$metric_family,
        n_obs        = NA_integer_,
        n_missing    = NA_integer_,
        pct_missing  = NA_real_,
        min          = NA_real_,
        q25          = NA_real_,
        median       = NA_real_,
        mean         = NA_real_,
        q75          = NA_real_,
        max          = NA_real_,
        sd           = NA_real_,
        iqr          = NA_real_,
        flag_low_variance      = NA,
        flag_impossible_values = NA,
        flag_low_n             = NA,
        precheck_status        = "missing_column"
      ))
    }

    vals <- data[[col_name]]

    ## Handle categorical metrics
    if (mc$metric_family %in% c("categorical")) {
      return(tibble::tibble(
        metric       = metric_key,
        display_name = mc$display_name,
        column_name  = col_name,
        metric_family = mc$metric_family,
        n_obs        = sum(!is.na(vals)),
        n_missing    = sum(is.na(vals)),
        pct_missing  = round(100 * sum(is.na(vals)) / length(vals), 1),
        min          = NA_real_,
        q25          = NA_real_,
        median       = NA_real_,
        mean         = NA_real_,
        q75          = NA_real_,
        max          = NA_real_,
        sd           = NA_real_,
        iqr          = NA_real_,
        flag_low_variance      = FALSE,
        flag_impossible_values = FALSE,
        flag_low_n             = sum(!is.na(vals)) < mc$min_sample_size,
        precheck_status        = "categorical"
      ))
    }

    ## Numeric metrics
    vals_num <- suppressWarnings(as.numeric(vals))
    n_obs     <- sum(!is.na(vals_num))
    n_missing <- sum(is.na(vals_num))
    pct_missing <- round(100 * n_missing / length(vals_num), 1)

    if (n_obs == 0) {
      return(tibble::tibble(
        metric = metric_key, display_name = mc$display_name,
        column_name = col_name, metric_family = mc$metric_family,
        n_obs = 0L, n_missing = n_missing, pct_missing = pct_missing,
        min = NA_real_, q25 = NA_real_, median = NA_real_, mean = NA_real_,
        q75 = NA_real_, max = NA_real_, sd = NA_real_, iqr = NA_real_,
        flag_low_variance = NA, flag_impossible_values = NA, flag_low_n = TRUE,
        precheck_status = "no_data"
      ))
    }

    v <- vals_num[!is.na(vals_num)]
    q <- quantile(v, probs = c(0.25, 0.75), na.rm = TRUE)

    ## Flags
    flag_low_var <- sd(v) < 0.001
    flag_impossible <- FALSE
    if (mc$metric_family == "proportion") {
      flag_impossible <- any(v < 0 | v > 100)
    }
    flag_low_n <- n_obs < mc$min_sample_size

    status <- "pass"
    if (flag_low_n) status <- "caution"
    if (n_obs == 0) status <- "fail"

    tibble::tibble(
      metric       = metric_key,
      display_name = mc$display_name,
      column_name  = col_name,
      metric_family = mc$metric_family,
      n_obs        = n_obs,
      n_missing    = n_missing,
      pct_missing  = pct_missing,
      min          = min(v),
      q25          = q[[1]],
      median       = median(v),
      mean         = mean(v),
      q75          = q[[2]],
      max          = max(v),
      sd           = sd(v),
      iqr          = IQR(v),
      flag_low_variance      = flag_low_var,
      flag_impossible_values = flag_impossible,
      flag_low_n             = flag_low_n,
      precheck_status        = status
    )
  })

  cli::cli_alert_success("Precheck complete: {nrow(results)} metrics evaluated")

  results
}
