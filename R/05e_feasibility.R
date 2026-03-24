## ── 05e: Operational Feasibility Assessment ──────────────────────────────────
## Evaluates practical feasibility of stratification candidates.

library(dplyr)
library(cli)

#' Assess operational feasibility of stratification candidates
#'
#' @param data Tibble with derived variables
#' @param strat_keys Character vector of stratification keys to evaluate
#' @param strat_config Parsed stratification_registry.yaml
#' @return Tibble with feasibility assessment per stratification
assess_feasibility <- function(data, strat_keys, strat_config) {

  results <- purrr::map_dfr(strat_keys, function(strat_key) {
    sc <- strat_config[[strat_key]]

    ## Skip paired stratifications
    if (!is.null(sc$type) && sc$type == "paired") {
      return(tibble::tibble(
        stratification = strat_key,
        n_levels = NA_integer_,
        min_group_n = NA_integer_,
        max_group_n = NA_integer_,
        pct_sparse_cells = NA_real_,
        data_completeness_pct = NA_real_,
        feasibility_flag = "not_applicable"
      ))
    }

    strat_col <- sc$column_name
    min_group_size <- sc$min_group_size %||% 5

    ## Check column exists
    if (!strat_col %in% names(data)) {
      return(tibble::tibble(
        stratification = strat_key,
        n_levels = NA_integer_,
        min_group_n = NA_integer_,
        max_group_n = NA_integer_,
        pct_sparse_cells = NA_real_,
        data_completeness_pct = NA_real_,
        feasibility_flag = "infeasible"
      ))
    }

    col_data <- data[[strat_col]]
    n_total <- length(col_data)
    n_complete <- sum(!is.na(col_data))
    completeness <- round(n_complete / n_total * 100, 1)

    ## Group sizes
    groups <- table(col_data, useNA = "no")
    n_levels <- length(groups)
    min_n <- if (n_levels > 0) min(groups) else 0L
    max_n <- if (n_levels > 0) max(groups) else 0L
    n_sparse <- sum(groups < min_group_size)
    pct_sparse <- if (n_levels > 0) round(n_sparse / n_levels * 100, 1) else 100

    ## Feasibility classification
    flag <- dplyr::case_when(
      n_levels < 2                          ~ "infeasible",
      min_n < 3                             ~ "infeasible",
      pct_sparse > 50                       ~ "infeasible",
      min_n < min_group_size                ~ "marginal",
      completeness < 80                     ~ "marginal",
      min_n >= min_group_size &
        completeness >= 90 &
        pct_sparse == 0                     ~ "feasible",
      TRUE                                  ~ "marginal"
    )

    tibble::tibble(
      stratification = strat_key,
      n_levels = as.integer(n_levels),
      min_group_n = as.integer(min_n),
      max_group_n = as.integer(max_n),
      pct_sparse_cells = pct_sparse,
      data_completeness_pct = completeness,
      feasibility_flag = flag
    )
  })

  results
}
