## ── 05b: Effect Size Computation ─────────────────────────────────────────────
## Computes effect sizes for stratification screening results.
## No new packages — uses base R stats functions.

library(dplyr)
library(cli)

#' Compute effect sizes for stratification candidates
#'
#' @param data Tibble with derived variables
#' @param metric_key Metric key from metric_registry
#' @param strat_keys Character vector of stratification keys to evaluate
#' @param metric_config Parsed metric_registry.yaml
#' @param strat_config Parsed stratification_registry.yaml
#' @return Tibble with effect size measures per stratification
compute_effect_sizes <- function(data, metric_key, strat_keys,
                                  metric_config, strat_config) {

  mc <- metric_config[[metric_key]]
  col_name <- mc$column_name

  results <- purrr::map_dfr(strat_keys, function(strat_key) {
    sc <- strat_config[[strat_key]]

    ## Skip paired stratifications
    if (!is.null(sc$type) && sc$type == "paired") {
      return(tibble::tibble(
        metric = metric_key, stratification = strat_key,
        epsilon_squared = NA_real_, eta_squared = NA_real_,
        rank_biserial_r = NA_real_, effect_size_label = "not_applicable",
        variance_explained_pct = NA_real_
      ))
    }

    strat_col <- sc$column_name

    ## Check columns exist
    if (!col_name %in% names(data) || !strat_col %in% names(data)) {
      return(tibble::tibble(
        metric = metric_key, stratification = strat_key,
        epsilon_squared = NA_real_, eta_squared = NA_real_,
        rank_biserial_r = NA_real_, effect_size_label = "not_applicable",
        variance_explained_pct = NA_real_
      ))
    }

    df <- data |>
      dplyr::select(dplyr::all_of(c(col_name, strat_col))) |>
      tidyr::drop_na()

    df[[strat_col]] <- factor(df[[strat_col]])
    df <- df |> dplyr::filter(!is.na(.data[[strat_col]]))

    n <- nrow(df)
    groups <- levels(df[[strat_col]])
    n_groups <- length(unique(df[[strat_col]][!is.na(df[[strat_col]])]))

    if (n < 3 || n_groups < 2) {
      return(tibble::tibble(
        metric = metric_key, stratification = strat_key,
        epsilon_squared = NA_real_, eta_squared = NA_real_,
        rank_biserial_r = NA_real_, effect_size_label = "not_applicable",
        variance_explained_pct = NA_real_
      ))
    }

    ## ── Epsilon-squared (KW effect size): H / (n - 1) ──────────────────────
    epsilon_sq <- NA_real_
    kw <- tryCatch(
      kruskal.test(as.formula(paste0("`", col_name, "` ~ `", strat_col, "`")), data = df),
      error = function(e) NULL
    )
    if (!is.null(kw)) {
      epsilon_sq <- kw$statistic / (n - 1)
    }

    ## ── Eta-squared (one-way ANOVA): SS_between / SS_total ─────────────────
    eta_sq <- NA_real_
    aov_fit <- tryCatch(
      aov(as.formula(paste0("`", col_name, "` ~ `", strat_col, "`")), data = df),
      error = function(e) NULL
    )
    if (!is.null(aov_fit)) {
      ss <- summary(aov_fit)[[1]]
      ss_between <- ss[["Sum Sq"]][1]
      ss_total <- sum(ss[["Sum Sq"]])
      if (ss_total > 0) {
        eta_sq <- ss_between / ss_total
      }
    }

    ## ── Rank-biserial r (for 2-group Wilcoxon): r = 1 - (2*W) / (n1*n2) ───
    rank_bis_r <- NA_real_
    if (n_groups == 2) {
      g1_vals <- df |> dplyr::filter(.data[[strat_col]] == groups[1]) |> dplyr::pull(!!col_name)
      g2_vals <- df |> dplyr::filter(.data[[strat_col]] == groups[2]) |> dplyr::pull(!!col_name)
      wt <- tryCatch(
        wilcox.test(g1_vals, g2_vals, exact = FALSE),
        error = function(e) NULL
      )
      if (!is.null(wt)) {
        n1 <- length(g1_vals)
        n2 <- length(g2_vals)
        if (n1 > 0 && n2 > 0) {
          rank_bis_r <- 1 - (2 * wt$statistic) / (n1 * n2)
        }
      }
    }

    ## ── Label effect size ───────────────────────────────────────────────────
    ## Use epsilon-squared as primary effect size measure
    es <- if (!is.na(epsilon_sq)) epsilon_sq else eta_sq
    label <- dplyr::case_when(
      is.na(es)   ~ "not_applicable",
      es < 0.01   ~ "negligible",
      es < 0.06   ~ "small",
      es < 0.14   ~ "medium",
      TRUE         ~ "large"
    )

    var_explained <- if (!is.na(eta_sq)) round(eta_sq * 100, 2) else NA_real_

    tibble::tibble(
      metric = metric_key,
      stratification = strat_key,
      epsilon_squared = as.numeric(epsilon_sq),
      eta_squared = eta_sq,
      rank_biserial_r = as.numeric(rank_bis_r),
      effect_size_label = label,
      variance_explained_pct = var_explained
    )
  })

  results
}
