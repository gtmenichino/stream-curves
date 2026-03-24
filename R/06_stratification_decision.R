## ── 06: Stratification Decision ─────────────────────────────────────────────
## Decision logic for selecting optimal stratification per metric.
## Extended to accept optional layer scores from multi-layer evaluation.

library(dplyr)
library(cli)

#' Make stratification decisions for all metrics
#'
#' @param screening_results Tibble from run_all_stratification_screening()$results
#' @param pairwise_results Tibble from run_all_stratification_screening()$pairwise
#' @param metric_config Parsed metric_registry.yaml
#' @param strat_config Parsed stratification_registry.yaml
#' @param effect_sizes Optional tibble from compute_effect_sizes()
#' @param relevance_scores Optional tibble from layer 4 (practical relevance)
#' @param feasibility Optional tibble from assess_feasibility()
#' @return Tibble with one row per metric containing the stratification decision
make_stratification_decisions <- function(screening_results, pairwise_results,
                                          metric_config, strat_config,
                                          effect_sizes = NULL,
                                          relevance_scores = NULL,
                                          feasibility = NULL) {

  cli::cli_alert_info("Making stratification decisions...")

  ## Determine if extended scoring is available
  has_effect <- !is.null(effect_sizes) && nrow(effect_sizes) > 0
  has_relevance <- !is.null(relevance_scores) && nrow(relevance_scores) > 0
  has_feasibility <- !is.null(feasibility) && nrow(feasibility) > 0

  decisions <- purrr::map_dfr(names(metric_config), function(metric_key) {

    mc <- metric_config[[metric_key]]

    ## Skip categorical metrics
    if (mc$metric_family %in% c("categorical")) {
      return(tibble::tibble(
        metric             = metric_key,
        decision_type      = "not_applicable",
        selected_strat     = NA_character_,
        selected_p_value   = NA_real_,
        selected_n_groups  = NA_integer_,
        selected_min_n     = NA_integer_,
        runner_up_strat    = NA_character_,
        runner_up_p_value  = NA_real_,
        needs_review       = FALSE,
        review_reason      = NA_character_,
        notes              = "Categorical metric — no stratification screening"
      ))
    }

    ## Get this metric's screening results
    mr <- screening_results |>
      dplyr::filter(metric == metric_key)

    if (nrow(mr) == 0) {
      return(tibble::tibble(
        metric = metric_key, decision_type = "none",
        selected_strat = NA_character_, selected_p_value = NA_real_,
        selected_n_groups = NA_integer_, selected_min_n = NA_integer_,
        runner_up_strat = NA_character_, runner_up_p_value = NA_real_,
        needs_review = FALSE, review_reason = NA_character_,
        notes = "No screening results available"
      ))
    }

    ## ── Score candidates ────────────────────────────────────────────────────
    ## Only consider single stratifications (not paired) for primary selection
    candidates <- mr |>
      dplyr::filter(!grepl("_x_", stratification)) |>
      dplyr::filter(!grepl("skipped", classification)) |>
      dplyr::filter(!is.na(p_value))

    if (nrow(candidates) == 0) {
      return(tibble::tibble(
        metric = metric_key, decision_type = "none",
        selected_strat = NA_character_, selected_p_value = NA_real_,
        selected_n_groups = NA_integer_, selected_min_n = NA_integer_,
        runner_up_strat = NA_character_, runner_up_p_value = NA_real_,
        needs_review = FALSE, review_reason = NA_character_,
        notes = "No valid candidates"
      ))
    }

    candidates <- candidates |>
      dplyr::mutate(
        ## Significance score (0-1, lower p = higher score)
        sig_score = dplyr::case_when(
          p_value < 0.01 ~ 1.0,
          p_value < 0.05 ~ 0.7,
          p_value < 0.10 ~ 0.3,
          TRUE           ~ 0.0
        ),
        ## Sample size adequacy (penalty for small groups)
        size_score = dplyr::case_when(
          min_group_n >= 10 ~ 1.0,
          min_group_n >= 5  ~ 0.7,
          min_group_n >= 3  ~ 0.3,
          TRUE              ~ 0.0
        ),
        ## Simplicity bonus (fewer groups preferred)
        simplicity_score = dplyr::case_when(
          n_groups <= 2 ~ 1.0,
          n_groups <= 3 ~ 0.8,
          n_groups <= 4 ~ 0.6,
          TRUE          ~ 0.4
        )
      )

    ## ── Extended scoring: add effect size, relevance, feasibility ──────────
    if (has_effect) {
      es_metric <- effect_sizes |>
        dplyr::filter(metric == metric_key) |>
        dplyr::select(stratification, epsilon_squared)
      candidates <- candidates |>
        dplyr::left_join(es_metric, by = "stratification") |>
        dplyr::mutate(
          effect_score = dplyr::case_when(
            is.na(epsilon_squared)     ~ 0.3,
            epsilon_squared >= 0.14    ~ 1.0,
            epsilon_squared >= 0.06    ~ 0.7,
            epsilon_squared >= 0.01    ~ 0.4,
            TRUE                       ~ 0.1
          )
        )
    } else {
      candidates$effect_score <- 0.5  # neutral if not available
    }

    if (has_relevance) {
      rel_metric <- relevance_scores |>
        dplyr::filter(metric == metric_key) |>
        dplyr::select(stratification, mean_score)
      candidates <- candidates |>
        dplyr::left_join(rel_metric, by = "stratification") |>
        dplyr::mutate(
          relevance_score = dplyr::case_when(
            is.na(mean_score)   ~ 0.5,
            mean_score >= 4.0   ~ 1.0,
            mean_score >= 3.0   ~ 0.7,
            mean_score >= 2.0   ~ 0.4,
            TRUE                ~ 0.1
          )
        )
    } else {
      candidates$relevance_score <- 0.5
    }

    ## ── Composite score (weights depend on available data) ────────────────
    if (has_effect || has_relevance) {
      ## Extended weights: sig 0.35, size 0.10, simplicity 0.10, effect 0.20, relevance 0.15, base = 0.90
      ## Remaining 0.10 reserved for feasibility check (applied as demotion)
      candidates <- candidates |>
        dplyr::mutate(
          total_score = sig_score * 0.35 +
                        size_score * 0.10 +
                        simplicity_score * 0.10 +
                        effect_score * 0.20 +
                        relevance_score * 0.15
        )
    } else {
      ## Original weights: sig 0.50, size 0.30, simplicity 0.20
      candidates <- candidates |>
        dplyr::mutate(
          total_score = sig_score * 0.5 + size_score * 0.3 + simplicity_score * 0.2
        )
    }

    ## ── Feasibility demotion ──────────────────────────────────────────────
    if (has_feasibility) {
      feas <- feasibility |>
        dplyr::select(stratification, feasibility_flag)
      candidates <- candidates |>
        dplyr::left_join(feas, by = "stratification") |>
        dplyr::mutate(
          total_score = dplyr::case_when(
            feasibility_flag == "infeasible" ~ 0,
            feasibility_flag == "marginal"   ~ total_score * 0.8,
            TRUE                             ~ total_score
          )
        )
    }

    candidates <- candidates |>
      dplyr::arrange(dplyr::desc(total_score), p_value)

    ## ── Decision ──────────────────────────────────────────────────────────
    best <- candidates |> dplyr::slice(1)

    any_sig <- any(candidates$p_value < 0.05, na.rm = TRUE)

    decision_type <- "none"
    selected_strat <- NA_character_
    if (any_sig && best$sig_score > 0) {
      decision_type <- "single"
      selected_strat <- best$stratification
    }

    ## Runner-up
    runner_up_strat <- NA_character_
    runner_up_p <- NA_real_
    if (nrow(candidates) > 1) {
      runner_up <- candidates |> dplyr::slice(2)
      runner_up_strat <- runner_up$stratification
      runner_up_p <- runner_up$p_value
    }

    ## ── Flags for review ──────────────────────────────────────────────────
    needs_review <- FALSE
    review_reasons <- c()

    if (nrow(candidates) > 1) {
      score_diff <- candidates$total_score[1] - candidates$total_score[2]
      if (score_diff < 0.1) {
        needs_review <- TRUE
        review_reasons <- c(review_reasons, "tied_top_candidates")
      }
    }

    if (!is.na(selected_strat) && best$min_group_n < 5) {
      needs_review <- TRUE
      review_reasons <- c(review_reasons, "sparse_groups")
    }

    paired <- mr |> dplyr::filter(grepl("_x_", stratification))
    if (nrow(paired) > 0 && any(paired$classification == "rejected_sparse")) {
      review_reasons <- c(review_reasons, "paired_strat_sparse")
    }

    if (!is.na(best$p_value) && best$p_value > 0.01 && best$p_value < 0.10) {
      needs_review <- TRUE
      review_reasons <- c(review_reasons, "borderline_significance")
    }

    tibble::tibble(
      metric             = metric_key,
      decision_type      = decision_type,
      selected_strat     = selected_strat,
      selected_p_value   = best$p_value,
      selected_n_groups  = best$n_groups,
      selected_min_n     = best$min_group_n,
      runner_up_strat    = runner_up_strat,
      runner_up_p_value  = runner_up_p,
      needs_review       = needs_review,
      review_reason      = if (length(review_reasons) > 0) paste(review_reasons, collapse = "; ") else NA_character_,
      notes              = NA_character_
    )
  })

  n_selected <- sum(decisions$decision_type == "single", na.rm = TRUE)
  n_none <- sum(decisions$decision_type == "none", na.rm = TRUE)
  n_review <- sum(decisions$needs_review, na.rm = TRUE)

  cli::cli_alert_success(
    "Decisions: {n_selected} stratified, {n_none} none, {n_review} need review"
  )

  decisions
}
