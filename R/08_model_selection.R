## ── 08: Model Selection ─────────────────────────────────────────────────────
## BIC-based ranking and final model proposal.

library(dplyr)
library(cli)

#' Select final models for all metrics
#'
#' @param model_candidates Tibble from run_all_model_building()$all_candidates
#' @param predictor_importance Tibble from run_all_model_building()$all_importance
#' @param metric_config Parsed metric_registry.yaml
#' @return Tibble with one row per metric containing the selected model
select_final_models <- function(model_candidates, predictor_importance, metric_config) {

  cli::cli_alert_info("Selecting final models...")

  if (nrow(model_candidates) == 0) {
    cli::cli_alert_warning("No model candidates to select from")
    return(tibble::tibble())
  }

  metrics_with_models <- unique(model_candidates$metric)

  selections <- purrr::map_dfr(metrics_with_models, function(metric_key) {

    mc <- metric_config[[metric_key]]
    cands <- model_candidates |> dplyr::filter(metric == metric_key)

    if (nrow(cands) == 0) {
      return(tibble::tibble(
        metric          = metric_key,
        selected_rank   = NA_integer_,
        selected_model_id = NA_integer_,
        predictors      = NA_character_,
        n_predictors    = NA_integer_,
        r_squared       = NA_real_,
        adj_r2          = NA_real_,
        bic             = NA_real_,
        delta_bic       = NA_real_,
        n_obs           = NA_integer_,
        n_top_models    = NA_integer_,
        selection_method = NA_character_,
        needs_review    = FALSE,
        review_reason   = NA_character_
      ))
    }

    ## Top models (ΔBIC < 2)
    top <- cands |> dplyr::filter(delta_bic < 2)
    n_top <- nrow(top)

    ## Select most parsimonious among top models (fewest predictors, then lowest BIC)
    selected <- top |>
      dplyr::arrange(n_predictors, bic) |>
      dplyr::slice(1)

    ## ── Flags ───────────────────────────────────────────────────────────────
    needs_review <- FALSE
    review_reasons <- c()

    ## Multiple top models
    if (n_top > 3) {
      needs_review <- TRUE
      review_reasons <- c(review_reasons, "many_top_models")
    }

    ## Too simple (intercept-only or 1 predictor)
    if (selected$n_predictors <= 1) {
      needs_review <- TRUE
      review_reasons <- c(review_reasons, "too_simple")
    }

    ## Too complex (> 5 predictors with only 39 obs)
    if (selected$n_predictors > 5) {
      needs_review <- TRUE
      review_reasons <- c(review_reasons, "too_complex")
    }

    ## Low R²
    if (!is.na(selected$adj_r2) && selected$adj_r2 < 0.15) {
      needs_review <- TRUE
      review_reasons <- c(review_reasons, "low_r_squared")
    }

    ## Tied top models (same BIC within 0.1)
    if (n_top > 1) {
      bic_range <- max(top$bic) - min(top$bic)
      if (bic_range < 0.5) {
        needs_review <- TRUE
        review_reasons <- c(review_reasons, "tied_top_models")
      }
    }

    tibble::tibble(
      metric           = metric_key,
      selected_rank    = selected$rank,
      selected_model_id = selected$model_id,
      predictors       = selected$predictors,
      n_predictors     = selected$n_predictors,
      r_squared        = selected$r_squared,
      adj_r2           = selected$adj_r2,
      bic              = selected$bic,
      delta_bic        = selected$delta_bic,
      n_obs            = selected$n_obs,
      n_top_models     = n_top,
      selection_method = "min_bic_parsimonious",
      needs_review     = needs_review,
      review_reason    = if (length(review_reasons) > 0) paste(review_reasons, collapse = "; ") else NA_character_
    )
  })

  n_selected <- sum(!is.na(selections$predictors))
  n_review <- sum(selections$needs_review, na.rm = TRUE)

  cli::cli_alert_success(
    "Model selection complete: {n_selected} models selected, {n_review} need review"
  )

  selections
}
