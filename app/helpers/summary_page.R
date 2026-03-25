## -- Summary Page Helpers ------------------------------------------------------
## Shared state helpers for the cross-metric Summary page.

library(dplyr)
library(purrr)
library(tibble)

eligible_summary_metrics <- function(metric_config) {
  keys <- names(metric_config)
  keys[vapply(keys, function(mk) {
    mc <- metric_config[[mk]]
    !identical(mc$metric_family, "categorical") && !identical(mc$include_in_summary, FALSE)
  }, logical(1))]
}

get_first_value <- function(df, column, default = NA) {
  if (is.null(df) || !is.data.frame(df) || !(column %in% names(df)) || nrow(df) == 0) {
    return(default)
  }
  value <- df[[column]][1]
  if (length(value) == 0 || is.null(value)) default else value
}

metric_direction_label <- function(metric_config, metric) {
  mc <- metric_config[[metric]]
  if (is.null(mc)) return("Unknown")
  if (isTRUE(mc$higher_is_better)) return("Higher is better")
  if (isFALSE(mc$higher_is_better)) return("Lower is better")
  "Neutral"
}

get_metric_precheck_row <- function(rv, metric) {
  if (is.null(rv$precheck_df)) return(tibble::tibble())
  rv$precheck_df |>
    dplyr::filter(metric == !!metric) |>
    dplyr::slice(1)
}

get_metric_config_allowed_strats <- function(rv, metric) {
  mc <- rv$metric_config[[metric]]
  allowed <- mc$allowed_stratifications %||% character(0)
  allowed[allowed %in% names(rv$strat_config)]
}

get_metric_allowed_strats <- function(rv, metric) {
  override <- rv$summary_available_overrides[[metric]] %||% NULL
  if (!is.null(override)) {
    allowed <- override
  } else {
    allowed <- get_metric_config_allowed_strats(rv, metric)
  }
  allowed[allowed %in% names(rv$strat_config)]
}

get_summary_available_choices <- function(rv) {
  keys <- names(rv$strat_config)
  if (length(keys) == 0) return(character(0))

  labels <- vapply(keys, function(sk) {
    get_strat_display_name(rv, sk)
  }, character(1))

  keys[order(labels, keys)]
}

get_strat_display_name <- function(rv, strat_key) {
  if (is.null(strat_key) || is.na(strat_key) || strat_key == "") return("None")
  cfg <- rv$strat_config[[strat_key]]
  cfg$display_name %||% strat_key
}

make_named_strat_choices <- function(strat_keys, label_map) {
  if (length(strat_keys) == 0) return(stats::setNames(character(0), character(0)))
  stats::setNames(strat_keys, label_map[strat_keys])
}

format_strat_list <- function(rv, strat_keys) {
  if (length(strat_keys) == 0) return("None")
  paste(vapply(strat_keys, function(sk) get_strat_display_name(rv, sk), character(1)),
        collapse = ", ")
}

empty_summary_note_store <- function() {
  list(
    "Phase 1" = list(),
    "Phase 2" = list(),
    "Phase 3" = list(),
    "Phase 4" = list()
  )
}

normalize_summary_note_items <- function(items) {
  if (is.null(items) || length(items) == 0) return(list())

  purrr::keep(lapply(items, function(item) {
    if (is.null(item$text) || identical(item$text, "")) {
      return(NULL)
    }
    list(
      level = item$level %||% "info",
      text = item$text
    )
  }), Negate(is.null))
}

get_metric_summary_edit_notes <- function(rv, metric) {
  metric_notes <- rv$summary_edit_notes[[metric]]
  if (is.null(metric_notes)) {
    return(empty_summary_note_store())
  }

  out <- empty_summary_note_store()
  for (phase in names(out)) {
    out[[phase]] <- normalize_summary_note_items(metric_notes[[phase]])
  }
  out
}

set_metric_summary_edit_notes <- function(rv, metric, phase, items) {
  metric_notes <- rv$summary_edit_notes[[metric]] %||% empty_summary_note_store()
  metric_notes[[phase]] <- normalize_summary_note_items(items)
  rv$summary_edit_notes[[metric]] <- metric_notes
  invisible(NULL)
}

clear_metric_summary_edit_notes <- function(rv, metric, phase = NULL) {
  if (is.null(phase)) {
    rv$summary_edit_notes[[metric]] <- empty_summary_note_store()
  } else {
    metric_notes <- rv$summary_edit_notes[[metric]] %||% empty_summary_note_store()
    metric_notes[[phase]] <- list()
    rv$summary_edit_notes[[metric]] <- metric_notes
  }
  invisible(NULL)
}

get_global_phase2_passed <- function(rv, metric = NULL) {
  if (is.null(rv$phase2_ranking) || nrow(rv$phase2_ranking) == 0) {
    out <- character(0)
  } else {
    out <- rv$phase2_ranking |>
      dplyr::filter(tier == "Broad-Use Candidate") |>
      dplyr::pull(stratification) |>
      unique()
  }

  if (!is.null(metric)) {
    out <- intersect(get_metric_allowed_strats(rv, metric), out)
  }
  out
}

get_metric_phase2_passed <- function(rv, metric) {
  allowed <- get_metric_allowed_strats(rv, metric)
  if (metric %in% names(rv$phase2_metric_overrides)) {
    return(intersect(allowed, rv$phase2_metric_overrides[[metric]] %||% character(0)))
  }
  intersect(allowed, get_global_phase2_passed(rv, metric))
}

set_metric_phase2_override <- function(rv, metric, selected) {
  rv$phase2_metric_overrides[[metric]] <- intersect(get_metric_allowed_strats(rv, metric), selected)
  invisible(NULL)
}

empty_phase2_settings <- function() {
  list(
    metric_filter = NULL,
    strat_filter = NULL,
    sig_threshold = 0.05,
    support_threshold = 0.5
  )
}

get_phase2_metric_choices <- function(rv) {
  keys <- names(rv$all_layer1_results)
  keys[vapply(keys, function(mk) {
    df <- rv$all_layer1_results[[mk]]
    is.data.frame(df) && nrow(df) > 0
  }, logical(1))]
}

get_phase2_strat_choices <- function(rv, metric_filter = get_phase2_metric_choices(rv)) {
  metric_filter <- intersect(metric_filter %||% character(0), names(rv$all_layer1_results))
  if (length(metric_filter) == 0) return(character(0))

  sort(unique(unlist(lapply(rv$all_layer1_results[metric_filter], function(df) {
    if (is.null(df) || nrow(df) == 0) return(character(0))
    unique(df$stratification %||% character(0))
  }), use.names = FALSE)))
}

normalize_phase2_settings <- function(rv, settings = rv$phase2_settings %||% empty_phase2_settings()) {
  defaults <- empty_phase2_settings()
  settings <- settings %||% defaults

  metric_choices <- get_phase2_metric_choices(rv)
  metric_filter <- intersect(settings$metric_filter %||% metric_choices, metric_choices)
  if (length(metric_filter) == 0 && length(metric_choices) > 0) {
    metric_filter <- metric_choices
  }

  strat_choices <- get_phase2_strat_choices(rv, metric_filter)
  strat_filter <- intersect(settings$strat_filter %||% strat_choices, strat_choices)
  if (length(strat_filter) == 0 && length(strat_choices) > 0) {
    strat_filter <- strat_choices
  }

  sig_threshold <- suppressWarnings(as.numeric(settings$sig_threshold %||% defaults$sig_threshold))
  if (!is.finite(sig_threshold)) sig_threshold <- defaults$sig_threshold
  sig_threshold <- min(max(sig_threshold, 0.01), 0.10)

  support_threshold <- suppressWarnings(as.numeric(settings$support_threshold %||% defaults$support_threshold))
  if (!is.finite(support_threshold)) support_threshold <- defaults$support_threshold
  support_threshold <- min(max(support_threshold, 0.10), 0.90)

  list(
    metric_filter = metric_filter,
    strat_filter = strat_filter,
    sig_threshold = sig_threshold,
    support_threshold = support_threshold
  )
}

set_phase2_settings <- function(rv, settings = rv$phase2_settings %||% empty_phase2_settings()) {
  rv$phase2_settings <- normalize_phase2_settings(rv, settings)
  invisible(rv$phase2_settings)
}

build_phase2_ranking <- function(result, phase1_cands, support_threshold) {
  if (is.null(result$summary) || nrow(result$summary) == 0) {
    return(NULL)
  }

  n_cand_metrics <- length(phase1_cands)

  result$summary |>
    dplyr::mutate(
      n_promising = if (n_cand_metrics == 0) 0L else sapply(stratification, function(sk) {
        sum(sapply(phase1_cands, function(df) {
          if (!is.null(df) && nrow(df) > 0) {
            row <- df |> dplyr::filter(stratification == sk)
            if (nrow(row) > 0) row$candidate_status[1] == "promising" else FALSE
          } else {
            FALSE
          }
        }))
      }),
      n_possible = if (n_cand_metrics == 0) 0L else sapply(stratification, function(sk) {
        sum(sapply(phase1_cands, function(df) {
          if (!is.null(df) && nrow(df) > 0) {
            row <- df |> dplyr::filter(stratification == sk)
            if (nrow(row) > 0) row$candidate_status[1] == "possible" else FALSE
          } else {
            FALSE
          }
        }))
      }),
      n_not_promising = if (n_cand_metrics == 0) 0L else sapply(stratification, function(sk) {
        sum(sapply(phase1_cands, function(df) {
          if (!is.null(df) && nrow(df) > 0) {
            row <- df |> dplyr::filter(stratification == sk)
            if (nrow(row) > 0) row$candidate_status[1] == "not_promising" else FALSE
          } else {
            FALSE
          }
        }))
      }),
      pct_promising_possible = (n_promising + n_possible) /
        pmax(n_promising + n_possible + n_not_promising, 1),
      tier = dplyr::case_when(
        consistency_score >= support_threshold ~ "Broad-Use Candidate",
        consistency_score >= support_threshold / 2 ~ "Metric-Specific Candidate",
        TRUE ~ "Weak Candidate"
      )
    )
}

recompute_phase2_shared <- function(rv, settings = rv$phase2_settings %||% empty_phase2_settings()) {
  settings <- normalize_phase2_settings(rv, settings)
  rv$phase2_settings <- settings

  if (length(settings$metric_filter) < 2 || length(settings$strat_filter) == 0) {
    rv$cross_metric_consistency <- NULL
    rv$phase2_ranking <- NULL
    return(invisible(NULL))
  }

  filtered_l1 <- rv$all_layer1_results[settings$metric_filter]
  filtered_l2 <- rv$all_layer2_results[intersect(settings$metric_filter, names(rv$all_layer2_results))]

  filtered_l1 <- lapply(filtered_l1, function(df) {
    if (is.null(df) || nrow(df) == 0) return(tibble::tibble())
    df |> dplyr::filter(stratification %in% settings$strat_filter)
  })
  filtered_l2 <- lapply(filtered_l2, function(df) {
    if (is.null(df) || nrow(df) == 0) return(tibble::tibble())
    df |> dplyr::filter(stratification %in% settings$strat_filter)
  })

  filtered_l1 <- filtered_l1[vapply(filtered_l1, nrow, integer(1)) > 0]
  filtered_l2 <- filtered_l2[vapply(filtered_l2, nrow, integer(1)) > 0]

  if (length(filtered_l1) < 2) {
    rv$cross_metric_consistency <- NULL
    rv$phase2_ranking <- NULL
    return(invisible(NULL))
  }

  result <- compute_strat_consistency(
    filtered_l1,
    filtered_l2,
    rv$metric_config,
    rv$strat_config,
    sig_threshold = settings$sig_threshold
  )

  ranking <- build_phase2_ranking(result, rv$phase1_candidates, settings$support_threshold)
  rv$cross_metric_consistency <- result
  rv$phase2_ranking <- ranking
  invisible(list(result = result, ranking = ranking, settings = settings))
}

auto_phase1_candidate_status <- function(p_val, es_label) {
  if (!is.na(p_val) && p_val < 0.05 && es_label %in% c("medium", "large")) {
    return("promising")
  }
  if (!is.na(p_val) && p_val < 0.10) {
    return("possible")
  }
  "not_promising"
}

build_metric_phase1_candidate_table_from_sources <- function(metric, allowed,
                                                             existing = NULL,
                                                             l1 = NULL,
                                                             l2 = NULL,
                                                             include_all_allowed = TRUE) {
  strat_keys <- character(0)
  if (include_all_allowed) strat_keys <- union(strat_keys, allowed)
  if (!is.null(existing) && nrow(existing) > 0) strat_keys <- union(strat_keys, existing$stratification)
  if (!is.null(l1) && nrow(l1) > 0) strat_keys <- union(strat_keys, l1$stratification)
  if (length(strat_keys) == 0) return(tibble::tibble())

  purrr::map_dfr(strat_keys, function(sk) {
    existing_row <- if (!is.null(existing) && nrow(existing) > 0) {
      existing |>
        dplyr::filter(stratification == sk) |>
        dplyr::slice(1)
    } else {
      tibble::tibble()
    }

    p_row <- if (!is.null(l1) && nrow(l1) > 0) {
      l1 |>
        dplyr::filter(stratification == sk) |>
        dplyr::slice(1)
    } else {
      tibble::tibble()
    }

    es_row <- if (!is.null(l2) && nrow(l2) > 0) {
      l2 |>
        dplyr::filter(stratification == sk) |>
        dplyr::slice(1)
    } else {
      tibble::tibble()
    }

    p_val <- get_first_value(existing_row, "p_value", get_first_value(p_row, "p_value", NA_real_))
    es_label <- get_first_value(existing_row, "effect_size_label",
                                get_first_value(es_row, "effect_size_label", "negligible"))
    status <- get_first_value(existing_row, "candidate_status",
                              auto_phase1_candidate_status(p_val, es_label))

    tibble::tibble(
      metric = metric,
      stratification = sk,
      p_value = p_val,
      epsilon_squared = get_first_value(existing_row, "epsilon_squared",
                                        get_first_value(es_row, "epsilon_squared", NA_real_)),
      effect_size_label = es_label,
      min_group_n = get_first_value(existing_row, "min_group_n",
                                    get_first_value(p_row, "min_group_n", NA_integer_)),
      candidate_status = status,
      reviewer_note = get_first_value(existing_row, "reviewer_note", "")
    )
  })
}

get_metric_phase1_candidate_table <- function(rv, metric, include_all_allowed = TRUE) {
  allowed <- get_metric_allowed_strats(rv, metric)
  existing <- rv$phase1_candidates[[metric]]
  l1 <- rv$all_layer1_results[[metric]]
  l2 <- rv$all_layer2_results[[metric]]
  build_metric_phase1_candidate_table_from_sources(
    metric = metric,
    allowed = allowed,
    existing = existing,
    l1 = l1,
    l2 = l2,
    include_all_allowed = include_all_allowed
  )
}

get_metric_phase1_selected <- function(rv, metric) {
  tbl <- get_metric_phase1_candidate_table(rv, metric)
  if (nrow(tbl) == 0) return(character(0))

  allowed <- get_metric_allowed_strats(rv, metric)
  tbl |>
    dplyr::filter(
      candidate_status %in% c("promising", "possible"),
      stratification %in% allowed
    ) |>
    dplyr::pull(stratification) |>
    unique()
}

set_metric_phase1_candidates <- function(rv, metric, selected) {
  allowed <- get_metric_allowed_strats(rv, metric)
  selected <- intersect(allowed, selected)
  base_tbl <- get_metric_phase1_candidate_table(rv, metric, include_all_allowed = TRUE)

  if (nrow(base_tbl) == 0 && length(allowed) > 0) {
    base_tbl <- tibble::tibble(
      metric = metric,
      stratification = allowed,
      p_value = NA_real_,
      epsilon_squared = NA_real_,
      effect_size_label = NA_character_,
      min_group_n = NA_integer_,
      candidate_status = "not_promising",
      reviewer_note = ""
    )
  }

  if (nrow(base_tbl) == 0) {
    rv$phase1_candidates[[metric]] <- tibble::tibble()
    return(invisible(NULL))
  }

  base_tbl <- base_tbl |>
    dplyr::mutate(
      candidate_status = dplyr::case_when(
        stratification %in% selected & candidate_status %in% c("promising", "possible") ~ candidate_status,
        stratification %in% selected ~ "possible",
        TRUE ~ "not_promising"
      )
    )

  rv$phase1_candidates[[metric]] <- base_tbl
  invisible(NULL)
}

get_metric_phase3_choices <- function(rv, metric) {
  union(get_metric_phase1_selected(rv, metric), get_metric_phase2_passed(rv, metric))
}

get_metric_phase3_selected <- function(rv, metric) {
  verified <- rv$phase3_verification[[metric]]
  if (!is.null(verified) && !is.null(verified$selected_strat)) {
    return(verified$selected_strat)
  }

  completed <- rv$completed_metrics[[metric]]
  if (!is.null(completed$strat_decision) && nrow(completed$strat_decision) > 0) {
    if (completed$strat_decision$decision_type[1] == "single") {
      return(completed$strat_decision$selected_strat[1] %||% "none")
    }
    return("none")
  }

  cached <- rv$metric_phase_cache[[metric]]
  if (!is.null(cached$strat_decision_user) && nrow(cached$strat_decision_user) > 0) {
    if (cached$strat_decision_user$decision_type[1] == "single") {
      return(cached$strat_decision_user$selected_strat[1] %||% "none")
    }
    return("none")
  }

  if (identical(rv$current_metric, metric) &&
      !is.null(rv$strat_decision_user) && nrow(rv$strat_decision_user) > 0) {
    if (rv$strat_decision_user$decision_type[1] == "single") {
      return(rv$strat_decision_user$selected_strat[1] %||% "none")
    }
    return("none")
  }

  "none"
}

build_metric_strat_decision <- function(rv, metric, selected_strat) {
  if (is.null(selected_strat) || is.na(selected_strat) || selected_strat == "" || selected_strat == "none") {
    return(tibble::tibble(
      metric = metric,
      decision_type = "none",
      selected_strat = NA_character_,
      selected_p_value = NA_real_,
      selected_n_groups = NA_integer_,
      selected_min_n = NA_integer_,
      runner_up_strat = NA_character_,
      runner_up_p_value = NA_real_,
      needs_review = FALSE,
      review_reason = NA_character_,
      notes = "Updated from Summary page"
    ))
  }

  l1 <- rv$all_layer1_results[[metric]]
  p1 <- rv$phase1_candidates[[metric]]

  row <- if (!is.null(l1) && nrow(l1) > 0) {
    l1 |>
      dplyr::filter(stratification == selected_strat) |>
      dplyr::slice(1)
  } else {
    tibble::tibble()
  }

  cand_row <- if (!is.null(p1) && nrow(p1) > 0) {
    p1 |>
      dplyr::filter(stratification == selected_strat) |>
      dplyr::slice(1)
  } else {
    tibble::tibble()
  }

  tibble::tibble(
    metric = metric,
    decision_type = "single",
    selected_strat = selected_strat,
    selected_p_value = get_first_value(row, "p_value", get_first_value(cand_row, "p_value", NA_real_)),
    selected_n_groups = get_first_value(row, "n_groups", NA_integer_),
    selected_min_n = get_first_value(row, "min_group_n", get_first_value(cand_row, "min_group_n", NA_integer_)),
    runner_up_strat = NA_character_,
    runner_up_p_value = NA_real_,
    needs_review = FALSE,
    review_reason = NA_character_,
    notes = "Updated from Summary page"
  )
}

sync_metric_decision_state <- function(rv, metric, decision_tbl) {
  if (identical(rv$current_metric, metric)) {
    rv$strat_decision_user <- decision_tbl
  }

  if (is.null(rv$metric_phase_cache[[metric]])) {
    rv$metric_phase_cache[[metric]] <- list()
  }
  rv$metric_phase_cache[[metric]]$strat_decision_user <- decision_tbl
  invisible(NULL)
}

set_metric_phase3_selected <- function(rv, metric, selected_strat) {
  choices <- get_metric_phase3_choices(rv, metric)
  normalized <- if (is.null(selected_strat) || is.na(selected_strat) || selected_strat == "" ||
                    !(selected_strat %in% choices)) "none" else selected_strat

  existing <- rv$phase3_verification[[metric]]
  rv$phase3_verification[[metric]] <- list(
    finalists = choices,
    pattern_results = existing$pattern_results %||% NULL,
    feasibility_results = existing$feasibility_results %||% NULL,
    verification_status = existing$verification_status %||%
      if (length(choices) > 0) stats::setNames(rep("verified", length(choices)), choices) else list(),
    selected_strat = normalized,
    justification = existing$justification %||% "Updated from Summary page"
  )

  sync_metric_decision_state(rv, metric, build_metric_strat_decision(rv, metric, normalized))
  invisible(normalized)
}

clear_metric_phase4_results <- function(rv, metric) {
  rv$completed_metrics[[metric]] <- NULL
  rv$stratum_results[[metric]] <- NULL

  if (identical(rv$current_metric, metric)) {
    rv$reference_curve <- NULL
    rv$current_stratum_level <- NULL
    rv$phase4_data <- NULL
  }

  if (is.null(rv$metric_phase_cache[[metric]])) {
    rv$metric_phase_cache[[metric]] <- list()
  }
  rv$metric_phase_cache[[metric]]$reference_curve <- NULL
  rv$metric_phase_cache[[metric]]$current_stratum_level <- NULL
  rv$metric_phase_cache[[metric]]$phase4_data <- NULL
  rv$metric_phase_cache[[metric]]$stratum_results <- NULL
  rv$metric_phase_cache[[metric]]$phase4_signature <- NULL
  invisible(NULL)
}

ensure_metric_phase_cache <- function(rv, metric) {
  if (is.null(rv$metric_phase_cache[[metric]])) {
    rv$metric_phase_cache[[metric]] <- list()
  }
  invisible(rv$metric_phase_cache[[metric]])
}

get_metric_phase4_decision_state <- function(rv, metric) {
  if (identical(rv$current_metric, metric) &&
      !is.null(rv$strat_decision_user) &&
      nrow(rv$strat_decision_user) > 0) {
    return(rv$strat_decision_user)
  }

  cached <- rv$metric_phase_cache[[metric]]$strat_decision_user %||% NULL
  if (!is.null(cached) && nrow(cached) > 0) {
    return(cached)
  }

  build_metric_strat_decision(rv, metric, get_metric_phase3_selected(rv, metric))
}

build_metric_phase4_signature <- function(rv, metric, decision_tbl = get_metric_phase4_decision_state(rv, metric)) {
  if (is.null(metric) || identical(metric, "")) {
    return(NULL)
  }

  decision_tbl <- decision_tbl %||% tibble::tibble()
  if (nrow(decision_tbl) == 0) {
    decision_tbl <- build_metric_strat_decision(rv, metric, get_metric_phase3_selected(rv, metric))
  }

  list(
    data_fingerprint = rv$data_fingerprint %||% NULL,
    config_version = rv$config_version %||% 0L,
    decision_type = decision_tbl$decision_type[1] %||% "none",
    selected_strat = decision_tbl$selected_strat[1] %||% NA_character_
  )
}

get_metric_phase4_signature <- function(rv, metric) {
  cached <- rv$metric_phase_cache[[metric]]$phase4_signature %||% NULL
  completed <- rv$completed_metrics[[metric]]$phase4_signature %||% NULL
  cached %||% completed
}

phase4_signature_matches <- function(lhs, rhs) {
  identical(lhs %||% NULL, rhs %||% NULL)
}

get_metric_phase4_cached_result <- function(rv, metric) {
  cache_entry <- rv$metric_phase_cache[[metric]] %||% list()
  completed_entry <- rv$completed_metrics[[metric]] %||% list()

  stratum_results <- cache_entry$stratum_results %||% completed_entry$stratum_results %||% NULL
  reference_curve <- cache_entry$reference_curve %||% completed_entry$reference_curve %||% NULL

  list(
    signature = cache_entry$phase4_signature %||% completed_entry$phase4_signature %||% NULL,
    reference_curve = reference_curve,
    stratum_results = stratum_results
  )
}

metric_has_any_phase4_results <- function(rv, metric) {
  cached <- get_metric_phase4_cached_result(rv, metric)
  !is.null(cached$reference_curve) || !is.null(cached$stratum_results)
}

metric_has_phase4_cache <- function(rv, metric, decision_tbl = get_metric_phase4_decision_state(rv, metric)) {
  cached <- get_metric_phase4_cached_result(rv, metric)
  expected_signature <- build_metric_phase4_signature(rv, metric, decision_tbl)

  if (is.null(cached$signature) || !phase4_signature_matches(cached$signature, expected_signature)) {
    return(FALSE)
  }

  if (!is.null(cached$stratum_results)) {
    return(
      length(cached$stratum_results) > 0 &&
        all(vapply(cached$stratum_results, function(entry) {
          !is.null(entry$reference_curve) && !is.null(entry$reference_curve$curve_row)
        }, logical(1)))
    )
  }

  !is.null(cached$reference_curve) && !is.null(cached$reference_curve$curve_row)
}

cache_metric_phase4_results <- function(rv, metric,
                                        decision_tbl = get_metric_phase4_decision_state(rv, metric),
                                        reference_curve = NULL,
                                        stratum_results = NULL) {
  ensure_metric_phase_cache(rv, metric)

  signature <- build_metric_phase4_signature(rv, metric, decision_tbl)

  rv$metric_phase_cache[[metric]]$strat_decision_user <- decision_tbl
  rv$metric_phase_cache[[metric]]$reference_curve <- reference_curve
  rv$metric_phase_cache[[metric]]$current_stratum_level <- NULL
  rv$metric_phase_cache[[metric]]$phase4_data <- rv$data
  rv$metric_phase_cache[[metric]]$stratum_results <- stratum_results
  rv$metric_phase_cache[[metric]]$phase4_signature <- signature

  rv$stratum_results[[metric]] <- stratum_results

  if (identical(rv$current_metric, metric)) {
    rv$reference_curve <- if (is.null(stratum_results)) reference_curve else NULL
    rv$current_stratum_level <- NULL
    rv$phase4_data <- rv$data
  }

  invisible(signature)
}

set_modal_progress_detail <- function(progress, detail) {
  if (!is.null(progress) && is.function(progress$set_detail)) {
    progress$set_detail(detail)
  }
  invisible(NULL)
}

advance_modal_progress <- function(progress, detail = NULL) {
  if (!is.null(progress) && is.function(progress$advance)) {
    progress$advance(detail)
  }
  invisible(NULL)
}

count_metric_phase4_preload_steps <- function(rv, metric) {
  decision_tbl <- get_metric_phase4_decision_state(rv, metric)
  if (metric_has_phase4_cache(rv, metric, decision_tbl)) {
    return(0L)
  }

  if (!is.null(decision_tbl) &&
      nrow(decision_tbl) > 0 &&
      identical(decision_tbl$decision_type[1], "single") &&
      !is.na(decision_tbl$selected_strat[1])) {
    strat_values <- get_stratification_values(rv$data, decision_tbl$selected_strat[1], rv$strat_config)
    levels <- sort(unique(stats::na.omit(strat_values)))
    return(as.integer(max(length(levels), 1L) + 1L))
  }

  2L
}

preload_metric_phase4_workspace <- function(rv, metric, progress = NULL) {
  decision_tbl <- get_metric_phase4_decision_state(rv, metric)
  sync_metric_decision_state(rv, metric, decision_tbl)
  rv$phase4_data <- rv$data
  rv$current_stratum_level <- NULL

  if (metric_has_phase4_cache(rv, metric, decision_tbl)) {
    cached <- get_metric_phase4_cached_result(rv, metric)
    if (!is.null(cached$reference_curve)) {
      rv$reference_curve <- cached$reference_curve
    } else {
      rv$reference_curve <- NULL
    }
    return(invisible(FALSE))
  }

  if (!is.null(decision_tbl) &&
      nrow(decision_tbl) > 0 &&
      identical(decision_tbl$decision_type[1], "single") &&
      !is.na(decision_tbl$selected_strat[1])) {
    strat_key <- decision_tbl$selected_strat[1]
    strat_values <- get_stratification_values(rv$data, strat_key, rv$strat_config)
    levels <- sort(unique(stats::na.omit(strat_values)))
    stratum_results <- list()

    if (length(levels) == 0) {
      set_modal_progress_detail(progress, "No strata available for curve generation.")
      advance_modal_progress(progress, "No strata available for curve generation.")
      cache_metric_phase4_results(rv, metric, decision_tbl = decision_tbl, stratum_results = list())
      advance_modal_progress(progress, "Prepared Phase 4 workspace.")
      return(invisible(TRUE))
    }

    for (i in seq_along(levels)) {
      lvl <- levels[[i]]
      set_modal_progress_detail(progress, paste0("Building curve for ", lvl, "..."))
      stratum_data <- rv$data[strat_values == lvl, , drop = FALSE]
      stratum_results[[lvl]] <- list(
        reference_curve = build_reference_curve(
          stratum_data,
          metric,
          rv$metric_config,
          stratum_label = lvl
        )
      )
      advance_modal_progress(
        progress,
        paste0("Built ", lvl, " curve (", i, "/", length(levels), ").")
      )
    }

    set_modal_progress_detail(progress, "Caching stratified Phase 4 results...")
    cache_metric_phase4_results(
      rv,
      metric,
      decision_tbl = decision_tbl,
      stratum_results = stratum_results
    )
    advance_modal_progress(progress, "Prepared Phase 4 workspace.")
    return(invisible(TRUE))
  }

  set_modal_progress_detail(progress, "Building reference curve...")
  curve_result <- build_reference_curve(rv$data, metric, rv$metric_config)
  advance_modal_progress(progress, "Built reference curve.")

  set_modal_progress_detail(progress, "Caching Phase 4 results...")
  cache_metric_phase4_results(
    rv,
    metric,
    decision_tbl = decision_tbl,
    reference_curve = curve_result
  )
  advance_modal_progress(progress, "Prepared Phase 4 workspace.")
  invisible(TRUE)
}

get_metric_phase1_artifact_mode <- function(rv, metric) {
  rv$metric_phase_cache[[metric]]$phase1_artifact_mode %||% "full"
}

get_metric_phase3_artifact_mode <- function(rv, metric) {
  rv$metric_phase_cache[[metric]]$phase3_artifact_mode %||% "full"
}

metric_needs_phase1_artifact_refresh <- function(rv, metric) {
  screening <- if (identical(rv$current_metric, metric) && !is.null(rv$phase1_screening)) {
    rv$phase1_screening
  } else {
    rv$metric_phase_cache[[metric]]$phase1_screening %||% NULL
  }

  !is.null(screening) && identical(get_metric_phase1_artifact_mode(rv, metric), "summary")
}

metric_needs_phase3_artifact_refresh <- function(rv, metric) {
  !is.null(rv$phase3_verification[[metric]]) &&
    identical(get_metric_phase3_artifact_mode(rv, metric), "summary")
}

get_metric_phase1_display_state <- function(rv, metric) {
  screening <- if (identical(rv$current_metric, metric) && !is.null(rv$phase1_screening)) {
    rv$phase1_screening
  } else {
    rv$metric_phase_cache[[metric]]$phase1_screening %||% NULL
  }

  effect_sizes <- if (identical(rv$current_metric, metric) && !is.null(rv$phase1_effect_sizes)) {
    rv$phase1_effect_sizes
  } else {
    rv$metric_phase_cache[[metric]]$phase1_effect_sizes %||% NULL
  }

  if (is.null(screening)) return(NULL)

  list(
    results = screening$results %||% tibble::tibble(),
    pairwise = screening$pairwise %||% tibble::tibble(),
    plots = screening$plots %||% list(),
    effect_sizes = effect_sizes %||% tibble::tibble()
  )
}

count_metric_phase1_backfill_steps <- function(rv, metric, mode = c("full", "summary")) {
  mode <- match.arg(mode)
  allowed <- get_metric_allowed_strats(rv, metric)
  as.integer(length(allowed) + 1L)
}

build_metric_phase1_backfill <- function(rv, metric, mode = c("full", "summary"), progress = NULL) {
  mode <- match.arg(mode)
  allowed <- get_metric_allowed_strats(rv, metric)
  existing_candidates <- rv$phase1_candidates[[metric]]
  compute_pairwise <- identical(mode, "full")
  build_plots <- identical(mode, "full")

  results_list <- list()
  for (i in seq_along(allowed)) {
    sk <- allowed[[i]]
    set_modal_progress_detail(
      progress,
      paste0("Running screening for ", get_strat_display_name(rv, sk), "...")
    )

    res <- tryCatch(
      screen_stratification(
        rv$data, metric, sk,
        rv$metric_config, rv$strat_config,
        compute_pairwise = compute_pairwise,
        build_plot = build_plots
      ),
      error = function(e) NULL
    )

    if (!is.null(res)) {
      results_list[[sk]] <- res
    }

    advance_modal_progress(
      progress,
      paste0("Screened ", get_strat_display_name(rv, sk), " (", i, "/", length(allowed), ").")
    )
  }

  results_list <- Filter(Negate(is.null), results_list)

  result_rows <- dplyr::bind_rows(lapply(results_list, `[[`, "result_row"))
  pairwise_rows <- if (isTRUE(compute_pairwise)) {
    dplyr::bind_rows(
      purrr::keep(lapply(results_list, `[[`, "pairwise_df"), ~ !is.null(.) && nrow(.) > 0)
    )
  } else {
    tibble::tibble()
  }
  plots <- if (isTRUE(build_plots)) {
    plot_list <- setNames(lapply(results_list, `[[`, "plot"), names(results_list))
    Filter(Negate(is.null), plot_list)
  } else {
    list()
  }

  tested_strats <- unique(result_rows$stratification %||% character(0))
  set_modal_progress_detail(progress, "Computing effect sizes and candidate defaults...")
  effect_sizes <- if (length(tested_strats) > 0) {
    tryCatch(
      compute_effect_sizes(rv$data, metric, tested_strats, rv$metric_config, rv$strat_config),
      error = function(e) tibble::tibble()
    )
  } else {
    tibble::tibble()
  }

  candidates <- build_metric_phase1_candidate_table_from_sources(
    metric = metric,
    allowed = allowed,
    existing = existing_candidates,
    l1 = result_rows,
    l2 = effect_sizes,
    include_all_allowed = TRUE
  )

  advance_modal_progress(progress, "Prepared Phase 1 screening details.")

  list(
    screening = list(
      results = result_rows,
      pairwise = pairwise_rows,
      plots = plots
    ),
    effect_sizes = effect_sizes,
    candidates = candidates,
    artifact_mode = mode
  )
}

commit_metric_phase1_backfill <- function(rv, metric, backfill) {
  ensure_metric_phase_cache(rv, metric)

  if (nrow(backfill$screening$results) > 0) {
    rv$all_layer1_results[[metric]] <- backfill$screening$results
  } else {
    rv$all_layer1_results[[metric]] <- NULL
  }

  if (!is.null(backfill$effect_sizes) && nrow(backfill$effect_sizes) > 0) {
    rv$all_layer2_results[[metric]] <- backfill$effect_sizes
  } else {
    rv$all_layer2_results[[metric]] <- NULL
  }

  rv$phase1_candidates[[metric]] <- backfill$candidates
  rv$metric_phase_cache[[metric]]$phase1_screening <- backfill$screening
  rv$metric_phase_cache[[metric]]$phase1_effect_sizes <- backfill$effect_sizes
  rv$metric_phase_cache[[metric]]$phase1_artifact_mode <- backfill$artifact_mode %||% "full"

  if (identical(rv$current_metric, metric)) {
    rv$phase1_screening <- backfill$screening
    rv$phase1_effect_sizes <- backfill$effect_sizes
  }

  invisible(backfill)
}

ensure_metric_phase1_artifacts <- function(rv, metric, progress = NULL) {
  if (!metric_needs_phase1_artifact_refresh(rv, metric)) {
    return(invisible(FALSE))
  }

  phase1_backfill <- build_metric_phase1_backfill(rv, metric, mode = "full", progress = progress)
  commit_metric_phase1_backfill(rv, metric, phase1_backfill)
  invisible(TRUE)
}

get_metric_phase3_display_state <- function(rv, metric) {
  verified <- rv$phase3_verification[[metric]]
  if (is.null(verified)) return(NULL)

  patterns <- verified$pattern_results %||%
    rv$metric_phase_cache[[metric]]$phase3_patterns %||%
    if (identical(rv$current_metric, metric)) rv$phase3_patterns else NULL

  feasibility <- verified$feasibility_results %||%
    rv$metric_phase_cache[[metric]]$phase3_feasibility %||%
    if (identical(rv$current_metric, metric)) rv$phase3_feasibility else NULL

  strats <- unique(c(
    verified$finalists %||% character(0),
    names(verified$verification_status %||% list())
  ))
  current_finalists <- get_metric_phase3_choices(rv, metric)
  if (length(current_finalists) > 0) {
    strats <- intersect(current_finalists, if (length(strats) > 0) strats else current_finalists)
    if (length(strats) == 0) {
      strats <- current_finalists
    }
  }

  if (length(strats) == 0 && is.null(patterns) && is.null(feasibility)) {
    return(NULL)
  }

  list(
    strats = strats,
    patterns = patterns %||% list(results = tibble::tibble(), plots = list()),
    feasibility = feasibility %||% tibble::tibble()
  )
}

count_metric_phase3_backfill_steps <- function(rv, metric, mode = c("full", "summary")) {
  mode <- match.arg(mode)
  finalists <- get_metric_phase3_choices(rv, metric)

  if (!identical(mode, "full")) {
    return(1L)
  }

  if (length(finalists) == 0) {
    return(1L)
  }

  as.integer(length(finalists) + 2L)
}

build_metric_phase3_backfill <- function(rv, metric, mode = c("full", "summary"), progress = NULL) {
  mode <- match.arg(mode)
  finalists <- get_metric_phase3_choices(rv, metric)
  existing <- rv$phase3_verification[[metric]]
  selected <- get_metric_phase3_selected(rv, metric)
  normalized_selected <- if (selected %in% finalists) selected else "none"

  verification_status <- existing$verification_status %||% list()
  if (length(finalists) > 0) {
    verification_status <- stats::setNames(
      vapply(finalists, function(sk) verification_status[[sk]] %||% "verified", character(1)),
      finalists
    )
  } else {
    verification_status <- list()
  }

  build_full_artifacts <- identical(mode, "full")
  pattern_results <- if (isTRUE(build_full_artifacts)) {
    list(results = tibble::tibble(), plots = list())
  } else {
    NULL
  }
  feasibility_results <- if (isTRUE(build_full_artifacts)) {
    tibble::tibble()
  } else {
    NULL
  }

  if (length(finalists) > 0 && isTRUE(build_full_artifacts)) {
    predictor_keys <- rv$metric_config[[metric]]$allowed_predictors %||% character(0)
    all_pattern_results <- list()
    all_pattern_plots <- list()

    pattern_targets <- c("none", finalists)
    for (i in seq_along(pattern_targets)) {
      sk <- pattern_targets[[i]]
      sk_actual <- if (sk == "none") NULL else sk
      target_label <- if (sk == "none") "(Unstratified baseline)" else get_strat_display_name(rv, sk)
      set_modal_progress_detail(progress, paste0("Assessing pattern stability for ", target_label, "..."))
      res <- tryCatch(
        assess_pattern_stability(
          rv$data,
          metric,
          sk_actual,
          predictor_keys,
          rv$metric_config,
          rv$strat_config,
          rv$predictor_config,
          build_plots = TRUE
        ),
        error = function(e) list(results = tibble::tibble(), plots = list())
      )

      if (!is.null(res$results) && nrow(res$results) > 0) {
        all_pattern_results <- c(all_pattern_results, list(res$results))
      }
      all_pattern_plots <- c(all_pattern_plots, res$plots %||% list())

      advance_modal_progress(
        progress,
        paste0("Checked pattern stability for ", target_label, " (", i, "/", length(pattern_targets), ").")
      )
    }

    pattern_results <- list(
      results = dplyr::bind_rows(all_pattern_results),
      plots = all_pattern_plots
    )
    set_modal_progress_detail(progress, "Assessing feasibility...")
    feasibility_results <- tryCatch(
      assess_feasibility(rv$data, finalists, rv$strat_config),
      error = function(e) tibble::tibble()
    )
    advance_modal_progress(progress, "Prepared Phase 3 verification details.")
  } else {
    advance_modal_progress(progress, "Prepared Phase 3 verification details.")
  }

  decision_tbl <- build_metric_strat_decision(rv, metric, normalized_selected)
  verification <- list(
    finalists = finalists,
    pattern_results = pattern_results,
    feasibility_results = feasibility_results,
    verification_status = verification_status,
    selected_strat = normalized_selected,
    justification = existing$justification %||% "Updated from Summary page"
  )

  list(
    verification = verification,
    phase3_patterns = pattern_results,
    phase3_feasibility = feasibility_results,
    decision_tbl = decision_tbl,
    artifact_mode = mode
  )
}

commit_metric_phase3_backfill <- function(rv, metric, backfill) {
  ensure_metric_phase_cache(rv, metric)

  rv$phase3_verification[[metric]] <- backfill$verification
  rv$metric_phase_cache[[metric]]$phase3_patterns <- backfill$phase3_patterns
  rv$metric_phase_cache[[metric]]$phase3_feasibility <- backfill$phase3_feasibility
  rv$metric_phase_cache[[metric]]$phase3_artifact_mode <- backfill$artifact_mode %||% "full"
  sync_metric_decision_state(rv, metric, backfill$decision_tbl)

  if (identical(rv$current_metric, metric)) {
    rv$phase3_patterns <- backfill$phase3_patterns
    rv$phase3_feasibility <- backfill$phase3_feasibility
  }

  invisible(backfill)
}

ensure_metric_phase3_artifacts <- function(rv, metric, progress = NULL) {
  if (!metric_needs_phase3_artifact_refresh(rv, metric)) {
    return(invisible(FALSE))
  }

  phase3_backfill <- build_metric_phase3_backfill(rv, metric, mode = "full", progress = progress)
  commit_metric_phase3_backfill(rv, metric, phase3_backfill)
  invisible(TRUE)
}

ensure_metric_phase3_valid <- function(rv, metric) {
  current_choice <- get_metric_phase3_selected(rv, metric)
  valid_choices <- get_metric_phase3_choices(rv, metric)
  if (identical(current_choice, "none") || current_choice %in% valid_choices) {
    return(invisible(FALSE))
  }

  set_metric_phase3_selected(rv, metric, "none")
  clear_metric_phase4_results(rv, metric)
  invisible(TRUE)
}

set_metric_available_strats <- function(rv, metric, selected) {
  current_allowed <- get_metric_allowed_strats(rv, metric)
  base_allowed <- get_metric_config_allowed_strats(rv, metric)
  all_choices <- get_summary_available_choices(rv)
  new_allowed <- all_choices[all_choices %in% unique(selected %||% character(0))]

  old_p1_selected <- get_metric_phase1_selected(rv, metric)
  old_p2_selected <- get_metric_phase2_passed(rv, metric)
  old_phase3 <- get_metric_phase3_selected(rv, metric)

  if (identical(sort(new_allowed), sort(base_allowed))) {
    rv$summary_available_overrides[[metric]] <- NULL
  } else {
    rv$summary_available_overrides[[metric]] <- new_allowed
  }

  removed <- setdiff(current_allowed, new_allowed)
  invalidated <- ensure_metric_phase3_valid(rv, metric)

  phase1_notes <- if (length(intersect(old_p1_selected, removed)) > 0) {
    list(list(
      level = "warning",
      text = paste0(
        "Summary removed unavailable Phase 1 selections: ",
        format_strat_list(rv, intersect(old_p1_selected, removed))
      )
    ))
  } else {
    list()
  }

  phase2_notes <- if (length(intersect(old_p2_selected, removed)) > 0) {
    list(list(
      level = "warning",
      text = paste0(
        "Summary removed unavailable Phase 2 selections: ",
        format_strat_list(rv, intersect(old_p2_selected, removed))
      )
    ))
  } else {
    list()
  }

  phase3_notes <- if (isTRUE(invalidated) && !identical(old_phase3, "none")) {
    list(list(
      level = "warning",
      text = paste0(
        "Phase 3 selection was cleared because ",
        get_strat_display_name(rv, old_phase3),
        " is no longer available."
      )
    ))
  } else {
    list()
  }

  phase4_notes <- if (isTRUE(invalidated)) {
    list(list(
      level = "warning",
      text = "Phase 4 outputs were cleared because the current Phase 3 selection became invalid."
    ))
  } else {
    list()
  }

  set_metric_summary_edit_notes(rv, metric, "Phase 1", phase1_notes)
  set_metric_summary_edit_notes(rv, metric, "Phase 2", phase2_notes)
  set_metric_summary_edit_notes(rv, metric, "Phase 3", phase3_notes)
  set_metric_summary_edit_notes(rv, metric, "Phase 4", phase4_notes)
  invisible(new_allowed)
}

get_stratification_values <- function(data, strat_key, strat_config) {
  sc <- strat_config[[strat_key]]
  if (is.null(sc)) return(rep(NA_character_, nrow(data)))

  if (!is.null(sc$column_name) && !is.na(sc$column_name) && sc$column_name %in% names(data)) {
    return(as.character(data[[sc$column_name]]))
  }

  if (identical(sc$type, "paired")) {
    primary <- sc$primary %||% NA_character_
    secondary <- sc$secondary %||% NA_character_
    if (!(primary %in% names(data)) || !(secondary %in% names(data))) {
      return(rep(NA_character_, nrow(data)))
    }

    primary_vals <- as.character(data[[primary]])
    secondary_vals <- as.character(data[[secondary]])
    out <- ifelse(
      is.na(primary_vals) | is.na(secondary_vals),
      NA_character_,
      paste(primary_vals, secondary_vals, sep = " | ")
    )
    return(out)
  }

  rep(NA_character_, nrow(data))
}

get_metric_curve_rows <- function(rv, metric) {
  entry <- rv$completed_metrics[[metric]]
  if (is.null(entry) || identical(entry$type, "regional")) {
    return(tibble::tibble())
  }

  if (!is.null(entry$stratum_results)) {
    rows <- purrr::map_dfr(entry$stratum_results, function(x) {
      if (!is.null(x$reference_curve) && !is.null(x$reference_curve$curve_row)) {
        x$reference_curve$curve_row
      } else {
        NULL
      }
    })
    return(rows)
  }

  if (!is.null(entry$reference_curve) && !is.null(entry$reference_curve$curve_row)) {
    return(entry$reference_curve$curve_row)
  }

  tibble::tibble()
}

metric_has_official_curve <- function(rv, metric) {
  nrow(get_metric_curve_rows(rv, metric)) > 0
}

recompute_metric_phase4 <- function(rv, metric) {
  selection <- get_metric_phase3_selected(rv, metric)
  decision_tbl <- build_metric_strat_decision(rv, metric, selection)
  sync_metric_decision_state(rv, metric, decision_tbl)

  if (decision_tbl$decision_type[1] == "single" && !is.na(decision_tbl$selected_strat[1])) {
    strat_key <- decision_tbl$selected_strat[1]
    strat_values <- get_stratification_values(rv$data, strat_key, rv$strat_config)
    valid_levels <- sort(unique(stats::na.omit(strat_values)))

    stratum_results <- list()
    for (lvl in valid_levels) {
      stratum_data <- rv$data[strat_values == lvl, , drop = FALSE]
      stratum_results[[lvl]] <- list(
        reference_curve = build_reference_curve(
          stratum_data,
          metric,
          rv$metric_config,
          stratum_label = lvl
        )
      )
    }

    clear_metric_phase4_results(rv, metric)
    phase4_signature <- cache_metric_phase4_results(
      rv,
      metric,
      decision_tbl = decision_tbl,
      stratum_results = stratum_results
    )
    rv$completed_metrics[[metric]] <- list(
      stratified = TRUE,
      strat_var = strat_key,
      strat_decision = decision_tbl,
      stratum_results = stratum_results,
      phase4_signature = phase4_signature
    )

    return(invisible(rv$completed_metrics[[metric]]))
  }

  curve_result <- build_reference_curve(rv$data, metric, rv$metric_config)
  clear_metric_phase4_results(rv, metric)
  phase4_signature <- cache_metric_phase4_results(
    rv,
    metric,
    decision_tbl = decision_tbl,
    reference_curve = curve_result
  )
  rv$completed_metrics[[metric]] <- list(
    strat_decision = decision_tbl,
    reference_curve = curve_result,
    phase4_signature = phase4_signature
  )

  invisible(rv$completed_metrics[[metric]])
}

recompute_metric_from_summary <- function(rv, metric, refresh_phase2 = TRUE,
                                          mode = c("summary", "full"),
                                          progress_cb = NULL) {
  mode <- match.arg(mode)

  if (is.function(progress_cb)) {
    progress_cb("phase1", metric, 1L, 1L, "start")
  }
  phase1_backfill <- build_metric_phase1_backfill(rv, metric, mode = mode)
  commit_metric_phase1_backfill(rv, metric, phase1_backfill)
  if (is.function(progress_cb)) {
    progress_cb("phase1", metric, 1L, 1L, "end")
  }

  if (isTRUE(refresh_phase2)) {
    if (is.function(progress_cb)) {
      progress_cb("phase2", NULL, 1L, 1L, "start")
    }
    recompute_phase2_shared(rv)
    for (mk in eligible_summary_metrics(rv$metric_config)) {
      ensure_metric_phase3_valid(rv, mk)
    }
    if (is.function(progress_cb)) {
      progress_cb("phase2", NULL, 1L, 1L, "end")
    }
  }

  if (is.function(progress_cb)) {
    progress_cb("phase3", metric, 1L, 1L, "start")
  }
  phase3_backfill <- build_metric_phase3_backfill(rv, metric, mode = mode)
  commit_metric_phase3_backfill(rv, metric, phase3_backfill)
  if (is.function(progress_cb)) {
    progress_cb("phase3", metric, 1L, 1L, "end")
  }

  if (is.function(progress_cb)) {
    progress_cb("phase4", metric, 1L, 1L, "start")
  }
  recompute_metric_phase4(rv, metric)
  if (is.function(progress_cb)) {
    progress_cb("phase4", metric, 1L, 1L, "end")
  }

  invisible(list(
    phase1 = phase1_backfill,
    phase3 = phase3_backfill,
    phase4 = rv$completed_metrics[[metric]]
  ))
}

recompute_metrics_from_summary <- function(rv, metrics,
                                           mode = c("summary", "full"),
                                           progress_cb = NULL,
                                           on_metric_done = NULL) {
  mode <- match.arg(mode)
  metrics <- intersect(metrics, eligible_summary_metrics(rv$metric_config))
  if (length(metrics) == 0) return(invisible(NULL))

  for (i in seq_along(metrics)) {
    metric <- metrics[[i]]
    if (is.function(progress_cb)) {
      progress_cb("phase1", metric, i, length(metrics), "start")
    }
    phase1_backfill <- build_metric_phase1_backfill(rv, metric, mode = mode)
    commit_metric_phase1_backfill(rv, metric, phase1_backfill)
    if (is.function(progress_cb)) {
      progress_cb("phase1", metric, i, length(metrics), "end")
    }
  }

  if (is.function(progress_cb)) {
    progress_cb("phase2", NULL, 1L, 1L, "start")
  }
  recompute_phase2_shared(rv)
  for (mk in eligible_summary_metrics(rv$metric_config)) {
    ensure_metric_phase3_valid(rv, mk)
  }
  if (is.function(progress_cb)) {
    progress_cb("phase2", NULL, 1L, 1L, "end")
  }

  for (i in seq_along(metrics)) {
    metric <- metrics[[i]]
    if (is.function(progress_cb)) {
      progress_cb("phase3", metric, i, length(metrics), "start")
    }
    phase3_backfill <- build_metric_phase3_backfill(rv, metric, mode = mode)
    commit_metric_phase3_backfill(rv, metric, phase3_backfill)
    if (is.function(progress_cb)) {
      progress_cb("phase3", metric, i, length(metrics), "end")
    }
  }

  for (i in seq_along(metrics)) {
    metric <- metrics[[i]]
    if (is.function(progress_cb)) {
      progress_cb("phase4", metric, i, length(metrics), "start")
    }
    recompute_metric_phase4(rv, metric)
    if (is.function(on_metric_done)) {
      on_metric_done(metric)
    }
    if (is.function(progress_cb)) {
      progress_cb("phase4", metric, i, length(metrics), "end")
    }
  }

  invisible(NULL)
}

format_summary_number <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("N/A")
  format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE)
}

format_summary_range <- function(min_val, max_val, digits = 2) {
  if (any(is.na(c(min_val, max_val)))) return("N/A")
  paste0(format_summary_number(min_val, digits), " - ", format_summary_number(max_val, digits))
}

metric_summary_cell_value <- function(curve_rows, field, digits = 2) {
  if (nrow(curve_rows) == 0) return("Not computed")
  if (nrow(curve_rows) > 1) return(paste0(nrow(curve_rows), " strata"))
  format_summary_number(curve_rows[[field]][1], digits)
}

metric_summary_range_value <- function(curve_rows, min_field, max_field, digits = 2) {
  if (nrow(curve_rows) == 0) return("Not computed")
  if (nrow(curve_rows) > 1) return(paste0(nrow(curve_rows), " strata"))
  format_summary_range(curve_rows[[min_field]][1], curve_rows[[max_field]][1], digits)
}

build_metric_notes <- function(rv, metric) {
  notes <- empty_summary_note_store()

  add_note <- function(phase, level, text) {
    notes[[phase]] <<- c(notes[[phase]], list(list(level = level, text = text)))
  }

  edit_notes <- get_metric_summary_edit_notes(rv, metric)
  for (phase in names(notes)) {
    notes[[phase]] <- c(notes[[phase]], edit_notes[[phase]])
  }

  allowed <- get_metric_allowed_strats(rv, metric)
  p1_tbl <- get_metric_phase1_candidate_table(rv, metric)
  p1_selected <- get_metric_phase1_selected(rv, metric)
  p1_dropped <- setdiff(intersect(allowed, p1_tbl$stratification), p1_selected)
  p1_screening <- rv$all_layer1_results[[metric]]

  if (is.null(p1_screening) || nrow(p1_screening) == 0) {
    add_note("Phase 1", "warning", "Phase 1 screening has not been saved.")
  } else {
    add_note("Phase 1", "info", paste0("Candidates carried forward: ", format_strat_list(rv, p1_selected)))
    if (length(p1_dropped) > 0) {
      add_note("Phase 1", "warning", paste0("Dropped after Phase 1: ", format_strat_list(rv, p1_dropped)))
    }
  }

  p2_selected <- get_metric_phase2_passed(rv, metric)
  p2_dropped <- setdiff(p1_selected, p2_selected)
  add_note("Phase 2", "info", paste0("Phase 2 passed: ", format_strat_list(rv, p2_selected)))
  if (length(p2_dropped) > 0) {
    add_note("Phase 2", "warning", paste0("Dropped after Phase 2: ", format_strat_list(rv, p2_dropped)))
  }

  phase3_selected <- get_metric_phase3_selected(rv, metric)
  if (identical(phase3_selected, "none")) {
    add_note("Phase 3", "info", "No Phase 3 stratification selected; Phase 4 uses an unstratified curve.")
  } else {
    add_note("Phase 3", "info", paste0("Phase 3 selected: ", get_strat_display_name(rv, phase3_selected)))
  }

  decision_tbl <- if (!is.null(rv$completed_metrics[[metric]]$strat_decision)) {
    rv$completed_metrics[[metric]]$strat_decision
  } else if (!is.null(rv$metric_phase_cache[[metric]]$strat_decision_user)) {
    rv$metric_phase_cache[[metric]]$strat_decision_user
  } else if (identical(rv$current_metric, metric)) {
    rv$strat_decision_user
  } else {
    NULL
  }

  if (!is.null(decision_tbl) && nrow(decision_tbl) > 0 && isTRUE(decision_tbl$needs_review[1])) {
    add_note("Phase 3", "warning", decision_tbl$review_reason[1] %||% "Phase 3 selection needs review.")
  }

  curve_rows <- get_metric_curve_rows(rv, metric)
  if (nrow(curve_rows) == 0) {
    add_note("Phase 4", "warning", "Phase 4 outputs are not current. Recompute is required.")
  } else {
    add_note("Phase 4", "info", paste0("Phase 4 outputs available for ", nrow(curve_rows), " curve(s)."))

    curve_flags <- unique(curve_rows$curve_status[curve_rows$curve_status != "complete"])
    if (length(curve_flags) > 0) {
      add_note("Phase 4", "warning",
               paste0("Phase 4 curve issues: ", paste(curve_flags, collapse = ", ")))
    }

    decision_selected <- if (!is.null(decision_tbl) && nrow(decision_tbl) > 0 &&
                             decision_tbl$decision_type[1] == "single") {
      decision_tbl$selected_strat[1]
    } else {
      NA_character_
    }

    if (!is.na(decision_selected)) {
      strat_values <- get_stratification_values(rv$data, decision_selected, rv$strat_config)
      min_n <- rv$metric_config[[metric]]$min_sample_size %||% 10
      sparse <- purrr::keep(sort(unique(stats::na.omit(strat_values))), function(lvl) {
        sum(strat_values == lvl, na.rm = TRUE) < min_n
      })
      if (length(sparse) > 0) {
        sparse_text <- paste(vapply(sparse, function(lvl) {
          paste0(lvl, " (n=", sum(strat_values == lvl, na.rm = TRUE), ")")
        }, character(1)), collapse = ", ")
        add_note("Phase 4", "warning",
                 paste0("Strata below minimum n=", min_n, ": ", sparse_text))
      }
    }
  }

  notes
}

metric_summary_status <- function(rv, metric) {
  notes <- build_metric_notes(rv, metric)
  has_warning <- any(unlist(lapply(notes, function(items) {
    vapply(items, function(item) identical(item$level, "warning"), logical(1))
  })))
  curve_rows <- get_metric_curve_rows(rv, metric)

  if (!metric_has_official_curve(rv, metric)) {
    return(list(
      label = "Incomplete",
      badge = "fail",
      summary_label = "Not Run",
      summary_class = "summary-status-not-run",
      notes = notes
    ))
  }

  has_failed_curve <- nrow(curve_rows) > 0 && any(curve_rows$curve_status != "complete")
  if (isTRUE(has_failed_curve)) {
    return(list(
      label = "Failed",
      badge = "fail",
      summary_label = "Failed",
      summary_class = "summary-status-failed",
      notes = notes
    ))
  }

  if (has_warning) {
    return(list(
      label = "Complete with Warnings",
      badge = "caution",
      summary_label = "Run (warnings)",
      summary_class = "summary-status-run-warnings",
      notes = notes
    ))
  }

  list(
    label = "Complete",
    badge = "pass",
    summary_label = "Run",
    summary_class = "summary-status-run",
    notes = notes
  )
}

build_metric_summary_snapshot <- function(rv, metric) {
  mc <- rv$metric_config[[metric]]
  precheck <- get_metric_precheck_row(rv, metric)
  allowed <- get_metric_allowed_strats(rv, metric)
  available_choices <- get_summary_available_choices(rv)
  phase3_choices <- get_metric_phase3_choices(rv, metric)
  phase3_selected <- get_metric_phase3_selected(rv, metric)
  if (!(phase3_selected %in% c("none", phase3_choices))) {
    phase3_selected <- "none"
  }

  status <- metric_summary_status(rv, metric)
  curve_rows <- get_metric_curve_rows(rv, metric)
  strat_label_map <- stats::setNames(vapply(available_choices, function(sk) {
    get_strat_display_name(rv, sk)
  }, character(1)), available_choices)

  list(
    metric = metric,
    display_name = mc$display_name %||% metric,
    family = mc$metric_family %||% "N/A",
    units = mc$units %||% "N/A",
    direction = metric_direction_label(rv$metric_config, metric),
    n_obs = get_first_value(precheck, "n_obs", "N/A"),
    status = status,
    notes = status$notes,
    available_selected = allowed,
    available_choices = available_choices,
    phase1_selected = get_metric_phase1_selected(rv, metric),
    phase2_selected = get_metric_phase2_passed(rv, metric),
    phase3_choices = phase3_choices,
    phase3_selected = phase3_selected,
    curve_rows = curve_rows,
    completed = rv$completed_metrics[[metric]],
    strat_label_map = strat_label_map
  )
}

build_stratified_stats_table <- function(curve_rows) {
  stats <- c("n", "Min", "Q25", "Median", "Q75", "Max", "IQR", "SD")
  tbl <- data.frame(Statistic = stats, stringsAsFactors = FALSE, check.names = FALSE)

  for (i in seq_len(nrow(curve_rows))) {
    row <- curve_rows[i, ]
    lbl <- row$stratum %||% paste0("Curve ", i)
    if (row$curve_status == "insufficient_data") {
      tbl[[lbl]] <- rep("N/A", length(stats))
    } else {
      tbl[[lbl]] <- c(
        as.character(row$n_reference),
        format_summary_number(row$min_val),
        format_summary_number(row$q25),
        format_summary_number(row$median_val),
        format_summary_number(row$q75),
        format_summary_number(row$max_val),
        format_summary_number(row$iqr),
        format_summary_number(row$sd_val)
      )
    }
  }
  tbl
}

build_stratified_threshold_table <- function(curve_rows) {
  tbl <- data.frame(
    Category = c("Functioning", "Functioning-At-Risk", "Non-functioning"),
    `Score Range` = c("0.70 - 1.00", "0.30 - 0.69", "0.00 - 0.29"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (i in seq_len(nrow(curve_rows))) {
    row <- curve_rows[i, ]
    lbl <- row$stratum %||% paste0("Curve ", i)
    if (row$curve_status == "insufficient_data") {
      tbl[[lbl]] <- rep("N/A", 3)
    } else {
      tbl[[lbl]] <- c(
        format_summary_range(row$functioning_min, row$functioning_max),
        format_summary_range(row$at_risk_min, row$at_risk_max),
        format_summary_range(row$not_functioning_min, row$not_functioning_max)
      )
    }
  }
  tbl
}
