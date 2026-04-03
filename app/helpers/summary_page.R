## -- Summary Page Helpers ------------------------------------------------------
## Shared state helpers for the cross-metric Summary page.

library(dplyr)
library(purrr)
library(tibble)

ANALYSIS_STEP_LABELS <- c(
  "Exploratory",
  "Cross-Metric Analysis",
  "Verification",
  "Reference Curves"
)

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

normalize_curve_stratification_value <- function(rv, metric, selected) {
  if (is.null(selected) || is.na(selected) || identical(selected, "") || identical(selected, "none")) {
    return("none")
  }

  allowed <- get_metric_allowed_strats(rv, metric)
  if (selected %in% allowed) {
    return(selected)
  }

  "none"
}

get_metric_curve_strat_recommendation <- function(rv, metric) {
  candidates <- get_metric_phase1_candidate_table(rv, metric, include_all_allowed = TRUE)
  if (is.null(candidates) || nrow(candidates) == 0) {
    return("none")
  }

  ranked <- candidates |>
    dplyr::filter(candidate_status %in% c("promising", "possible")) |>
    dplyr::arrange(match(candidate_status, c("promising", "possible")), p_value, stratification)

  if (nrow(ranked) == 0) {
    return("none")
  }

  normalize_curve_stratification_value(rv, metric, ranked$stratification[1])
}

get_metric_curve_stratification <- function(rv, metric, fallback_to_auto = TRUE) {
  stored <- rv$curve_stratification[[metric]] %||% NULL
  normalized <- normalize_curve_stratification_value(rv, metric, stored)

  if (!is.null(stored) || !isTRUE(fallback_to_auto)) {
    return(normalized)
  }

  get_metric_curve_strat_recommendation(rv, metric)
}

get_metric_curve_strat_choices <- function(rv, metric) {
  allowed <- get_metric_allowed_strats(rv, metric)
  stats::setNames(
    c("none", allowed),
    c("None", vapply(allowed, function(sk) get_strat_display_name(rv, sk), character(1)))
  )
}

set_metric_curve_stratification <- function(rv, metric, selected, clear_phase4 = TRUE) {
  old_value <- get_metric_curve_stratification(rv, metric, fallback_to_auto = FALSE)
  normalized <- normalize_curve_stratification_value(rv, metric, selected)

  rv$curve_stratification[[metric]] <- normalized
  sync_metric_decision_state(rv, metric, build_metric_strat_decision(rv, metric, normalized))

  if (isTRUE(clear_phase4) && !identical(old_value, normalized)) {
    clear_metric_phase4_results(rv, metric)
  }

  invisible(normalized)
}

get_metric_curve_strat_label <- function(rv, metric, selected = get_metric_curve_stratification(rv, metric)) {
  if (is.null(selected) || identical(selected, "none")) {
    return("None")
  }

  get_strat_display_name(rv, selected)
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
  stats::setNames(replicate(length(ANALYSIS_STEP_LABELS), list(), simplify = FALSE), ANALYSIS_STEP_LABELS)
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

recompute_phase2_shared <- function(rv,
                                    settings = rv$phase2_settings %||% empty_phase2_settings(),
                                    persist_settings = TRUE) {
  settings <- if (isTRUE(persist_settings)) {
    set_phase2_settings(rv, settings)
  } else {
    normalize_phase2_settings(rv, settings)
  }

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

refresh_phase2_ranking_shared <- function(rv,
                                          settings = rv$phase2_settings %||% empty_phase2_settings(),
                                          persist_settings = TRUE) {
  settings <- if (isTRUE(persist_settings)) {
    set_phase2_settings(rv, settings)
  } else {
    normalize_phase2_settings(rv, settings)
  }
  result <- rv$cross_metric_consistency %||% NULL

  if (is.null(result) || is.null(result$summary) || !is.data.frame(result$summary) ||
      nrow(result$summary) == 0) {
    rv$phase2_ranking <- NULL
    return(invisible(NULL))
  }

  rv$phase2_ranking <- build_phase2_ranking(
    result,
    rv$phase1_candidates,
    settings$support_threshold
  )

  invisible(list(result = result, ranking = rv$phase2_ranking, settings = settings))
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
    status <- auto_phase1_candidate_status(p_val, es_label)

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
      reviewer_note = ""
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
  curve_choice <- get_metric_curve_stratification(rv, metric)
  if (!is.null(curve_choice) && nzchar(curve_choice)) {
    return(curve_choice)
  }

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
  ensure_metric_phase_cache(rv, metric)

  if (identical(rv$current_metric, metric) &&
      !identical(rv$strat_decision_user %||% NULL, decision_tbl %||% NULL)) {
    rv$strat_decision_user <- decision_tbl
  }

  if (!identical(rv$metric_phase_cache[[metric]]$strat_decision_user %||% NULL,
                 decision_tbl %||% NULL)) {
    rv$metric_phase_cache[[metric]]$strat_decision_user <- decision_tbl
  }
  invisible(NULL)
}

set_metric_phase3_selected <- function(rv, metric, selected_strat) {
  choices <- get_metric_phase3_choices(rv, metric)
  normalized <- if (is.null(selected_strat) || is.na(selected_strat) || selected_strat == "") {
    "none"
  } else if (selected_strat %in% c("none", choices, get_metric_allowed_strats(rv, metric))) {
    normalize_curve_stratification_value(rv, metric, selected_strat)
  } else {
    "none"
  }

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

  set_metric_curve_stratification(rv, metric, normalized, clear_phase4 = FALSE)
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
  rv$metric_phase_cache[[metric]]$phase4_artifact_mode <- NULL
  rv$metric_phase_cache[[metric]]$phase4_curve_rows <- NULL
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

  build_metric_strat_decision(rv, metric, get_metric_curve_stratification(rv, metric))
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

phase4_artifact_mode_satisfies <- function(entry_mode = NULL, artifact_mode = c("summary", "full")) {
  artifact_mode <- match.arg(artifact_mode)
  resolved_mode <- entry_mode %||% "full"

  if (identical(artifact_mode, "summary")) {
    return(resolved_mode %in% c("summary", "full"))
  }

  identical(resolved_mode, "full")
}

get_metric_phase4_artifact_mode <- function(rv, metric) {
  cached <- rv$metric_phase_cache[[metric]]$phase4_artifact_mode %||% NULL
  completed <- rv$completed_metrics[[metric]]$phase4_artifact_mode %||% NULL
  cached %||% completed
}

get_metric_phase4_cached_result <- function(rv, metric) {
  cache_entry <- rv$metric_phase_cache[[metric]] %||% list()
  completed_entry <- rv$completed_metrics[[metric]] %||% list()

  stratum_results <- cache_entry$stratum_results %||% completed_entry$stratum_results %||% NULL
  reference_curve <- cache_entry$reference_curve %||% completed_entry$reference_curve %||% NULL
  curve_rows <- cache_entry$phase4_curve_rows %||% completed_entry$phase4_curve_rows %||% NULL

  list(
    signature = cache_entry$phase4_signature %||% completed_entry$phase4_signature %||% NULL,
    artifact_mode = cache_entry$phase4_artifact_mode %||% completed_entry$phase4_artifact_mode %||% NULL,
    reference_curve = reference_curve,
    stratum_results = stratum_results,
    curve_rows = curve_rows
  )
}

extract_metric_phase4_curve_rows <- function(entry) {
  if (is.null(entry) || identical(entry$type, "regional")) {
    return(tibble::tibble())
  }

  stored_curve_rows <- entry$curve_rows %||% entry$phase4_curve_rows %||% NULL
  if (!is.null(stored_curve_rows)) {
    return(tibble::as_tibble(stored_curve_rows))
  }

  if (!is.null(entry$stratum_results)) {
    rows <- purrr::imap_dfr(entry$stratum_results, function(x, lvl) {
      result <- normalize_reference_curve_result(x$reference_curve %||% x, stratum_label = lvl)
      if (!is.null(result) && !is.null(result$curve_row)) {
        result$curve_row
      } else {
        NULL
      }
    })
    return(rows)
  }

  if (!is.null(entry$reference_curve) && !is.null(entry$reference_curve$curve_row)) {
    normalized <- normalize_reference_curve_result(entry$reference_curve)
    return(normalized$curve_row %||% tibble::tibble())
  }

  tibble::tibble()
}

update_metric_phase4_completed_entry <- function(rv, metric, phase4_fields) {
  existing <- rv$completed_metrics[[metric]] %||% list()
  rv$completed_metrics[[metric]] <- utils::modifyList(existing, phase4_fields, keep.null = TRUE)
  invisible(rv$completed_metrics[[metric]])
}

metric_phase4_entry_is_current <- function(entry, expected_signature) {
  if (is.null(entry) || identical(entry$type, "regional")) {
    return(FALSE)
  }

  entry_signature <- entry$phase4_signature %||% NULL
  if (is.null(entry_signature) || !phase4_signature_matches(entry_signature, expected_signature)) {
    return(FALSE)
  }

  nrow(extract_metric_phase4_curve_rows(entry)) > 0
}

get_metric_phase4_display_state <- function(rv, metric,
                                            decision_tbl = get_metric_phase4_decision_state(rv, metric)) {
  expected_signature <- build_metric_phase4_signature(rv, metric, decision_tbl)

  cache_entry <- rv$metric_phase_cache[[metric]] %||% list()
  cache_entry$strat_decision <- cache_entry$strat_decision_user %||% decision_tbl

  completed_entry <- rv$completed_metrics[[metric]] %||% list()

  if (metric_phase4_entry_is_current(cache_entry, expected_signature)) {
    return(list(
      source = "cache",
      artifact_mode = cache_entry$phase4_artifact_mode %||% "full",
      reference_curve = cache_entry$reference_curve %||% NULL,
      stratum_results = cache_entry$stratum_results %||% NULL,
      strat_decision = cache_entry$strat_decision %||% decision_tbl,
      curve_rows = extract_metric_phase4_curve_rows(cache_entry)
    ))
  }

  if (metric_phase4_entry_is_current(completed_entry, expected_signature)) {
    return(list(
      source = "completed",
      artifact_mode = completed_entry$phase4_artifact_mode %||% "full",
      reference_curve = completed_entry$reference_curve %||% NULL,
      stratum_results = completed_entry$stratum_results %||% NULL,
      strat_decision = completed_entry$strat_decision %||% decision_tbl,
      curve_rows = extract_metric_phase4_curve_rows(completed_entry)
    ))
  }

  list(
    source = "none",
    artifact_mode = NULL,
    reference_curve = NULL,
    stratum_results = NULL,
    strat_decision = decision_tbl,
    curve_rows = tibble::tibble()
  )
}

metric_has_any_phase4_results <- function(rv, metric) {
  cached <- get_metric_phase4_cached_result(rv, metric)
  !is.null(cached$reference_curve) || !is.null(cached$stratum_results)
}

metric_has_phase4_cache <- function(rv, metric,
                                    decision_tbl = get_metric_phase4_decision_state(rv, metric),
                                    artifact_mode = c("full", "summary")) {
  artifact_mode <- match.arg(artifact_mode)
  cached <- get_metric_phase4_cached_result(rv, metric)
  expected_signature <- build_metric_phase4_signature(rv, metric, decision_tbl)

  if (is.null(cached$signature) || !phase4_signature_matches(cached$signature, expected_signature)) {
    return(FALSE)
  }

  if (!phase4_artifact_mode_satisfies(cached$artifact_mode, artifact_mode)) {
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

metric_needs_phase4_artifact_refresh <- function(rv, metric,
                                                 artifact_mode = c("full", "summary")) {
  artifact_mode <- match.arg(artifact_mode)
  !metric_has_phase4_cache(
    rv,
    metric,
    artifact_mode = artifact_mode
  )
}

cache_metric_phase4_results <- function(rv, metric,
                                        decision_tbl = get_metric_phase4_decision_state(rv, metric),
                                        reference_curve = NULL,
                                        stratum_results = NULL,
                                        artifact_mode = c("full", "summary")) {
  artifact_mode <- match.arg(artifact_mode)
  ensure_metric_phase_cache(rv, metric)

  signature <- build_metric_phase4_signature(rv, metric, decision_tbl)
  curve_rows <- extract_metric_phase4_curve_rows(list(
    reference_curve = reference_curve,
    stratum_results = stratum_results
  ))

  rv$metric_phase_cache[[metric]]$strat_decision_user <- decision_tbl
  rv$metric_phase_cache[[metric]]$reference_curve <- reference_curve
  rv$metric_phase_cache[[metric]]$current_stratum_level <- NULL
  rv$metric_phase_cache[[metric]]$phase4_data <- rv$data
  rv$metric_phase_cache[[metric]]$stratum_results <- stratum_results
  rv$metric_phase_cache[[metric]]$phase4_signature <- signature
  rv$metric_phase_cache[[metric]]$phase4_artifact_mode <- artifact_mode
  rv$metric_phase_cache[[metric]]$phase4_curve_rows <- curve_rows

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

count_metric_phase4_preload_steps <- function(rv, metric, artifact_mode = c("full", "summary")) {
  artifact_mode <- match.arg(artifact_mode)
  decision_tbl <- get_metric_phase4_decision_state(rv, metric)
  if (metric_has_phase4_cache(rv, metric, decision_tbl, artifact_mode = artifact_mode)) {
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

preload_metric_phase4_workspace <- function(rv, metric, progress = NULL,
                                            artifact_mode = c("full", "summary")) {
  artifact_mode <- match.arg(artifact_mode)
  decision_tbl <- get_metric_phase4_decision_state(rv, metric)
  sync_metric_decision_state(rv, metric, decision_tbl)
  rv$phase4_data <- rv$data
  rv$current_stratum_level <- NULL
  expected_signature <- build_metric_phase4_signature(rv, metric, decision_tbl)
  cached <- get_metric_phase4_cached_result(rv, metric)

  if (!is.null(cached$signature) &&
      phase4_signature_matches(cached$signature, expected_signature) &&
      phase4_artifact_mode_satisfies(cached$artifact_mode, artifact_mode) &&
      nrow(extract_metric_phase4_curve_rows(cached)) > 0) {
    if (!is.null(cached$stratum_results)) {
      hydrated_strata <- purrr::imap(cached$stratum_results, function(entry, lvl) {
        list(reference_curve = hydrate_reference_curve_result(
          entry$reference_curve %||% entry,
          rv$data,
          metric,
          rv$metric_config,
          stratum_label = lvl,
          artifact_mode = artifact_mode
        ))
      })

      cache_metric_phase4_results(
        rv,
        metric,
        decision_tbl = decision_tbl,
        stratum_results = hydrated_strata,
        artifact_mode = artifact_mode
      )
      rv$reference_curve <- NULL
    } else if (!is.null(cached$reference_curve)) {
      hydrated_curve <- hydrate_reference_curve_result(
        cached$reference_curve,
        rv$data,
        metric,
        rv$metric_config,
        artifact_mode = artifact_mode
      )
      rv$reference_curve <- hydrated_curve
      cache_metric_phase4_results(
        rv,
        metric,
        decision_tbl = decision_tbl,
        reference_curve = hydrated_curve,
        artifact_mode = artifact_mode
      )
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
      cache_metric_phase4_results(
        rv,
        metric,
        decision_tbl = decision_tbl,
        stratum_results = list(),
        artifact_mode = artifact_mode
      )
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
          stratum_label = lvl,
          build_plots = identical(artifact_mode, "full")
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
      stratum_results = stratum_results,
      artifact_mode = artifact_mode
    )
    advance_modal_progress(progress, "Prepared Phase 4 workspace.")
    return(invisible(TRUE))
  }

  set_modal_progress_detail(progress, "Building reference curve...")
  curve_result <- build_reference_curve(
    rv$data,
    metric,
    rv$metric_config,
    build_plots = identical(artifact_mode, "full")
  )
  advance_modal_progress(progress, "Built reference curve.")

  set_modal_progress_detail(progress, "Caching Phase 4 results...")
  cache_metric_phase4_results(
    rv,
    metric,
    decision_tbl = decision_tbl,
    reference_curve = curve_result,
    artifact_mode = artifact_mode
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
    plot_specs = screening$plot_specs %||% list(),
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
  plot_specs <- setNames(lapply(results_list, `[[`, "plot_spec"), names(results_list))
  plot_specs <- Filter(Negate(is.null), plot_specs)

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
      plots = plots,
      plot_specs = plot_specs
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
  normalized_selected <- get_metric_curve_stratification(rv, metric)

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
  stored_choice <- rv$curve_stratification[[metric]] %||% NULL
  if (is.null(stored_choice) || identical(stored_choice, "none")) {
    return(invisible(FALSE))
  }

  if (stored_choice %in% get_metric_allowed_strats(rv, metric)) {
    return(invisible(FALSE))
  }

  rv$curve_stratification[[metric]] <- NULL
  sync_metric_decision_state(rv, metric, build_metric_strat_decision(rv, metric, "none"))
  clear_metric_phase4_results(rv, metric)
  invisible(TRUE)
}

set_metric_available_strats <- function(rv, metric, selected) {
  current_allowed <- get_metric_allowed_strats(rv, metric)
  base_allowed <- get_metric_config_allowed_strats(rv, metric)
  all_choices <- get_summary_available_choices(rv)
  new_allowed <- all_choices[all_choices %in% unique(selected %||% character(0))]

  old_curve_choice <- get_metric_curve_stratification(rv, metric)
  old_recommendation <- get_metric_curve_strat_recommendation(rv, metric)

  if (identical(sort(new_allowed), sort(base_allowed))) {
    rv$summary_available_overrides[[metric]] <- NULL
  } else {
    rv$summary_available_overrides[[metric]] <- new_allowed
  }

  removed <- setdiff(current_allowed, new_allowed)
  invalidated <- ensure_metric_phase3_valid(rv, metric)
  new_recommendation <- get_metric_curve_strat_recommendation(rv, metric)

  exploratory_notes <- if (length(removed) > 0) {
    list(list(
      level = "warning",
      text = paste0(
        "Unavailable stratifications were removed from this metric: ",
        format_strat_list(rv, removed)
      )
    ))
  } else {
    list()
  }

  cross_metric_notes <- if (!identical(old_recommendation, new_recommendation)) {
    list(list(
      level = "warning",
      text = paste0(
        "The recommended curve stratification changed to ",
        get_metric_curve_strat_label(rv, metric, new_recommendation),
        " after the available stratifications were updated."
      )
    ))
  } else {
    list()
  }

  verification_notes <- list()

  reference_notes <- if (isTRUE(invalidated) && !identical(old_curve_choice, "none")) {
    list(list(
      level = "warning",
      text = paste0(
        "The previous curve stratification (",
        get_metric_curve_strat_label(rv, metric, old_curve_choice),
        ") is no longer available. Phase 4 outputs were cleared."
      )
    ))
  } else {
    list()
  }

  set_metric_summary_edit_notes(rv, metric, "Exploratory", exploratory_notes)
  set_metric_summary_edit_notes(rv, metric, "Cross-Metric Analysis", cross_metric_notes)
  set_metric_summary_edit_notes(rv, metric, "Verification", verification_notes)
  set_metric_summary_edit_notes(rv, metric, "Reference Curves", reference_notes)
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
  get_metric_phase4_display_state(rv, metric)$curve_rows
}

manual_curve_info_from_rows <- function(curve_rows, metric, display_name = metric) {
  curve_rows <- tibble::as_tibble(curve_rows %||% tibble::tibble())

  if (nrow(curve_rows) == 0 || !("curve_source" %in% names(curve_rows))) {
    return(list(
      metric = metric,
      display_name = display_name,
      has_manual_curve = FALSE,
      manual_curve_count = 0L,
      manual_strata = character(0),
      summary_label = NULL,
      selection_label = paste0(display_name, " (manual curve)")
    ))
  }

  manual_rows <- curve_rows |>
    dplyr::filter(as.character(curve_source %||% "") == "manual")

  manual_strata <- character(0)
  if ("stratum" %in% names(manual_rows) && nrow(manual_rows) > 0) {
    manual_strata <- as.character(manual_rows$stratum %||% character(0))
    manual_strata <- manual_strata[!is.na(manual_strata) & nzchar(manual_strata)]
    manual_strata <- sort(unique(manual_strata))
  }

  has_manual_curve <- nrow(manual_rows) > 0
  summary_label <- if (!isTRUE(has_manual_curve)) {
    NULL
  } else if (length(manual_strata) > 0) {
    paste0("Manual strata: ", paste(manual_strata, collapse = ", "))
  } else if (nrow(manual_rows) > 1) {
    "Manual curves"
  } else {
    "Manual curve"
  }

  selection_label <- if (!isTRUE(has_manual_curve)) {
    paste0(display_name, " (manual curve)")
  } else if (length(manual_strata) > 0) {
    paste0(display_name, " (manual strata: ", paste(manual_strata, collapse = ", "), ")")
  } else if (nrow(manual_rows) > 1) {
    paste0(display_name, " (manual curves)")
  } else {
    paste0(display_name, " (manual curve)")
  }

  list(
    metric = metric,
    display_name = display_name,
    has_manual_curve = has_manual_curve,
    manual_curve_count = nrow(manual_rows),
    manual_strata = manual_strata,
    summary_label = summary_label,
    selection_label = selection_label
  )
}

get_metric_phase4_manual_curve_info <- function(rv, metric,
                                                phase4 = get_metric_phase4_display_state(rv, metric)) {
  display_name <- rv$metric_config[[metric]]$display_name %||% metric
  manual_curve_info_from_rows(phase4$curve_rows %||% tibble::tibble(), metric, display_name)
}

build_summary_recompute_plan <- function(rv, metrics) {
  metrics <- intersect(metrics %||% character(0), eligible_summary_metrics(rv$metric_config))
  if (length(metrics) == 0) {
    return(list(
      auto_metrics = character(0),
      manual_metrics = character(0),
      manual_info = tibble::tibble()
    ))
  }

  info_rows <- purrr::map_dfr(metrics, function(metric) {
    info <- get_metric_phase4_manual_curve_info(rv, metric)
    tibble::tibble(
      metric = info$metric,
      display_name = info$display_name,
      has_manual_curve = isTRUE(info$has_manual_curve),
      manual_curve_count = as.integer(info$manual_curve_count),
      manual_strata = list(info$manual_strata),
      summary_label = info$summary_label %||% NA_character_,
      selection_label = info$selection_label
    )
  })

  manual_info <- info_rows |>
    dplyr::filter(has_manual_curve)

  list(
    auto_metrics = setdiff(metrics, manual_info$metric),
    manual_metrics = manual_info$metric,
    manual_info = manual_info
  )
}

resolve_summary_recompute_metrics <- function(auto_metrics, manual_metrics,
                                              selected_manual_metrics = character(0)) {
  auto_metrics <- auto_metrics %||% character(0)
  manual_metrics <- manual_metrics %||% character(0)
  selected_manual_metrics <- intersect(selected_manual_metrics %||% character(0), manual_metrics)

  unique(c(auto_metrics, selected_manual_metrics))
}

metric_has_official_curve <- function(rv, metric) {
  nrow(get_metric_curve_rows(rv, metric)) > 0
}

recompute_metric_phase4 <- function(rv, metric, artifact_mode = c("full", "summary")) {
  artifact_mode <- match.arg(artifact_mode)
  selection <- get_metric_curve_stratification(rv, metric)
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
          stratum_label = lvl,
          build_plots = identical(artifact_mode, "full")
        )
      )
    }

    curve_rows <- extract_metric_phase4_curve_rows(list(stratum_results = stratum_results))
    clear_metric_phase4_results(rv, metric)
    phase4_signature <- cache_metric_phase4_results(
      rv,
      metric,
      decision_tbl = decision_tbl,
      stratum_results = stratum_results,
      artifact_mode = artifact_mode
    )
    update_metric_phase4_completed_entry(rv, metric, list(
      stratified = TRUE,
      strat_var = strat_key,
      strat_decision = decision_tbl,
      stratum_results = stratum_results,
      phase4_signature = phase4_signature,
      phase4_artifact_mode = artifact_mode,
      phase4_curve_rows = curve_rows
    ))

    return(invisible(rv$completed_metrics[[metric]]))
  }

  curve_result <- build_reference_curve(
    rv$data,
    metric,
    rv$metric_config,
    build_plots = identical(artifact_mode, "full")
  )
  curve_rows <- extract_metric_phase4_curve_rows(list(reference_curve = curve_result))
  clear_metric_phase4_results(rv, metric)
  phase4_signature <- cache_metric_phase4_results(
    rv,
    metric,
    decision_tbl = decision_tbl,
    reference_curve = curve_result,
    artifact_mode = artifact_mode
  )
  update_metric_phase4_completed_entry(rv, metric, list(
    strat_decision = decision_tbl,
    reference_curve = curve_result,
    phase4_signature = phase4_signature,
    phase4_artifact_mode = artifact_mode,
    phase4_curve_rows = curve_rows
  ))

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
  recompute_metric_phase4(
    rv,
    metric,
    artifact_mode = if (identical(mode, "summary")) "summary" else "full"
  )
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
    recompute_metric_phase4(
      rv,
      metric,
      artifact_mode = if (identical(mode, "summary")) "summary" else "full"
    )
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

  range_name <- sub("_min$", "", min_field)
  if (identical(paste0(range_name, "_max"), max_field)) {
    return(reference_curve_row_range_display(curve_rows, range_name, digits = digits))
  }

  format_summary_range(curve_rows[[min_field]][1], curve_rows[[max_field]][1], digits)
}

build_summary_snapshot_context <- function(rv) {
  available_choices <- get_summary_available_choices(rv)

  list(
    data_fingerprint = rv$data_fingerprint %||% NULL,
    config_version = rv$config_version %||% 0L,
    available_choices = available_choices,
    strat_label_map = stats::setNames(vapply(available_choices, function(sk) {
      get_strat_display_name(rv, sk)
    }, character(1)), available_choices)
  )
}

build_metric_summary_snapshot_signature <- function(rv, metric,
                                                    context = build_summary_snapshot_context(rv),
                                                    phase4 = get_metric_phase4_display_state(rv, metric),
                                                    phase3_state = get_metric_phase3_display_state(rv, metric)) {
  precheck <- get_metric_precheck_row(rv, metric)
  p1_screening <- rv$all_layer1_results[[metric]] %||% NULL
  p1_selected <- sort(get_metric_phase1_selected(rv, metric) %||% character(0))
  p2_selected <- sort(get_metric_phase2_passed(rv, metric) %||% character(0))
  phase3_flagged <- character(0)
  if (!is.null(phase3_state$feasibility) && nrow(phase3_state$feasibility) > 0) {
    phase3_flagged <- phase3_state$feasibility |>
      dplyr::filter(feasibility_flag != "feasible") |>
      dplyr::pull(stratification) |>
      as.character() |>
      sort()
  }

  list(
    metric = metric,
    data_fingerprint = context$data_fingerprint,
    config_version = context$config_version,
    n_obs = get_first_value(precheck, "n_obs", "N/A"),
    available_selected = sort(get_metric_allowed_strats(rv, metric) %||% character(0)),
    available_choices = context$available_choices %||% character(0),
    curve_strat_used = get_metric_curve_stratification(rv, metric),
    curve_strat_choices = as.list(get_metric_curve_strat_choices(rv, metric) %||% character(0)),
    phase1_has_results = !is.null(p1_screening) && nrow(p1_screening) > 0,
    phase1_selected = p1_selected,
    phase2_selected = p2_selected,
    phase3_strats = sort(phase3_state$strats %||% character(0)),
    phase3_flagged = phase3_flagged,
    phase4_source = phase4$source %||% "none",
    phase4_signature = get_metric_phase4_signature(rv, metric),
    phase4_artifact_mode = phase4$artifact_mode %||% NULL,
    curve_rows = phase4$curve_rows %||% tibble::tibble(),
    edit_notes = rv$summary_edit_notes[[metric]] %||% list()
  )
}

build_metric_notes <- function(rv, metric,
                               phase4 = get_metric_phase4_display_state(rv, metric)) {
  notes <- empty_summary_note_store()

  add_note <- function(step, level, text) {
    notes[[step]] <<- c(notes[[step]], list(list(level = level, text = text)))
  }

  edit_notes <- get_metric_summary_edit_notes(rv, metric)
  for (phase in names(notes)) {
    notes[[phase]] <- c(notes[[phase]], edit_notes[[phase]])
  }

  allowed <- get_metric_allowed_strats(rv, metric)
  p1_tbl <- get_metric_phase1_candidate_table(rv, metric)
  p1_selected <- get_metric_phase1_selected(rv, metric)
  p1_screening <- rv$all_layer1_results[[metric]]

  if (is.null(p1_screening) || nrow(p1_screening) == 0) {
    add_note("Exploratory", "warning", "Exploratory screening has not been run for this metric.")
  } else {
    add_note("Exploratory", "info", paste0("Stratifications available for analysis: ", format_strat_list(rv, allowed)))
    if (length(p1_selected) > 0) {
      add_note("Exploratory", "info", paste0("Automatic screening shortlist: ", format_strat_list(rv, p1_selected)))
    } else {
      add_note("Exploratory", "warning", "No stratifications met the automatic exploratory shortlist criteria.")
    }
  }

  p2_selected <- get_metric_phase2_passed(rv, metric)
  if (length(p2_selected) > 0) {
    add_note("Cross-Metric Analysis", "info", paste0("Broad-use candidates in the current cross-metric analysis: ", format_strat_list(rv, p2_selected)))
  } else {
    add_note("Cross-Metric Analysis", "info", "No broad-use candidates are currently highlighted for this metric.")
  }

  verification_state <- get_metric_phase3_display_state(rv, metric)
  if (is.null(verification_state)) {
    add_note("Verification", "info", "Verification diagnostics have not been run yet.")
  } else {
    add_note("Verification", "info", paste0("Verification diagnostics available for ", length(verification_state$strats %||% character(0)), " stratification(s)."))
    if (!is.null(verification_state$feasibility) && nrow(verification_state$feasibility) > 0) {
      flagged <- verification_state$feasibility |>
        dplyr::filter(feasibility_flag != "feasible") |>
        dplyr::pull(stratification)
      if (length(flagged) > 0) {
        add_note("Verification", "warning", paste0("Verification flagged feasibility concerns for: ", format_strat_list(rv, flagged)))
      }
    }
  }

  curve_rows <- phase4$curve_rows %||% tibble::tibble()
  curve_choice <- get_metric_curve_stratification(rv, metric)
  curve_recommendation <- get_metric_curve_strat_recommendation(rv, metric)

  add_note("Reference Curves", "info", paste0("Stratification used for curves: ", get_metric_curve_strat_label(rv, metric, curve_choice)))
  add_note("Reference Curves", "info", paste0("Current recommendation: ", get_metric_curve_strat_label(rv, metric, curve_recommendation)))

  if (nrow(curve_rows) == 0) {
    add_note("Reference Curves", "warning", "Reference curve outputs are not current. Recompute is required.")
  } else {
    add_note("Reference Curves", "info", paste0("Reference curve outputs available for ", nrow(curve_rows), " curve(s)."))

    curve_flags <- unique(curve_rows$curve_status[curve_rows$curve_status != "complete"])
    if (length(curve_flags) > 0) {
      add_note("Reference Curves", "warning",
               paste0("Reference curve issues: ", paste(curve_flags, collapse = ", ")))
    }

    if (!identical(curve_choice, "none")) {
      strat_values <- get_stratification_values(rv$data, curve_choice, rv$strat_config)
      min_n <- rv$metric_config[[metric]]$min_sample_size %||% 10
      sparse <- purrr::keep(sort(unique(stats::na.omit(strat_values))), function(lvl) {
        sum(strat_values == lvl, na.rm = TRUE) < min_n
      })
      if (length(sparse) > 0) {
        sparse_text <- paste(vapply(sparse, function(lvl) {
          paste0(lvl, " (n=", sum(strat_values == lvl, na.rm = TRUE), ")")
        }, character(1)), collapse = ", ")
        add_note("Reference Curves", "warning",
                 paste0("Strata below minimum n=", min_n, ": ", sparse_text))
      }
    }
  }

  notes
}

metric_summary_status <- function(rv, metric,
                                  phase4 = get_metric_phase4_display_state(rv, metric)) {
  notes <- build_metric_notes(rv, metric, phase4 = phase4)
  has_warning <- any(unlist(lapply(notes, function(items) {
    vapply(items, function(item) identical(item$level, "warning"), logical(1))
  })))
  curve_rows <- phase4$curve_rows %||% tibble::tibble()

  if (nrow(curve_rows) == 0) {
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

build_metric_summary_snapshot <- function(rv, metric,
                                          context = build_summary_snapshot_context(rv)) {
  mc <- rv$metric_config[[metric]]
  precheck <- get_metric_precheck_row(rv, metric)
  allowed <- get_metric_allowed_strats(rv, metric)
  available_choices <- context$available_choices %||% get_summary_available_choices(rv)
  curve_strat_used <- get_metric_curve_stratification(rv, metric)
  curve_strat_choices <- get_metric_curve_strat_choices(rv, metric)
  curve_strat_recommended <- get_metric_curve_strat_recommendation(rv, metric)

  phase4 <- get_metric_phase4_display_state(rv, metric)
  status <- metric_summary_status(rv, metric, phase4 = phase4)
  curve_rows <- phase4$curve_rows
  manual_curve_info <- get_metric_phase4_manual_curve_info(rv, metric, phase4 = phase4)
  strat_label_map <- context$strat_label_map %||% stats::setNames(vapply(available_choices, function(sk) {
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
    curve_strat_used = curve_strat_used,
    curve_strat_recommended = curve_strat_recommended,
    curve_strat_choices = curve_strat_choices,
    curve_rows = curve_rows,
    phase4 = phase4,
    phase4_source = phase4$source,
    phase4_artifact_mode = phase4$artifact_mode,
    strat_label_map = strat_label_map,
    has_manual_curve = manual_curve_info$has_manual_curve,
    manual_curve_count = manual_curve_info$manual_curve_count,
    manual_curve_strata = manual_curve_info$manual_strata,
    manual_curve_label = manual_curve_info$summary_label,
    manual_curve_selection_label = manual_curve_info$selection_label
  )
}

build_stratified_stats_table <- function(curve_rows) {
  stats <- c("n", "Min", "Q25", "Median", "Q75", "Max", "IQR", "SD")
  tbl <- data.frame(Statistic = stats, stringsAsFactors = FALSE, check.names = FALSE)

  for (i in seq_len(nrow(curve_rows))) {
    row <- curve_rows[i, ]
    lbl <- row$stratum %||% paste0("Curve ", i)
    if (is.null(lbl) || is.na(lbl) || !nzchar(lbl)) {
      lbl <- paste0("Curve ", i)
    }
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
    if (is.null(lbl) || is.na(lbl) || !nzchar(lbl)) {
      lbl <- paste0("Curve ", i)
    }
    if (row$curve_status == "insufficient_data") {
      tbl[[lbl]] <- rep("N/A", 3)
    } else {
      tbl[[lbl]] <- c(
        reference_curve_row_range_display(row, "functioning"),
        reference_curve_row_range_display(row, "at_risk"),
        reference_curve_row_range_display(row, "not_functioning")
      )
    }
  }
  tbl
}

flatten_summary_note_text <- function(notes, level = NULL) {
  if (is.null(notes) || length(notes) == 0) {
    return(character(0))
  }

  out <- character(0)
  for (step in names(notes)) {
    step_items <- notes[[step]] %||% list()
    if (length(step_items) == 0) {
      next
    }

    for (item in step_items) {
      item_level <- item$level %||% NULL
      if (!is.null(level) && !identical(item_level, level)) {
        next
      }

      item_text <- item$text %||% ""
      if (!nzchar(item_text)) {
        next
      }

      out <- c(out, paste0(step, ": ", item_text))
    }
  }

  unique(out)
}

metric_curve_summary_label <- function(snapshot) {
  curve_rows <- tibble::as_tibble(snapshot$curve_rows %||% tibble::tibble())

  if (nrow(curve_rows) == 0) {
    return("Not available")
  }

  if (nrow(curve_rows) == 1) {
    row <- curve_rows[1, , drop = FALSE]
    if (all(c("q25", "q75") %in% names(row)) && all(is.finite(c(row$q25[1], row$q75[1])))) {
      return(paste0(format_summary_number(row$q25[1]), " - ", format_summary_number(row$q75[1])))
    }
    return(reference_curve_row_range_display(row, "functioning"))
  }

  paste0(nrow(curve_rows), " stratified curves")
}

metric_appendix_plot <- function(rv, metric, snapshot = build_metric_summary_snapshot(rv, metric)) {
  curve_rows <- tibble::as_tibble(snapshot$curve_rows %||% tibble::tibble())
  if (nrow(curve_rows) == 0) {
    return(NULL)
  }

  if (nrow(curve_rows) > 1) {
    return(build_overlay_curve_plot(curve_rows, rv$metric_config))
  }

  phase4 <- snapshot$phase4 %||% list()
  reference_curve <- phase4$reference_curve %||% NULL
  if (!is.null(reference_curve$curve_plot)) {
    return(reference_curve$curve_plot)
  }

  hydrated <- hydrate_reference_curve_result(
    result = reference_curve,
    data = rv$data,
    metric_key = metric,
    metric_config = rv$metric_config,
    artifact_mode = "full"
  )

  hydrated$curve_plot %||% NULL
}

build_metric_status_export_row <- function(rv, snapshot) {
  warning_notes <- flatten_summary_note_text(snapshot$notes, level = "warning")
  info_notes <- flatten_summary_note_text(snapshot$notes, level = "info")

  tibble::tibble(
    metric = snapshot$metric,
    display_name = snapshot$display_name,
    family = snapshot$family,
    units = snapshot$units,
    direction = snapshot$direction,
    n_obs = snapshot$n_obs,
    status_label = snapshot$status$label %||% "Unknown",
    status_badge = snapshot$status$badge %||% "unknown",
    needs_review = isTRUE((snapshot$status$badge %||% "unknown") != "pass") || isTRUE(snapshot$has_manual_curve),
    available_stratifications = format_strat_list(rv, snapshot$available_selected %||% character(0)),
    selected_curve_stratification = snapshot$curve_strat_used %||% "none",
    selected_curve_stratification_label = get_metric_curve_strat_label(rv, snapshot$metric, snapshot$curve_strat_used),
    recommended_curve_stratification = snapshot$curve_strat_recommended %||% "none",
    recommended_curve_stratification_label = get_metric_curve_strat_label(rv, snapshot$metric, snapshot$curve_strat_recommended),
    phase4_source = snapshot$phase4_source %||% "none",
    phase4_artifact_mode = snapshot$phase4_artifact_mode %||% "none",
    curve_count = nrow(snapshot$curve_rows %||% tibble::tibble()),
    curve_summary = metric_curve_summary_label(snapshot),
    has_manual_curve = isTRUE(snapshot$has_manual_curve),
    manual_curve_count = as.integer(snapshot$manual_curve_count %||% 0L),
    manual_curve_label = snapshot$manual_curve_label %||% NA_character_,
    warning_count = length(warning_notes),
    warning_summary = if (length(warning_notes) > 0) paste(warning_notes, collapse = " | ") else NA_character_,
    note_summary = if (length(c(warning_notes, info_notes)) > 0) {
      paste(c(warning_notes, info_notes), collapse = " | ")
    } else {
      NA_character_
    }
  )
}

build_metric_decision_export_row <- function(rv, snapshot) {
  decision_tbl <- tibble::as_tibble(snapshot$phase4$strat_decision %||% tibble::tibble())
  if (nrow(decision_tbl) == 0) {
    decision_tbl <- build_metric_strat_decision(rv, snapshot$metric, snapshot$curve_strat_used)
  }

  decision_tbl |>
    dplyr::mutate(
      display_name = snapshot$display_name,
      selected_curve_stratification = snapshot$curve_strat_used %||% "none",
      selected_curve_stratification_label = get_metric_curve_strat_label(rv, snapshot$metric, snapshot$curve_strat_used),
      recommended_curve_stratification = snapshot$curve_strat_recommended %||% "none",
      recommended_curve_stratification_label = get_metric_curve_strat_label(rv, snapshot$metric, snapshot$curve_strat_recommended),
      phase4_source = snapshot$phase4_source %||% "none",
      phase4_artifact_mode = snapshot$phase4_artifact_mode %||% "none",
      status_label = snapshot$status$label %||% "Unknown",
      status_badge = snapshot$status$badge %||% "unknown",
      has_manual_curve = isTRUE(snapshot$has_manual_curve),
      manual_curve_label = snapshot$manual_curve_label %||% NA_character_
    )
}

build_phase2_summary_export <- function(rv) {
  ranking <- rv$phase2_ranking %||% NULL
  if (is.null(ranking) || !is.data.frame(ranking) || nrow(ranking) == 0) {
    return(tibble::tibble())
  }

  ranking <- tibble::as_tibble(ranking)
  if ("stratification" %in% names(ranking)) {
    ranking <- ranking |>
      dplyr::mutate(stratification_label = vapply(stratification, function(sk) {
        get_strat_display_name(rv, sk)
      }, character(1)))
  }

  settings <- rv$phase2_settings %||% empty_phase2_settings()
  ranking |>
    dplyr::mutate(
      sig_threshold = settings$sig_threshold %||% NA_real_,
      support_threshold = settings$support_threshold %||% NA_real_
    )
}

build_summary_export_context <- function(rv,
                                         metrics = eligible_summary_metrics(rv$metric_config),
                                         include_appendix_plots = FALSE,
                                         plots_dir = NULL) {
  metrics <- intersect(metrics %||% character(0), eligible_summary_metrics(rv$metric_config))
  snapshots <- lapply(metrics, function(metric) build_metric_summary_snapshot(rv, metric))
  names(snapshots) <- metrics

  metric_status <- purrr::map_dfr(snapshots, function(snapshot) {
    build_metric_status_export_row(rv, snapshot)
  })

  threshold_rows <- purrr::map_dfr(snapshots, function(snapshot) {
    curve_rows <- tibble::as_tibble(snapshot$curve_rows %||% tibble::tibble())
    if (nrow(curve_rows) == 0) {
      return(tibble::tibble())
    }

    reference_curve_rows_for_export(curve_rows)
  })

  current_decisions <- purrr::map_dfr(snapshots, function(snapshot) {
    build_metric_decision_export_row(rv, snapshot)
  })

  decision_history <- rv$decision_log %||% tibble::tibble()
  decision_history <- tibble::as_tibble(decision_history)

  regional_entries <- purrr::keep(rv$completed_metrics %||% list(), function(entry) {
    identical(entry$type %||% NULL, "regional")
  })
  regional_curves <- purrr::map_dfr(regional_entries, function(entry) {
    tibble::as_tibble(entry$model_summary %||% tibble::tibble()) |>
      dplyr::mutate(
        response = entry$response %||% get_first_value(entry$model_summary, "response", NA_character_),
        predictor = entry$predictor %||% get_first_value(entry$model_summary, "predictor", NA_character_),
        stratify = entry$stratify %||% "None"
      )
  })

  phase2_summary <- build_phase2_summary_export(rv)

  appendix_metrics <- lapply(snapshots, function(snapshot) {
    warning_notes <- flatten_summary_note_text(snapshot$notes, level = "warning")
    metric_info <- list(
      metric = snapshot$metric,
      display_name = snapshot$display_name,
      family = snapshot$family,
      units = snapshot$units,
      direction = snapshot$direction,
      n_obs = snapshot$n_obs,
      status_label = snapshot$status$label %||% "Unknown",
      status_badge = snapshot$status$badge %||% "unknown",
      selected_curve_stratification = snapshot$curve_strat_used %||% "none",
      selected_curve_stratification_label = get_metric_curve_strat_label(rv, snapshot$metric, snapshot$curve_strat_used),
      recommended_curve_stratification = snapshot$curve_strat_recommended %||% "none",
      recommended_curve_stratification_label = get_metric_curve_strat_label(rv, snapshot$metric, snapshot$curve_strat_recommended),
      phase4_source = snapshot$phase4_source %||% "none",
      phase4_artifact_mode = snapshot$phase4_artifact_mode %||% "none",
      curve_count = nrow(snapshot$curve_rows %||% tibble::tibble()),
      curve_summary = metric_curve_summary_label(snapshot),
      has_manual_curve = isTRUE(snapshot$has_manual_curve),
      manual_curve_count = as.integer(snapshot$manual_curve_count %||% 0L),
      manual_curve_label = snapshot$manual_curve_label %||% NA_character_,
      warning_summary = if (length(warning_notes) > 0) paste(warning_notes, collapse = " | ") else NA_character_,
      notes = snapshot$notes,
      stats_table = if (nrow(snapshot$curve_rows %||% tibble::tibble()) > 0) {
        build_stratified_stats_table(snapshot$curve_rows)
      } else {
        data.frame()
      },
      threshold_table = if (nrow(snapshot$curve_rows %||% tibble::tibble()) > 0) {
        build_stratified_threshold_table(snapshot$curve_rows)
      } else {
        data.frame()
      },
      plot_file = NA_character_
    )

    if (isTRUE(include_appendix_plots) && !is.null(plots_dir)) {
      plot_obj <- metric_appendix_plot(rv, snapshot$metric, snapshot = snapshot)
      if (!is.null(plot_obj)) {
        safe_metric <- gsub("[^A-Za-z0-9_-]+", "_", snapshot$metric)
        plot_path <- file.path(plots_dir, paste0(safe_metric, "_curve.png"))
        ggplot2::ggsave(plot_path, plot_obj, width = 8, height = 5, dpi = 300)
        metric_info$plot_file <- file.path("plots", basename(plot_path))
      }
    }

    metric_info
  })

  session_meta <- list(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    metric_count = length(metrics),
    complete_metrics = sum(metric_status$status_label != "Incomplete"),
    pending_metrics = sum(metric_status$status_label == "Incomplete"),
    review_metrics = sum(metric_status$needs_review %||% FALSE, na.rm = TRUE),
    manual_curve_metrics = sum(metric_status$has_manual_curve %||% FALSE, na.rm = TRUE),
    regional_curve_rows = nrow(regional_curves),
    regional_curve_sets = length(regional_entries),
    decision_history_entries = nrow(decision_history),
    phase2_available = nrow(phase2_summary) > 0
  )

  list(
    session_meta = session_meta,
    metric_status = metric_status,
    threshold_rows = threshold_rows,
    current_decisions = current_decisions,
    decision_history = decision_history,
    regional_curves = regional_curves,
    phase2_summary = phase2_summary,
    metrics = appendix_metrics
  )
}

write_summary_export_stage <- function(rv,
                                       bundle_dir,
                                       include_appendix_plots = FALSE) {
  dir.create(bundle_dir, showWarnings = FALSE, recursive = TRUE)
  plots_dir <- if (isTRUE(include_appendix_plots)) {
    dir.create(file.path(bundle_dir, "plots"), showWarnings = FALSE, recursive = TRUE)
    file.path(bundle_dir, "plots")
  } else {
    NULL
  }

  context <- build_summary_export_context(
    rv,
    include_appendix_plots = include_appendix_plots,
    plots_dir = plots_dir
  )

  file_map <- list(
    metric_status = file.path(bundle_dir, "metric_status.csv"),
    thresholds = file.path(bundle_dir, "reference_curve_thresholds.csv"),
    stratification_decisions = file.path(bundle_dir, "stratification_decisions.csv"),
    session_info = file.path(bundle_dir, "session_info.txt"),
    summary = file.path(bundle_dir, "summary.txt"),
    report_context = file.path(bundle_dir, "report_context.rds"),
    phase2_summary = file.path(bundle_dir, "phase2_summary.csv"),
    decision_history = file.path(bundle_dir, "decision_history.csv"),
    regional_curves = file.path(bundle_dir, "regional_curves.csv")
  )

  utils::write.csv(context$metric_status, file_map$metric_status, row.names = FALSE)
  utils::write.csv(context$threshold_rows, file_map$thresholds, row.names = FALSE)
  utils::write.csv(context$current_decisions, file_map$stratification_decisions, row.names = FALSE)
  utils::write.csv(context$phase2_summary, file_map$phase2_summary, row.names = FALSE)

  if (nrow(context$decision_history) > 0) {
    utils::write.csv(context$decision_history, file_map$decision_history, row.names = FALSE)
  }
  if (nrow(context$regional_curves) > 0) {
    utils::write.csv(context$regional_curves, file_map$regional_curves, row.names = FALSE)
  }

  saveRDS(context, file_map$report_context)
  writeLines(capture.output(sessionInfo()), file_map$session_info)
  writeLines(c(
    "StreamCurves Export Bundle",
    paste0("Generated: ", context$session_meta$generated_at),
    paste0("Summary metrics: ", context$session_meta$metric_count),
    paste0("Complete metrics: ", context$session_meta$complete_metrics),
    paste0("Pending metrics: ", context$session_meta$pending_metrics),
    paste0("Metrics needing review: ", context$session_meta$review_metrics),
    paste0("Manual curve metrics: ", context$session_meta$manual_curve_metrics),
    paste0("Regional curve rows: ", context$session_meta$regional_curve_rows),
    paste0("Decision history entries: ", context$session_meta$decision_history_entries)
  ), file_map$summary)

  list(bundle_dir = bundle_dir, files = file_map, context = context)
}
