## -- 03: Derived Variables & Factor Recoding ----------------------------------
## Computes derived predictors, applies factor recodes, and materializes
## workbook-defined custom stratifications.

library(dplyr)
library(forcats)
library(cli)

evaluate_derivation_method <- function(data, predictor_key, predictor_cfg) {
  target_col <- predictor_cfg$column_name %||% predictor_key
  method <- predictor_cfg$derivation_method %||% "none"
  source_cols <- predictor_cfg$source_columns %||% character(0)

  if (!isTRUE(predictor_cfg$derived)) {
    return(NULL)
  }

  if (identical(method, "none")) {
    return(NULL)
  }

  missing_sources <- setdiff(source_cols, names(data))
  if (length(missing_sources) > 0) {
    cli::cli_alert_info(
      "Skipping {.var {target_col}}: missing source column(s) {paste(missing_sources, collapse = ', ')}"
    )
    return(NULL)
  }

  result <- switch(
    method,
    sum = {
      Reduce(`+`, lapply(source_cols, function(col) data[[col]]))
    },
    multiply = {
      Reduce(`*`, lapply(source_cols, function(col) data[[col]]))
    },
    multiply_by_constant = {
      if (length(source_cols) != 1 || !is.finite(predictor_cfg$constant)) {
        stop(
          paste0(
            "Predictor '", predictor_key,
            "' requires exactly one source column and a finite constant for derivation_method='multiply_by_constant'."
          ),
          call. = FALSE
        )
      }
      data[[source_cols[[1]]]] * predictor_cfg$constant
    },
    divide_by_constant = {
      if (length(source_cols) != 1 || !is.finite(predictor_cfg$constant) || predictor_cfg$constant == 0) {
        stop(
          paste0(
            "Predictor '", predictor_key,
            "' requires exactly one source column and a non-zero finite constant for derivation_method='divide_by_constant'."
          ),
          call. = FALSE
        )
      }
      data[[source_cols[[1]]]] / predictor_cfg$constant
    },
    stop(
      paste0("Unsupported derivation_method '", method, "' for predictor '", predictor_key, "'."),
      call. = FALSE
    )
  )

  list(column_name = target_col, values = result)
}

parse_numeric_rule_clause <- function(clause_text) {
  matches <- stringr::str_match(
    clause_text,
    "^\\s*(<=|>=|<|>)\\s*(-?\\d*\\.?\\d+(?:[eE][+-]?\\d+)?)\\s*$"
  )

  if (all(is.na(matches))) {
    stop(
      paste0(
        "Invalid continuous rule clause '", clause_text,
        "'. Use clauses like <= 1, > 1, or > 1 & <= 5."
      ),
      call. = FALSE
    )
  }

  list(
    operator = matches[[2]],
    threshold = as.numeric(matches[[3]])
  )
}

evaluate_numeric_rule <- function(values, rule_expression) {
  clauses <- compact_chr(strsplit(rule_expression, "&", fixed = TRUE)[[1]])
  if (length(clauses) == 0) {
    stop("Continuous grouping rules cannot be blank.", call. = FALSE)
  }

  mask <- rep(TRUE, length(values))
  for (clause in clauses) {
    parsed <- parse_numeric_rule_clause(clause)
    clause_mask <- switch(
      parsed$operator,
      "<" = values < parsed$threshold,
      "<=" = values <= parsed$threshold,
      ">" = values > parsed$threshold,
      ">=" = values >= parsed$threshold
    )
    mask <- mask & clause_mask
  }

  mask & !is.na(values)
}

materialize_categorical_custom_group <- function(source_values, sc, strat_key) {
  defs <- sc$group_definitions %||% list()
  labels <- vapply(defs, `[[`, character(1), "group_label")
  assignments <- purrr::map(defs, "source_values")

  if (any(lengths(assignments) == 0)) {
    stop(
      paste0("Categorical custom grouping '", strat_key, "' contains an empty source_values mapping."),
      call. = FALSE
    )
  }

  duplicates <- duplicated(unlist(assignments, use.names = FALSE))
  if (any(duplicates)) {
    dup_values <- unique(unlist(assignments, use.names = FALSE)[duplicates])
    stop(
      paste0(
        "Categorical custom grouping '", strat_key,
        "' assigns source values to multiple groups: ", paste(dup_values, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  observed <- sort(unique(as.character(source_values[!is.na(source_values)])))
  missing_values <- setdiff(observed, unlist(assignments, use.names = FALSE))
  if (length(missing_values) > 0) {
    stop(
      paste0(
        "Categorical custom grouping '", strat_key,
        "' does not assign these observed values: ", paste(missing_values, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  out <- rep(NA_character_, length(source_values))
  for (idx in seq_along(defs)) {
    def <- defs[[idx]]
    out[as.character(source_values) %in% def$source_values] <- labels[[idx]]
  }

  factor(out, levels = sc$levels %||% labels)
}

materialize_continuous_custom_group <- function(source_values, sc, strat_key) {
  numeric_values <- suppressWarnings(as.numeric(source_values))
  bad_rows <- which(!is.na(source_values) & is.na(numeric_values))
  if (length(bad_rows) > 0) {
    stop(
      paste0(
        "Continuous custom grouping '", strat_key,
        "' references non-numeric data in column '", sc$source_column %||% sc$column_name, "'."
      ),
      call. = FALSE
    )
  }

  defs <- sc$group_definitions %||% list()
  labels <- vapply(defs, `[[`, character(1), "group_label")
  match_matrix <- sapply(defs, function(def) {
    rule_expression <- def$rule_expression %||% NA_character_
    if (is.na(rule_expression) || !nzchar(rule_expression)) {
      stop(
        paste0("Continuous custom grouping '", strat_key, "' has a blank rule_expression."),
        call. = FALSE
      )
    }
    evaluate_numeric_rule(numeric_values, rule_expression)
  })

  if (is.null(dim(match_matrix))) {
    match_matrix <- matrix(match_matrix, ncol = 1)
  }

  valid_rows <- !is.na(numeric_values)
  match_count <- rowSums(match_matrix, na.rm = TRUE)
  bad_rows <- which(valid_rows & match_count != 1)

  if (length(bad_rows) > 0) {
    bad_values <- unique(numeric_values[bad_rows])
    bad_values <- bad_values[seq_len(min(length(bad_values), 5))]
    problem_type <- if (any(match_count[bad_rows] == 0)) "unmatched" else "overlapping"
    stop(
      paste0(
        "Continuous custom grouping '", strat_key,
        "' has ", problem_type, " rules for observed values: ",
        paste(bad_values, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  out <- rep(NA_character_, length(numeric_values))
  for (idx in seq_along(defs)) {
    out[match_matrix[, idx]] <- labels[[idx]]
  }

  factor(out, levels = sc$levels %||% labels)
}

materialize_custom_stratifications <- function(data, strat_config) {
  custom_keys <- names(purrr::keep(strat_config, ~ isTRUE(.x$is_custom_grouping)))

  if (length(custom_keys) == 0) {
    return(data)
  }

  for (strat_key in custom_keys) {
    sc <- strat_config[[strat_key]]
    source_col <- sc$source_column %||% NA_character_
    target_col <- sc$column_name %||% strat_key

    if (!source_col %in% names(data)) {
      stop(
        paste0("Custom grouping '", strat_key, "' references missing source column '", source_col, "'."),
        call. = FALSE
      )
    }

    data[[target_col]] <- if (identical(sc$source_data_type %||% "categorical", "continuous")) {
      materialize_continuous_custom_group(data[[source_col]], sc, strat_key)
    } else {
      materialize_categorical_custom_group(data[[source_col]], sc, strat_key)
    }

    cli::cli_alert_success(
      "Materialized custom stratification {.var {target_col}} from {.var {source_col}}"
    )
  }

  data
}

#' Derive computed variables and apply factor recoding
#'
#' @param data Cleaned tibble from clean_data()
#' @param factor_recode_config Parsed factor recode config
#' @param predictor_config Parsed predictor config
#' @param strat_config Parsed stratification config
#' @return Tibble with derived variables, recoded factors, and custom strata
derive_variables <- function(data,
                             factor_recode_config,
                             predictor_config,
                             strat_config = list()) {

  cli::cli_alert_info("Deriving variables and recoding factors...")

  derived_msgs <- character(0)
  for (predictor_key in names(predictor_config)) {
    predictor_cfg <- predictor_config[[predictor_key]]
    derivation <- evaluate_derivation_method(data, predictor_key, predictor_cfg)
    if (is.null(derivation)) {
      next
    }

    data[[derivation$column_name]] <- derivation$values
    derived_msgs <- c(derived_msgs, derivation$column_name)
  }

  if (length(derived_msgs) > 0) {
    cli::cli_alert_success("Derived: {paste(derived_msgs, collapse = ', ')}")
  }

  for (recode_name in names(factor_recode_config)) {
    recode <- factor_recode_config[[recode_name]]
    src_col <- recode$source_column
    tgt_col <- recode$target_column

    if (!src_col %in% names(data)) {
      cli::cli_alert_warning("Source column {.var {src_col}} not found for {recode_name}, skipping")
      next
    }

    if (!is.factor(data[[src_col]])) {
      data[[src_col]] <- factor(data[[src_col]])
    }

    data[[tgt_col]] <- forcats::fct_collapse(data[[src_col]], !!!recode$collapse_map)

    n_levels <- length(levels(data[[tgt_col]]))
    cli::cli_alert_success(
      "Recoded {.var {src_col}} -> {.var {tgt_col}} ({n_levels} levels: {paste(levels(data[[tgt_col]]), collapse = ', ')})"
    )
  }

  data <- materialize_custom_stratifications(data, strat_config)

  derived_cols <- unique(c(
    derived_msgs,
    purrr::map_chr(purrr::keep(strat_config, ~ isTRUE(.x$is_custom_grouping)), ~ .x$column_name %||% NA_character_)
  ))

  for (col in stats::na.omit(derived_cols)) {
    if (col %in% names(data)) {
      n_na <- sum(is.na(data[[col]]))
      if (n_na > 0) {
        cli::cli_alert_warning("Derived column {.var {col}} has {n_na} NAs")
      }
    }
  }

  cli::cli_alert_success("Variable derivation complete: {ncol(data)} total columns")

  data
}
