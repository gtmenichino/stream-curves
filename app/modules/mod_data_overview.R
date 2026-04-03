## ── Module: Data Overview ─────────────────────────────────────────────────────
## Top control layout: Data & Setup above workbook metadata,
## followed by collapsible validation and data panels.

library(shiny)
library(bslib)
library(DT)

data_setup_intro_card <- function() {
  card(
    class = "border-primary mb-3 data-setup-intro-card",
    card_header(
      class = "bg-primary text-white",
      tags$span("StreamCurves")
    ),
    card_body(
      p("This application guides you through a structured, 4-phase evaluation
         of stratification variables and reference curve development for geomorphic
         metrics. Upload your own dataset to begin the workflow and run
         the analysis."),
      p("For each metric, you will explore candidate stratifications, verify
         consistency across metrics, confirm your selection, then build and
         finalize reference curves scored on a 0-1 scale.")
    )
  )
}

upload_format_tooltip_content <- function() {
  div(
    class = "upload-format-tooltip",
    div(
      class = "upload-format-row",
      tags$strong("File type:"),
      tags$span("XLSX workbook.")
    ),
    div(
      class = "upload-format-row",
      tags$strong("Structure:"),
      tags$span("Separate sheets for data, metrics, stratifications, predictors, and recodes.")
    ),
    div(
      class = "upload-format-row",
      tags$strong("Custom strata:"),
      tags$span("Categorical and continuous grouping rules are materialized at import.")
    ),
    div(
      class = "upload-format-row",
      tags$strong("Runtime config:"),
      tags$span("Workbook metadata replaces the old YAML registries.")
    )
  )
}

session_tooltip_content <- function() {
  div(
    class = "upload-format-tooltip",
    div(
      class = "upload-format-row",
      tags$strong("File type:"),
      tags$span("RDS StreamCurves session snapshot.")
    ),
    div(
      class = "upload-format-row",
      tags$strong("Includes:"),
      tags$span("Current derived analysis data, workbook tables/metadata, site masks, current app selections/settings, saved results/caches, reference-curve choices/results, and decision history.")
    ),
    div(
      class = "upload-format-row",
      tags$strong("Original workbook:"),
      tags$span("Stores the workbook tables and upload filename, but not the original .xlsx file itself as an attached binary.")
    ),
    div(
      class = "upload-format-row",
      tags$strong("Restore:"),
      tags$span("Uploading the .rds restores the saved workspace state.")
    )
  )
}

save_session_ui <- function(ns, app_data_loaded = FALSE) {
  download_session_button <- if (isTRUE(app_data_loaded)) {
    htmltools::tagAppendAttributes(
      downloadButton(
        ns("download_session"),
        label = "Download Session",
        class = "btn btn-outline-primary btn-sm save-session-download-btn"
      ),
      title = "Download Session",
      `aria-label` = "Download Session"
    )
  } else {
    htmltools::tagAppendAttributes(
      tags$button(
        type = "button",
        class = "btn btn-outline-secondary btn-sm save-session-download-btn",
        disabled = "disabled",
        "Download Session"
      ),
      title = "Load a workbook or session to enable session download.",
      `aria-label` = "Download Session disabled"
    )
  }

  tagList(
    div(
      class = "data-upload-label save-session-title",
      tags$span(class = "data-setup-panel-title save-session-title-text", "Save Session"),
      bslib::tooltip(
        trigger = tags$button(
          type = "button",
          class = "upload-format-help",
          `aria-label` = "What a saved session includes",
          "i"
        ),
        session_tooltip_content(),
        placement = "right"
      )
    ),
    div(
      class = "save-session-row",
      div(
        class = "save-session-name",
        textInput(ns("session_name"), NULL, placeholder = "Session name...", width = "100%")
      ),
      div(
        class = "save-session-download",
        download_session_button
      )
    ),
    if (!isTRUE(app_data_loaded)) {
      div(
        class = "save-session-help text-muted small",
        "Load a workbook or session to enable session download."
      )
    }
  )
}

upload_session_ui <- function(ns) {
  tagList(
    div(class = "data-setup-panel-title", "Upload Session"),
    fileInput(
      ns("load_session_file"),
      label = NULL,
      accept = c(".rds"),
      buttonLabel = "Choose RDS",
      placeholder = "No file selected"
    )
  )
}

legacy_sidebar_mod_data_overview_ui_unused <- function(id) {
  ns <- NS(id)

  div(
    class = "data-setup-shell",
    layout_sidebar(
      height = "calc(100vh - var(--app-navbar-offset, 4.5rem))",
      sidebar = sidebar(
        title = "Data & Setup",
        width = 320,
        open = list(desktop = "always", mobile = "closed"),
        fillable = TRUE,
        class = "data-setup-sidebar",

      ## ── Data Source ──────────────────────────────────────────────────────
      div(
        class = "data-upload-label",
        tags$span("Upload Workbook (.xlsx)"),
        bslib::tooltip(
          trigger = tags$button(
            type = "button",
            class = "upload-format-help",
            `aria-label` = "Expected file format",
            "?"
          ),
          upload_format_tooltip_content(),
          placement = "right"
        )
      ),
      fileInput(
        ns("upload_file"),
        label = NULL,
        accept = c(".xlsx"),
        buttonLabel = "Choose File",
        placeholder = "No file selected"
      ),
      uiOutput(ns("upload_status")),
      uiOutput(ns("reset_analysis_button")),
      hr(),

      ## ── Session Management ──────────────────────────────────────────────
      tagList(save_session_ui(ns), upload_session_ui(ns))
    ),

    ## ── Main Content ──────────────────────────────────────────────────────
      uiOutput(ns("main_content"))
    )
  )
}

data_setup_controls_card <- function(ns, app_data_loaded = FALSE) {
  card(
    class = "border-primary mb-3 data-setup-controls-card",
    card_header(
      class = "data-setup-card-header",
      tags$span("Data & Setup"),
      uiOutput(ns("reset_analysis_button"))
    ),
    card_body(
      div(
        class = "row g-2",
        div(
          class = "col-12 col-lg-4",
          div(
            class = "data-setup-control-panel",
            div(
              class = "data-upload-label",
              tags$span("Upload Workbook (.xlsx)"),
              bslib::tooltip(
                trigger = tags$button(
                  type = "button",
                  class = "upload-format-help",
                  `aria-label` = "Expected file format",
                  "?"
                ),
                upload_format_tooltip_content(),
                placement = "right"
              )
            ),
            fileInput(
              ns("upload_file"),
              label = NULL,
              accept = c(".xlsx"),
              buttonLabel = "Choose File",
              placeholder = "No file selected"
            ),
            uiOutput(ns("upload_status"))
          )
        ),
        div(
          class = "col-12 col-lg-4",
          div(
            class = "data-setup-control-panel session-controls",
            save_session_ui(ns, app_data_loaded = app_data_loaded)
          )
        ),
        div(
          class = "col-12 col-lg-4",
          div(
            class = "data-setup-control-panel session-controls",
            upload_session_ui(ns)
          )
        )
      )
    )
  )
}

mod_data_overview_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "data-setup-shell",
    uiOutput(ns("main_content"))
  )
}

filter_removed_strat_rows <- function(df, removed_key) {
  if (is.null(df) || !is.data.frame(df) || !("stratification" %in% names(df))) {
    return(df)
  }

  df |>
    dplyr::filter(.data$stratification != removed_key)
}

sanitize_removed_stratification_state <- function(rv, removed_key) {
  make_none_decision <- function(metric) {
    tibble::tibble(
      metric = metric,
      decision_type = "none",
      selected_strat = NA_character_,
      selected_p_value = NA_real_,
      selected_n_groups = NA_integer_,
      selected_min_n = NA_integer_,
      runner_up_strat = NA_character_,
      runner_up_p_value = NA_real_,
      needs_review = TRUE,
      review_reason = paste0("Restored session selection removed: ", removed_key),
      notes = "Selection cleared because the stratification is no longer supported."
    )
  }

  sanitize_verification <- function(verification) {
    if (is.null(verification)) return(NULL)

    verification$finalists <- setdiff(verification$finalists %||% character(0), removed_key)

    status <- verification$verification_status %||% list()
    if (length(status) > 0 && !is.null(names(status))) {
      verification$verification_status <- status[!(names(status) %in% removed_key)]
    }

    if (identical(verification$selected_strat %||% "none", removed_key)) {
      verification$selected_strat <- "none"
    }

    verification
  }

  sanitize_screening <- function(screening) {
    if (is.null(screening)) return(NULL)

    screening$results <- filter_removed_strat_rows(screening$results, removed_key)
    screening$pairwise <- filter_removed_strat_rows(screening$pairwise, removed_key)
    if (!is.null(screening$plots)) {
      screening$plots[[removed_key]] <- NULL
    }
    screening
  }

  if (removed_key %in% names(rv$data %||% list())) {
    rv$data[[removed_key]] <- NULL
  }

  rv$strat_config[[removed_key]] <- NULL
  rv$factor_recode_config[[removed_key]] <- NULL

  for (metric in names(rv$metric_config)) {
    allowed <- rv$metric_config[[metric]]$allowed_stratifications %||% character(0)
    rv$metric_config[[metric]]$allowed_stratifications <- setdiff(allowed, removed_key)
  }

  if (!is.null(rv$decision_log) && nrow(rv$decision_log) > 0) {
    if ("selected_strat" %in% names(rv$decision_log)) {
      rv$decision_log$selected_strat[rv$decision_log$selected_strat == removed_key] <- NA_character_
    }
    if ("auto_recommended" %in% names(rv$decision_log)) {
      rv$decision_log$auto_recommended[rv$decision_log$auto_recommended == removed_key] <- NA_character_
    }
  }

  rv$phase2_settings$strat_filter <- setdiff(rv$phase2_settings$strat_filter %||% character(0), removed_key)
  rv$phase2_ranking <- filter_removed_strat_rows(rv$phase2_ranking, removed_key)
  rv$cross_metric_consistency <- NULL

  for (metric in names(rv$phase1_candidates)) {
    rv$phase1_candidates[[metric]] <- filter_removed_strat_rows(rv$phase1_candidates[[metric]], removed_key)
  }
  for (metric in names(rv$all_layer1_results)) {
    rv$all_layer1_results[[metric]] <- filter_removed_strat_rows(rv$all_layer1_results[[metric]], removed_key)
  }
  for (metric in names(rv$all_layer2_results)) {
    rv$all_layer2_results[[metric]] <- filter_removed_strat_rows(rv$all_layer2_results[[metric]], removed_key)
  }
  for (metric in names(rv$phase3_verification)) {
    rv$phase3_verification[[metric]] <- sanitize_verification(rv$phase3_verification[[metric]])
  }

  completed_with_removed <- character(0)

  for (metric in names(rv$metric_phase_cache)) {
    cache <- rv$metric_phase_cache[[metric]]
    if (is.null(cache)) next

    cache$phase1_screening <- sanitize_screening(cache$phase1_screening %||% NULL)
    cache$phase1_effect_sizes <- filter_removed_strat_rows(cache$phase1_effect_sizes %||% NULL, removed_key)

    if (!is.null(cache$strat_decision_user) &&
        nrow(cache$strat_decision_user) > 0 &&
        identical(cache$strat_decision_user$selected_strat[1] %||% NA_character_, removed_key)) {
      cache$strat_decision_user <- make_none_decision(metric)
      cache$reference_curve <- NULL
      cache$current_stratum_level <- NULL
      cache$phase4_data <- NULL
      cache$stratum_results <- NULL
    }

    rv$metric_phase_cache[[metric]] <- cache
  }

  for (metric in names(rv$completed_metrics)) {
    entry <- rv$completed_metrics[[metric]]
    if (!is.null(entry$strat_decision) &&
        nrow(entry$strat_decision) > 0 &&
        identical(entry$strat_decision$selected_strat[1] %||% NA_character_, removed_key)) {
      completed_with_removed <- c(completed_with_removed, metric)
    }
  }

  if (!is.null(rv$strat_decision_user) &&
      nrow(rv$strat_decision_user) > 0 &&
      identical(rv$strat_decision_user$selected_strat[1] %||% NA_character_, removed_key)) {
    rv$strat_decision_user <- make_none_decision(rv$current_metric %||% NA_character_)
  }

  for (metric in completed_with_removed) {
    clear_metric_phase4_results(rv, metric)
    rv$phase3_verification[[metric]] <- sanitize_verification(rv$phase3_verification[[metric]])
    if (is.null(rv$metric_phase_cache[[metric]])) {
      rv$metric_phase_cache[[metric]] <- list()
    }
    rv$metric_phase_cache[[metric]]$strat_decision_user <- make_none_decision(metric)
  }

  invisible(NULL)
}

metadata_editor_columns <- function() {
  c(
    workbook_sheet_columns()[["metrics"]],
    "allowed_predictors",
    "allowed_stratifications"
  )
}

metadata_table_to_editor_df <- function(df, sheet_name) {
  df <- ensure_workbook_sheet_columns(df, sheet_name)
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  as.data.frame(lapply(df, function(col) {
    out <- as.character(col)
    out[is.na(out)] <- ""
    out
  }), stringsAsFactors = FALSE)
}

collapse_pipe_text <- function(values) {
  vals <- compact_chr(values)
  if (length(vals) == 0) {
    return("")
  }
  paste(vals, collapse = "|")
}

collapse_pairwise_text <- function(levels) {
  levels <- compact_chr(levels)
  if (length(levels) < 2) {
    return("")
  }

  pairs <- auto_pairwise_values(levels)
  paste(vapply(pairs, function(pair) paste(pair, collapse = "~"), character(1)), collapse = "|")
}

metric_link_text <- function(link_df, metric_key, value_col) {
  if (nrow(link_df) == 0) {
    return("")
  }

  rows <- link_df[link_df$metric_key %in% metric_key, , drop = FALSE]
  if (nrow(rows) == 0) {
    return("")
  }

  collapse_pipe_text(rows[[value_col]])
}

build_metrics_editor_df <- function(tables) {
  metrics_df <- metadata_table_to_editor_df(tables$metrics, "metrics")
  metric_predictors_df <- metadata_table_to_editor_df(tables$metric_predictors, "metric_predictors")
  metric_stratifications_df <- metadata_table_to_editor_df(tables$metric_stratifications, "metric_stratifications")

  if (!("metric_key" %in% names(metrics_df))) {
    metrics_df$metric_key <- character(0)
  }

  metrics_df$allowed_predictors <- vapply(
    metrics_df$metric_key,
    function(metric_key) metric_link_text(metric_predictors_df, metric_key, "predictor_key"),
    character(1)
  )
  metrics_df$allowed_stratifications <- vapply(
    metrics_df$metric_key,
    function(metric_key) metric_link_text(metric_stratifications_df, metric_key, "strat_key"),
    character(1)
  )

  missing_cols <- setdiff(metadata_editor_columns(), names(metrics_df))
  for (col_name in missing_cols) {
    metrics_df[[col_name]] <- ""
  }

  metrics_df[, metadata_editor_columns(), drop = FALSE]
}

apply_metrics_editor_df <- function(tables, editor_df) {
  editor_df <- as.data.frame(editor_df, stringsAsFactors = FALSE)
  editor_df <- metadata_table_to_editor_df(editor_df, "metrics")
  missing_cols <- setdiff(metadata_editor_columns(), names(editor_df))
  for (col_name in missing_cols) {
    editor_df[[col_name]] <- ""
  }

  metric_cols <- workbook_sheet_columns()[["metrics"]]
  metrics_df <- editor_df[, metric_cols, drop = FALSE]

  metric_predictors_df <- purrr::map_dfr(seq_len(nrow(editor_df)), function(idx) {
    metric_key <- editor_df$metric_key[[idx]]
    predictor_keys <- parse_pipe_values(editor_df$allowed_predictors[[idx]])
    if (!nzchar(metric_key) || length(predictor_keys) == 0) {
      return(tibble::tibble())
    }

    tibble::tibble(
      metric_key = metric_key,
      predictor_key = predictor_keys,
      sort_order = seq_along(predictor_keys)
    )
  })

  metric_stratifications_df <- purrr::map_dfr(seq_len(nrow(editor_df)), function(idx) {
    metric_key <- editor_df$metric_key[[idx]]
    strat_keys <- parse_pipe_values(editor_df$allowed_stratifications[[idx]])
    if (!nzchar(metric_key) || length(strat_keys) == 0) {
      return(tibble::tibble())
    }

    tibble::tibble(
      metric_key = metric_key,
      strat_key = strat_keys,
      sort_order = seq_along(strat_keys)
    )
  })

  tables$metrics <- ensure_workbook_sheet_columns(metrics_df, "metrics")
  tables$metric_predictors <- ensure_workbook_sheet_columns(metric_predictors_df, "metric_predictors")
  tables$metric_stratifications <- ensure_workbook_sheet_columns(metric_stratifications_df, "metric_stratifications")
  tables
}

editor_df_for_tab <- function(tables, tab_key) {
  switch(
    tab_key,
    metrics = build_metrics_editor_df(tables),
    stratifications = metadata_table_to_editor_df(tables$stratifications, "stratifications"),
    predictors = metadata_table_to_editor_df(tables$predictors, "predictors"),
    factor_recodes = metadata_table_to_editor_df(tables$factor_recodes, "factor_recodes"),
    custom_groups = metadata_table_to_editor_df(tables$strat_groups, "strat_groups"),
    site_masks = metadata_table_to_editor_df(tables$site_masks, "site_masks"),
    stop(paste0("Unsupported metadata tab: ", tab_key), call. = FALSE)
  )
}

apply_editor_df_to_tables <- function(tables, tab_key, editor_df) {
  switch(
    tab_key,
    metrics = apply_metrics_editor_df(tables, editor_df),
    stratifications = {
      tables$stratifications <- ensure_workbook_sheet_columns(editor_df, "stratifications")
      tables
    },
    predictors = {
      tables$predictors <- ensure_workbook_sheet_columns(editor_df, "predictors")
      tables
    },
    factor_recodes = {
      tables$factor_recodes <- ensure_workbook_sheet_columns(editor_df, "factor_recodes")
      tables
    },
    custom_groups = {
      tables$strat_groups <- ensure_workbook_sheet_columns(editor_df, "strat_groups")
      tables
    },
    site_masks = {
      tables$site_masks <- ensure_workbook_sheet_columns(editor_df, "site_masks")
      tables
    },
    stop(paste0("Unsupported metadata tab: ", tab_key), call. = FALSE)
  )
}

next_unique_key <- function(existing_keys, prefix) {
  existing_keys <- compact_chr(existing_keys)
  idx <- 1L
  repeat {
    candidate <- paste0(prefix, idx)
    if (!(candidate %in% existing_keys)) {
      return(candidate)
    }
    idx <- idx + 1L
  }
}

first_numeric_column_name <- function(data) {
  if (is.null(data) || !ncol(data)) {
    return("")
  }
  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  if (length(numeric_cols) > 0) {
    return(numeric_cols[[1]])
  }
  if (length(names(data)) > 0) {
    return(names(data)[[1]])
  }
  ""
}

first_categorical_column_name <- function(data) {
  if (is.null(data) || !ncol(data)) {
    return("")
  }

  categorical_mask <- vapply(data, function(col) {
    is.factor(col) || is.character(col) || is.logical(col)
  }, logical(1))
  categorical_cols <- names(data)[categorical_mask]
  if (length(categorical_cols) > 0) {
    return(categorical_cols[[1]])
  }
  if (length(names(data)) > 0) {
    return(names(data)[[1]])
  }
  ""
}

observed_values_for_column <- function(data, column_name) {
  if (!nzchar(column_name) || is.null(data) || !(column_name %in% names(data))) {
    return(character(0))
  }

  vals <- data[[column_name]]
  vals <- as.character(vals[!is.na(vals)])
  sort(unique(vals[nzchar(trimws(vals))]))
}

site_mask_label_column_from_tables <- function(tables) {
  settings_df <- ensure_workbook_sheet_columns(tables$site_mask_settings, "site_mask_settings")
  label_values <- compact_chr(settings_df$site_label_column)
  label_column <- if (length(label_values) > 0) label_values[[1]] else NA_character_
  data <- tables$data %||% data.frame()

  if (is.na(label_column) || !nzchar(label_column) || !(label_column %in% names(data))) {
    return(default_site_label_source_column(data))
  }

  label_column
}

site_mask_labels_for_ids <- function(tables, site_ids, label_column = site_mask_label_column_from_tables(tables)) {
  data <- tables$data %||% data.frame()
  site_ids <- sort(unique(as.integer(site_ids %||% integer(0))))
  site_ids <- site_ids[!is.na(site_ids) & site_ids > 0L & site_ids <= nrow(data)]

  if (length(site_ids) == 0) {
    return(character(0))
  }

  resolve_site_label_values(data, site_ids, label_column)
}

apply_site_mask_selection_to_tables <- function(tables, site_ids, label_column = NULL) {
  data <- tables$data %||% data.frame()
  resolved_label_column <- label_column %||% site_mask_label_column_from_tables(tables)
  if (!resolved_label_column %in% names(data)) {
    stop(
      paste0("Site label column '", resolved_label_column, "' was not found in the data sheet."),
      call. = FALSE
    )
  }

  site_ids <- sort(unique(as.integer(site_ids %||% integer(0))))
  site_ids <- site_ids[!is.na(site_ids)]
  if (length(site_ids) > 0 && any(site_ids <= 0L | site_ids > nrow(data))) {
    stop("Selected masked site ids must be within the workbook data row range.", call. = FALSE)
  }

  site_labels <- site_mask_labels_for_ids(tables, site_ids, resolved_label_column)
  tables$site_masks <- ensure_workbook_sheet_columns(
    tibble::tibble(
      masked_sites = site_ids,
      site_label = site_labels
    ),
    "site_masks"
  )
  tables$site_mask_settings <- ensure_workbook_sheet_columns(
    tibble::tibble(site_label_column = resolved_label_column),
    "site_mask_settings"
  )

  tables
}

site_mask_choice_vector <- function(tables, label_column = site_mask_label_column_from_tables(tables)) {
  data <- tables$data %||% data.frame()
  if (nrow(data) == 0) {
    return(character(0))
  }

  site_ids <- seq_len(nrow(data))
  site_labels <- site_mask_labels_for_ids(tables, site_ids, label_column)
  stats::setNames(as.character(site_ids), paste0(site_ids, " - ", site_labels))
}

expanded_removed_strat_keys <- function(stratifications_df, removed_keys) {
  removed_keys <- compact_chr(removed_keys)
  if (length(removed_keys) == 0) {
    return(character(0))
  }

  stratifications_df <- metadata_table_to_editor_df(stratifications_df, "stratifications")

  repeat {
    dependent_keys <- stratifications_df |>
      dplyr::filter(
        .data$strat_key %in% removed_keys |
          (.data$strat_type == "paired" &
             (.data$primary_strat_key %in% removed_keys |
                .data$secondary_strat_key %in% removed_keys))
      ) |>
      dplyr::pull(.data$strat_key) |>
      compact_chr()

    updated <- unique(c(removed_keys, dependent_keys))
    if (length(updated) == length(removed_keys)) {
      return(updated)
    }
    removed_keys <- updated
  }
}

add_metric_row_to_tables <- function(tables) {
  metrics_df <- build_metrics_editor_df(tables)
  data <- tables$data %||% data.frame()
  new_key <- next_unique_key(metrics_df$metric_key, "new_metric_")
  metric_col <- first_numeric_column_name(data)

  new_row <- as.data.frame(as.list(stats::setNames(rep("", length(metadata_editor_columns())), metadata_editor_columns())), stringsAsFactors = FALSE)
  new_row$metric_key <- new_key
  new_row$display_name <- new_key
  new_row$column_name <- metric_col
  new_row$metric_family <- "continuous"
  new_row$higher_is_better <- "TRUE"
  new_row$monotonic_linear <- "TRUE"
  new_row$preferred_transform <- "none"
  new_row$min_sample_size <- "10"
  new_row$best_subsets_allowed <- "TRUE"
  new_row$count_model <- "FALSE"
  new_row$stratification_mode <- "subset"
  new_row$include_in_summary <- "TRUE"

  apply_metrics_editor_df(tables, rbind(metrics_df, new_row))
}

add_stratification_row_to_tables <- function(tables) {
  stratifications_df <- metadata_table_to_editor_df(tables$stratifications, "stratifications")
  data <- tables$data %||% data.frame()
  new_key <- next_unique_key(stratifications_df$strat_key, "new_strat_")
  source_column <- first_categorical_column_name(data)

  new_row <- as.data.frame(as.list(stats::setNames(rep("", length(workbook_sheet_columns()[["stratifications"]])), workbook_sheet_columns()[["stratifications"]])), stringsAsFactors = FALSE)
  new_row$strat_key <- new_key
  new_row$display_name <- new_key
  new_row$strat_type <- "raw_single"
  new_row$source_column <- source_column
  new_row$source_data_type <- if (nzchar(source_column) && source_column %in% names(data) && is.numeric(data[[source_column]])) "continuous" else "categorical"
  new_row$min_group_size <- "5"

  tables$stratifications <- ensure_workbook_sheet_columns(rbind(stratifications_df, new_row), "stratifications")
  tables
}

add_predictor_row_to_tables <- function(tables) {
  predictors_df <- metadata_table_to_editor_df(tables$predictors, "predictors")
  data <- tables$data %||% data.frame()
  new_key <- next_unique_key(predictors_df$predictor_key, "new_predictor_")
  column_name <- first_numeric_column_name(data)

  new_row <- as.data.frame(as.list(stats::setNames(rep("", length(workbook_sheet_columns()[["predictors"]])), workbook_sheet_columns()[["predictors"]])), stringsAsFactors = FALSE)
  new_row$predictor_key <- new_key
  new_row$display_name <- new_key
  new_row$column_name <- column_name
  new_row$type <- "continuous"
  new_row$derived <- "FALSE"
  new_row$derivation_method <- "none"
  new_row$missing_data_rule <- "error"

  tables$predictors <- ensure_workbook_sheet_columns(rbind(predictors_df, new_row), "predictors")
  tables
}

add_factor_recode_row_to_tables <- function(tables) {
  factor_recodes_df <- metadata_table_to_editor_df(tables$factor_recodes, "factor_recodes")
  data <- tables$data %||% data.frame()
  new_key <- next_unique_key(factor_recodes_df$recode_key, "new_recode_")
  source_column <- first_categorical_column_name(data)
  observed_values <- observed_values_for_column(data, source_column)

  new_row <- as.data.frame(as.list(stats::setNames(rep("", length(workbook_sheet_columns()[["factor_recodes"]])), workbook_sheet_columns()[["factor_recodes"]])), stringsAsFactors = FALSE)
  new_row$recode_key <- new_key
  new_row$source_column <- source_column
  new_row$target_column <- new_key
  new_row$target_level <- "Group1"
  new_row$source_values <- if (length(observed_values) > 0) observed_values[[1]] else ""

  tables$factor_recodes <- ensure_workbook_sheet_columns(rbind(factor_recodes_df, new_row), "factor_recodes")
  tables
}

add_custom_grouping_to_tables <- function(tables) {
  stratifications_df <- metadata_table_to_editor_df(tables$stratifications, "stratifications")
  strat_groups_df <- metadata_table_to_editor_df(tables$strat_groups, "strat_groups")
  data <- tables$data %||% data.frame()

  source_column <- first_categorical_column_name(data)
  source_data_type <- "categorical"

  if (!nzchar(source_column) || !(source_column %in% names(data))) {
    source_column <- first_numeric_column_name(data)
    source_data_type <- "continuous"
  }

  if (!nzchar(source_column) || !(source_column %in% names(data))) {
    stop("No source column is available to create a custom grouping.", call. = FALSE)
  }

  new_key <- next_unique_key(stratifications_df$strat_key, "custom_group_")
  derived_column_name <- new_key
  display_name <- new_key

  strat_row <- as.data.frame(as.list(stats::setNames(rep("", length(workbook_sheet_columns()[["stratifications"]])), workbook_sheet_columns()[["stratifications"]])), stringsAsFactors = FALSE)
  strat_row$strat_key <- new_key
  strat_row$display_name <- display_name
  strat_row$strat_type <- "custom_group"
  strat_row$source_column <- source_column
  strat_row$source_data_type <- source_data_type
  strat_row$derived_column_name <- derived_column_name
  strat_row$min_group_size <- "5"

  group_row <- as.data.frame(as.list(stats::setNames(rep("", length(workbook_sheet_columns()[["strat_groups"]])), workbook_sheet_columns()[["strat_groups"]])), stringsAsFactors = FALSE)
  group_row$strat_key <- new_key
  group_row$group_label <- "All"
  group_row$sort_order <- "1"

  if (identical(source_data_type, "continuous")) {
    numeric_values <- suppressWarnings(as.numeric(data[[source_column]]))
    numeric_values <- numeric_values[!is.na(numeric_values)]
    if (length(numeric_values) == 0) {
      stop(
        paste0("Column '", source_column, "' has no numeric values for a continuous custom grouping."),
        call. = FALSE
      )
    }

    min_value <- min(numeric_values)
    max_value <- max(numeric_values)
    group_row$rule_expression <- paste0(">= ", signif(min_value, 10), " & <= ", signif(max_value, 10))
    strat_row$levels <- "All"
  } else {
    observed_values <- observed_values_for_column(data, source_column)
    if (length(observed_values) == 0) {
      stop(
        paste0("Column '", source_column, "' has no observed values for a categorical custom grouping."),
        call. = FALSE
      )
    }

    group_row$source_values <- collapse_pipe_text(observed_values)
    strat_row$levels <- "All"
  }

  tables$stratifications <- ensure_workbook_sheet_columns(rbind(stratifications_df, strat_row), "stratifications")
  tables$strat_groups <- ensure_workbook_sheet_columns(rbind(strat_groups_df, group_row), "strat_groups")
  tables
}

delete_rows_from_tables <- function(tables, tab_key, selected_rows) {
  selected_rows <- sort(unique(as.integer(selected_rows)))
  if (length(selected_rows) == 0) {
    return(tables)
  }

  if (identical(tab_key, "metrics")) {
    editor_df <- build_metrics_editor_df(tables)
    removed_keys <- compact_chr(editor_df$metric_key[selected_rows])
    tables$metrics <- ensure_workbook_sheet_columns(editor_df[-selected_rows, setdiff(names(editor_df), c("allowed_predictors", "allowed_stratifications")), drop = FALSE], "metrics")
    tables$metric_predictors <- ensure_workbook_sheet_columns(
      metadata_table_to_editor_df(tables$metric_predictors, "metric_predictors") |>
        dplyr::filter(!(.data$metric_key %in% removed_keys)),
      "metric_predictors"
    )
    tables$metric_stratifications <- ensure_workbook_sheet_columns(
      metadata_table_to_editor_df(tables$metric_stratifications, "metric_stratifications") |>
        dplyr::filter(!(.data$metric_key %in% removed_keys)),
      "metric_stratifications"
    )
    return(tables)
  }

  if (identical(tab_key, "stratifications")) {
    editor_df <- metadata_table_to_editor_df(tables$stratifications, "stratifications")
    removed_keys <- expanded_removed_strat_keys(editor_df, editor_df$strat_key[selected_rows])
    tables$stratifications <- ensure_workbook_sheet_columns(
      editor_df |> dplyr::filter(!(.data$strat_key %in% removed_keys)),
      "stratifications"
    )
    tables$metric_stratifications <- ensure_workbook_sheet_columns(
      metadata_table_to_editor_df(tables$metric_stratifications, "metric_stratifications") |>
        dplyr::filter(!(.data$strat_key %in% removed_keys)),
      "metric_stratifications"
    )
    tables$strat_groups <- ensure_workbook_sheet_columns(
      metadata_table_to_editor_df(tables$strat_groups, "strat_groups") |>
        dplyr::filter(!(.data$strat_key %in% removed_keys)),
      "strat_groups"
    )
    return(tables)
  }

  if (identical(tab_key, "predictors")) {
    editor_df <- metadata_table_to_editor_df(tables$predictors, "predictors")
    removed_keys <- compact_chr(editor_df$predictor_key[selected_rows])
    tables$predictors <- ensure_workbook_sheet_columns(editor_df[-selected_rows, , drop = FALSE], "predictors")
    tables$metric_predictors <- ensure_workbook_sheet_columns(
      metadata_table_to_editor_df(tables$metric_predictors, "metric_predictors") |>
        dplyr::filter(!(.data$predictor_key %in% removed_keys)),
      "metric_predictors"
    )
    return(tables)
  }

  if (identical(tab_key, "factor_recodes")) {
    editor_df <- metadata_table_to_editor_df(tables$factor_recodes, "factor_recodes")
    keep_rows <- setdiff(seq_len(nrow(editor_df)), selected_rows)
    tables$factor_recodes <- ensure_workbook_sheet_columns(editor_df[keep_rows, , drop = FALSE], "factor_recodes")
    return(tables)
  }

  if (identical(tab_key, "custom_groups")) {
    editor_df <- metadata_table_to_editor_df(tables$strat_groups, "strat_groups")
    removed_parent_keys <- compact_chr(editor_df$strat_key[selected_rows])
    keep_rows <- setdiff(seq_len(nrow(editor_df)), selected_rows)
    remaining_groups <- editor_df[keep_rows, , drop = FALSE]

    orphan_keys <- setdiff(removed_parent_keys, compact_chr(remaining_groups$strat_key))
    if (length(orphan_keys) > 0) {
      removed_keys <- expanded_removed_strat_keys(tables$stratifications, orphan_keys)
      tables$stratifications <- ensure_workbook_sheet_columns(
        metadata_table_to_editor_df(tables$stratifications, "stratifications") |>
          dplyr::filter(!(.data$strat_key %in% removed_keys)),
        "stratifications"
      )
      tables$metric_stratifications <- ensure_workbook_sheet_columns(
        metadata_table_to_editor_df(tables$metric_stratifications, "metric_stratifications") |>
          dplyr::filter(!(.data$strat_key %in% removed_keys)),
        "metric_stratifications"
      )
      remaining_groups <- remaining_groups |>
        dplyr::filter(!(.data$strat_key %in% removed_keys))
    }

    tables$strat_groups <- ensure_workbook_sheet_columns(remaining_groups, "strat_groups")
    return(tables)
  }

  if (identical(tab_key, "site_masks")) {
    editor_df <- metadata_table_to_editor_df(tables$site_masks, "site_masks")
    keep_rows <- setdiff(seq_len(nrow(editor_df)), selected_rows)
    remaining_ids <- if (length(keep_rows) == 0) integer(0) else suppressWarnings(as.integer(editor_df$masked_sites[keep_rows]))
    return(apply_site_mask_selection_to_tables(
      tables,
      remaining_ids,
      site_mask_label_column_from_tables(tables)
    ))
  }

  stop(paste0("Unsupported metadata tab: ", tab_key), call. = FALSE)
}

metadata_tab_titles <- function() {
  c(
    metrics = "Metrics",
    stratifications = "Stratifications",
    predictors = "Predictors",
    factor_recodes = "Factor Recodes",
    custom_groups = "Custom Groups",
    site_masks = "Site Masks"
  )
}

metadata_table_stem <- function(tab_key) {
  switch(
    tab_key,
    metrics = "metrics",
    stratifications = "strat",
    predictors = "predictor",
    factor_recodes = "recode",
    custom_groups = "group",
    site_masks = "site_mask",
    stop(paste0("Unsupported metadata tab: ", tab_key), call. = FALSE)
  )
}

metadata_output_id <- function(tab_key, modal = FALSE) {
  paste0("metadata_", metadata_table_stem(tab_key), "_table", if (isTRUE(modal)) "_modal" else "")
}

metadata_selection_input_id <- function(tab_key, modal = FALSE) {
  paste0(metadata_output_id(tab_key, modal), "_rows_selected")
}

metadata_column_picker_input_id <- function(tab_key, modal = FALSE) {
  paste0("metadata_", metadata_table_stem(tab_key), "_visible_columns", if (isTRUE(modal)) "_modal" else "")
}

metadata_column_picker_ui_output_id <- function(tab_key, modal = FALSE) {
  paste0(metadata_column_picker_input_id(tab_key, modal), "_ui")
}

metadata_pretty_label <- function(column_name) {
  label <- gsub("_", " ", column_name, fixed = TRUE)
  tools::toTitleCase(label)
}

metadata_wrap_label <- function(label) {
  label <- gsub(" / ", "/<br>", label, fixed = TRUE)
  label <- gsub(" ", "<br>", label, fixed = TRUE)
  label
}

metadata_table_container <- function(column_labels) {
  htmltools::withTags(
    table(
      class = "display compact stripe cell-border metadata-editor-table",
      thead(
        tr(
          lapply(column_labels, function(label) {
            th(htmltools::HTML(label))
          })
        )
      )
    )
  )
}

metadata_display_df_for_tab <- function(tab_key, tables) {
  df <- editor_df_for_tab(tables, tab_key)
  if (nrow(df) == 0) {
    return(df)
  }

  display_df <- df
  display_df[] <- lapply(display_df, function(col) {
    out <- as.character(col)
    out[is.na(out)] <- ""
    out <- gsub("\\|", " | ", out)
    out
  })
  display_df
}

metadata_raw_data_columns <- function(tables) {
  names(tables$data %||% data.frame())
}

metadata_categorical_data_columns <- function(tables) {
  data <- tables$data %||% data.frame()
  if (!ncol(data)) {
    return(character(0))
  }

  names(data)[vapply(data, function(col) {
    is.factor(col) || is.character(col) || is.logical(col)
  }, logical(1))]
}

metadata_factor_recode_targets <- function(tables) {
  factor_recodes_df <- metadata_table_to_editor_df(tables$factor_recodes, "factor_recodes")
  unique(compact_chr(factor_recodes_df$target_column))
}

metadata_non_paired_strat_keys <- function(tables) {
  stratifications_df <- metadata_table_to_editor_df(tables$stratifications, "stratifications")
  unique(compact_chr(stratifications_df$strat_key[stratifications_df$strat_type != "paired"]))
}

metadata_custom_group_keys <- function(tables) {
  stratifications_df <- metadata_table_to_editor_df(tables$stratifications, "stratifications")
  unique(compact_chr(stratifications_df$strat_key[stratifications_df$strat_type == "custom_group"]))
}

metadata_observed_values <- function(tables, column_name) {
  observed_values_for_column(tables$data %||% data.frame(), column_name)
}

metadata_parent_strat_row <- function(tables, strat_key) {
  stratifications_df <- metadata_table_to_editor_df(tables$stratifications, "stratifications")
  matches <- stratifications_df[stratifications_df$strat_key %in% strat_key, , drop = FALSE]
  if (nrow(matches) == 0) {
    return(NULL)
  }
  matches[1, , drop = FALSE]
}

named_choice_vector <- function(values, blank_label = NULL) {
  values <- unique(as.character(values))
  values <- values[!is.na(values)]
  out <- stats::setNames(values, values)
  if (!is.null(blank_label)) {
    out <- c(stats::setNames("", blank_label), out)
  }
  out
}

metadata_field_spec <- function(tab_key, column_name, tables, row) {
  row <- row[1, , drop = FALSE]
  spec <- list(
    label = metadata_pretty_label(column_name),
    editor = "text",
    choices = NULL,
    multiple = FALSE,
    enabled = TRUE,
    rows = 2L
  )

  if (identical(column_name, "notes")) {
    spec$editor <- "textarea"
    spec$rows <- 3L
  }

  if (identical(tab_key, "metrics")) {
    if (column_name %in% c("column_name")) {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(metadata_raw_data_columns(tables))
    } else if (column_name %in% c("metric_family")) {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(c("continuous", "proportion", "count", "categorical"))
    } else if (column_name %in% c("higher_is_better", "monotonic_linear", "best_subsets_allowed", "count_model", "include_in_summary")) {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(c("TRUE", "FALSE"))
    } else if (column_name %in% c("preferred_transform")) {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(c("none", "log"))
    } else if (column_name %in% c("stratification_mode")) {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(c("covariate", "subset", "auto"))
    } else if (column_name %in% c("missing_data_rule")) {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(c("warn", "error"), blank_label = "(blank)")
    } else if (column_name %in% c("allowed_predictors")) {
      spec$label <- "Allowed Predictors"
      spec$editor <- "multiselect"
      spec$choices <- named_choice_vector(compact_chr(metadata_table_to_editor_df(tables$predictors, "predictors")$predictor_key))
      spec$multiple <- TRUE
    } else if (column_name %in% c("allowed_stratifications")) {
      spec$label <- "Allowed Stratifications"
      spec$editor <- "multiselect"
      spec$choices <- named_choice_vector(compact_chr(metadata_table_to_editor_df(tables$stratifications, "stratifications")$strat_key))
      spec$multiple <- TRUE
    }
  }

  if (identical(tab_key, "stratifications")) {
    strat_type <- row$strat_type[[1]] %||% ""
    if (column_name == "strat_type") {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(c("raw_single", "paired", "custom_group"))
    } else if (column_name == "source_column") {
      source_choices <- if (identical(strat_type, "custom_group")) {
        metadata_raw_data_columns(tables)
      } else {
        unique(c(metadata_raw_data_columns(tables), metadata_factor_recode_targets(tables)))
      }
      spec$editor <- "select"
      spec$choices <- named_choice_vector(source_choices, blank_label = "(blank)")
      spec$enabled <- !identical(strat_type, "paired")
    } else if (column_name == "source_data_type") {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(c("categorical", "continuous"))
      spec$enabled <- !identical(strat_type, "paired")
    } else if (column_name %in% c("primary_strat_key", "secondary_strat_key")) {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(metadata_non_paired_strat_keys(tables), blank_label = "(blank)")
      spec$enabled <- identical(strat_type, "paired")
    }
  }

  if (identical(tab_key, "predictors")) {
    is_derived <- identical(row$derived[[1]] %||% "", "TRUE")
    if (column_name == "column_name" && !is_derived) {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(metadata_raw_data_columns(tables))
    } else if (column_name == "type") {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(unique(c("continuous", "categorical", compact_chr(metadata_table_to_editor_df(tables$predictors, "predictors")$type))))
    } else if (column_name == "derived") {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(c("TRUE", "FALSE"))
    } else if (column_name == "derivation_method") {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(c("none", "sum", "multiply", "multiply_by_constant", "divide_by_constant"))
      spec$enabled <- is_derived
    } else if (column_name == "source_columns") {
      spec$editor <- "multiselect"
      spec$choices <- named_choice_vector(metadata_raw_data_columns(tables))
      spec$multiple <- TRUE
      spec$enabled <- is_derived
    } else if (column_name == "missing_data_rule") {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(c("error", "warn"))
    }
  }

  if (identical(tab_key, "factor_recodes")) {
    source_column <- row$source_column[[1]] %||% ""
    if (column_name == "source_column") {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(metadata_categorical_data_columns(tables), blank_label = "(blank)")
    } else if (column_name == "source_values") {
      spec$editor <- "multiselect"
      spec$choices <- named_choice_vector(metadata_observed_values(tables, source_column))
      spec$multiple <- TRUE
    }
  }

  if (identical(tab_key, "custom_groups")) {
    parent_row <- metadata_parent_strat_row(tables, row$strat_key[[1]] %||% "")
    source_type <- if (is.null(parent_row)) "" else parent_row$source_data_type[[1]] %||% ""
    source_column <- if (is.null(parent_row)) "" else parent_row$source_column[[1]] %||% ""

    if (column_name == "strat_key") {
      spec$editor <- "select"
      spec$choices <- named_choice_vector(metadata_custom_group_keys(tables), blank_label = "(blank)")
    } else if (column_name == "source_values") {
      spec$editor <- "multiselect"
      spec$choices <- named_choice_vector(metadata_observed_values(tables, source_column))
      spec$multiple <- TRUE
      spec$enabled <- identical(source_type, "categorical")
    } else if (column_name == "rule_expression") {
      spec$enabled <- identical(source_type, "continuous")
    }
  }

  spec
}

metadata_column_labels <- function(tab_key, tables) {
  display_df <- metadata_display_df_for_tab(tab_key, tables)
  columns <- names(display_df)
  sample_row <- if (length(columns) == 0) {
    data.frame()
  } else if (nrow(display_df) > 0) {
    display_df[1, , drop = FALSE]
  } else {
    as.data.frame(as.list(stats::setNames(rep("", length(columns)), columns)), stringsAsFactors = FALSE)
  }

  stats::setNames(vapply(columns, function(column_name) {
    spec <- metadata_field_spec(tab_key, column_name, tables, sample_row)
    metadata_wrap_label(spec$label)
  }, character(1)), columns)
}

default_visible_metadata_columns <- function(tab_key, tables) {
  available_columns <- names(editor_df_for_tab(tables, tab_key))
  if (length(available_columns) == 0) {
    return(character(0))
  }

  non_notes <- setdiff(available_columns, "notes")
  defaults <- head(non_notes, 6L)

  if (length(defaults) == 0) {
    defaults <- head(available_columns, 1L)
  }

  defaults
}

normalize_visible_metadata_columns <- function(tab_key, selected_columns, tables) {
  available_columns <- names(editor_df_for_tab(tables, tab_key))
  if (length(available_columns) == 0) {
    return(character(0))
  }

  selected_columns <- compact_chr(selected_columns)
  selected_columns <- selected_columns[selected_columns %in% available_columns]

  if (length(selected_columns) == 0) {
    selected_columns <- default_visible_metadata_columns(tab_key, tables)
  }

  if (length(selected_columns) == 0) {
    selected_columns <- available_columns[1]
  }

  selected_columns
}

metadata_editor_input <- function(id, spec, value) {
  value <- value %||% ""

  if (!isTRUE(spec$enabled)) {
    return(
      div(
        class = "mb-3",
        tags$label(class = "form-label", spec$label),
        div(class = "config-readonly", if (nzchar(value)) value else "(not used for this row)")
      )
    )
  }

  if (identical(spec$editor, "select")) {
    return(selectInput(id, spec$label, choices = spec$choices %||% character(0), selected = value))
  }

  if (identical(spec$editor, "multiselect")) {
    selected_values <- parse_pipe_values(value)
    available_values <- unname(spec$choices %||% character(0))
    selected_values <- selected_values[selected_values %in% available_values]

    return(
      shinyWidgets::pickerInput(
        inputId = id,
        label = spec$label,
        choices = spec$choices %||% character(0),
        selected = selected_values,
        multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          selectedTextFormat = "count > 3",
          countSelectedText = "{0} of {1} selected",
          noneSelectedText = "None selected"
        )
      )
    )
  }

  if (identical(spec$editor, "textarea")) {
    return(textAreaInput(id, spec$label, value = value, rows = spec$rows %||% 3L, resize = "vertical"))
  }

  textInput(id, spec$label, value = value)
}

default_session_name <- function(session_name = NULL, upload_filename = NULL) {
  value <- trimws(as.character(session_name %||% ""))
  if (!nzchar(value)) {
    upload_stem <- tools::file_path_sans_ext(as.character(upload_filename %||% ""))
    value <- if (nzchar(upload_stem)) upload_stem else "streamcurves_session"
  }
  value
}

sanitize_session_file_stem <- function(session_name = NULL, upload_filename = NULL) {
  value <- default_session_name(session_name, upload_filename)
  value <- gsub("[^A-Za-z0-9._-]+", "_", value)
  value <- gsub("_+", "_", value)
  value <- gsub("(^[._-]+)|([._-]+$)", "", value)
  if (!nzchar(value)) {
    value <- "streamcurves_session"
  }
  value
}

build_session_snapshot <- function(rv, session_name = NULL) {
  session_name <- default_session_name(session_name, rv$upload_filename %||% rv$session_name %||% NULL)
  current_metric <- rv$current_metric %||% NULL
  metric_config <- rv$metric_config %||% list()

  if (!is.null(current_metric) &&
      nzchar(current_metric) &&
      current_metric %in% names(metric_config)) {
    save_metric_phase_state(rv, current_metric)
  }

  list(
    version = "4.0",
    saved_at = Sys.time(),
    session_name = session_name,
    data_source = rv$data_source %||% "session_file",
    data = deep_copy_value(rv$data),
    qa_log = deep_copy_value(rv$qa_log),
    precheck_df = deep_copy_value(rv$precheck_df),
    data_fingerprint = rv$data_fingerprint %||% NULL,
    upload_filename = rv$upload_filename %||% NULL,
    current_metric = current_metric,
    input_metadata = deep_copy_value(rv$input_metadata),
    site_mask_config = deep_copy_value(rv$site_mask_config),
    phase1_candidates = deep_copy_value(rv$phase1_candidates),
    all_layer1_results = deep_copy_value(rv$all_layer1_results),
    all_layer2_results = deep_copy_value(rv$all_layer2_results),
    phase2_ranking = deep_copy_value(rv$phase2_ranking),
    cross_metric_consistency = deep_copy_value(rv$cross_metric_consistency),
    phase2_settings = deep_copy_value(rv$phase2_settings),
    phase2_metric_overrides = deep_copy_value(rv$phase2_metric_overrides),
    curve_stratification = deep_copy_value(rv$curve_stratification),
    summary_available_overrides = deep_copy_value(rv$summary_available_overrides),
    summary_edit_notes = deep_copy_value(rv$summary_edit_notes),
    phase3_verification = deep_copy_value(rv$phase3_verification),
    metric_phase_cache = deep_copy_value(rv$metric_phase_cache),
    stratum_results = deep_copy_value(rv$stratum_results),
    completed_metrics = deep_copy_value(rv$completed_metrics),
    decision_log = deep_copy_value(rv$decision_log),
    metric_config = deep_copy_value(rv$metric_config),
    strat_config = deep_copy_value(rv$strat_config),
    predictor_config = deep_copy_value(rv$predictor_config),
    factor_recode_config = deep_copy_value(rv$factor_recode_config),
    output_config = deep_copy_value(rv$output_config),
    config_version = deep_copy_value(rv$config_version),
    custom_groupings = deep_copy_value(rv$custom_groupings),
    custom_grouping_counter = deep_copy_value(rv$custom_grouping_counter)
  )
}

resolve_session_snapshot <- function(session_data, progress_cb = NULL) {
  if (!is.list(session_data)) {
    stop("Session file is not a valid StreamCurves .rds snapshot.", call. = FALSE)
  }

  session_data$version <- as.character(session_data$version %||% "1.0")

  if (is.null(session_data$metric_phase_cache) && !is.null(session_data$metric_step_cache)) {
    session_data$metric_phase_cache <- session_data$metric_step_cache
  }

  if (!is.null(session_data$input_metadata)) {
    input_bundle <- build_input_bundle_from_tables(session_data$input_metadata)
    session_data$metric_config <- session_data$metric_config %||% input_bundle$metric_config
    session_data$strat_config <- session_data$strat_config %||% input_bundle$strat_config
    session_data$predictor_config <- session_data$predictor_config %||% input_bundle$predictor_config
    session_data$factor_recode_config <- session_data$factor_recode_config %||% input_bundle$factor_recode_config
    session_data$site_mask_config <- session_data$site_mask_config %||% input_bundle$site_mask_config

    if (is.null(session_data$data)) {
      if (is.function(progress_cb)) {
        progress_cb("Rebuilding derived data from workbook metadata...", value = 2L)
      }
      clean_result <- clean_data(
        input_bundle$raw_data,
        input_bundle$metric_config,
        input_bundle$strat_config,
        input_bundle$factor_recode_config
      )
      derived <- derive_variables(
        clean_result$data,
        input_bundle$factor_recode_config,
        input_bundle$predictor_config,
        input_bundle$strat_config
      )
      precheck <- run_metric_precheck(derived, input_bundle$metric_config)

      session_data$data <- derived
      session_data$qa_log <- session_data$qa_log %||% clean_result$qa_log
      session_data$precheck_df <- session_data$precheck_df %||% precheck
      session_data$data_fingerprint <- session_data$data_fingerprint %||% digest::digest(derived, algo = "md5")
      session_data$data_source <- session_data$data_source %||% "session_file"
    }
  }

  if (is.null(session_data$data)) {
    stop("Session file does not include derived data or rebuildable workbook metadata.", call. = FALSE)
  }

  if (is.null(session_data$precheck_df) && !is.null(session_data$metric_config)) {
    if (is.function(progress_cb)) {
      progress_cb("Recomputing pre-run validation from restored data...", value = 3L)
    }
    session_data$precheck_df <- run_metric_precheck(session_data$data, session_data$metric_config)
  }

  if (is.null(session_data$data_fingerprint)) {
    session_data$data_fingerprint <- digest::digest(session_data$data, algo = "md5")
  }

  session_data$current_metric <- session_data$current_metric %||% names(session_data$metric_config %||% list())[1] %||% NULL
  session_data
}

mod_data_overview_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    legacy_browserless_session_restore_disabled <- FALSE

    ## ── Data loaded gate ────────────────────────────────────────────────

    ## ── Config editor init trigger ──────────────────────────────────────
    upload_error <- reactiveVal(NULL)
    metadata_status <- reactiveVal(NULL)
    metadata_visible_columns <- reactiveValues(
      metrics = NULL,
      stratifications = NULL,
      predictors = NULL,
      factor_recodes = NULL,
      custom_groups = NULL,
      site_masks = NULL
    )
    data_setup_load_progress <- NULL
    data_setup_load_request_nonce <- 0L
    data_setup_load_final_timeout_seconds <- 10

    next_data_setup_load_request_id <- function() {
      data_setup_load_request_nonce <<- data_setup_load_request_nonce + 1L
      paste0("data-setup-load-", data_setup_load_request_nonce)
    }

    close_data_setup_load_progress <- function(request_id = NULL, force = FALSE) {
      current <- data_setup_load_progress
      if (is.null(current)) {
        return(invisible(FALSE))
      }

      if (!isTRUE(force) &&
          !is.null(request_id) &&
          !identical(current$request_id %||% NULL, request_id)) {
        return(invisible(FALSE))
      }

      if (!is.null(current$progress)) {
        try(current$progress$close(), silent = TRUE)
      }
      if (!is.null(current$final_notification_id %||% NULL)) {
        remove_final_loading_notification(session, current$final_notification_id)
      }
      data_setup_load_progress <<- NULL
      invisible(TRUE)
    }

    schedule_data_setup_load_close <- function(request_id, delay_seconds = data_setup_load_final_timeout_seconds) {
      later::later(function() {
        current_after_wait <- data_setup_load_progress
        if (is.null(current_after_wait) ||
            !identical(current_after_wait$request_id %||% NULL, request_id)) {
          return(invisible(NULL))
        }

        close_data_setup_load_progress(request_id)
        invisible(NULL)
      }, delay_seconds)

      invisible(NULL)
    }

    begin_data_setup_load_progress <- function(total_steps, message, detail = NULL) {
      request_id <- next_data_setup_load_request_id()
      close_data_setup_load_progress(force = TRUE)

      progress <- shiny::Progress$new(session, min = 0, max = total_steps)
      progress$set(message = message, detail = detail, value = 0)

      data_setup_load_progress <<- list(
        request_id = request_id,
        progress = progress,
        total_steps = total_steps,
        message = message,
        detail = detail,
        value = 0,
        final_notification_id = NULL
      )

      request_id
    }

    show_data_setup_final_loading <- function(request_id) {
      current <- data_setup_load_progress
      if (is.null(current) || !identical(current$request_id %||% NULL, request_id)) {
        return(invisible(FALSE))
      }

      if (!is.null(current$progress)) {
        try(current$progress$close(), silent = TRUE)
        current$progress <- NULL
      }

      current$final_notification_id <- current$final_notification_id %||%
        paste0(ns("data_setup_load_final"), "-", request_id)

      show_final_loading_notification(
        session,
        current$final_notification_id,
        current$message,
        current$detail
      )

      data_setup_load_progress <<- current
      invisible(TRUE)
    }

    update_data_setup_load_progress <- function(request_id, detail = NULL, value = NULL, message = NULL) {
      current <- data_setup_load_progress
      if (is.null(current) || !identical(current$request_id %||% NULL, request_id)) {
        return(invisible(FALSE))
      }

      if (!is.null(message)) {
        current$message <- message
      }
      if (!is.null(detail)) {
        current$detail <- detail
      }
      if (!is.null(value)) {
        current$value <- max(0, min(as.numeric(value), as.numeric(current$total_steps %||% value)))
      }

      if (!is.null(current$progress)) {
        current$progress$set(
          message = current$message,
          detail = current$detail,
          value = current$value
        )
      } else if (!is.null(current$final_notification_id %||% NULL)) {
        show_final_loading_notification(
          session,
          current$final_notification_id,
          current$message,
          current$detail
        )
      }

      data_setup_load_progress <<- current
      invisible(TRUE)
    }

    finalize_data_setup_load_progress <- function(request_id,
                                                  detail = "Loading page, please wait. Preparing Data & Setup tab.") {
      current <- data_setup_load_progress
      if (is.null(current) || !identical(current$request_id %||% NULL, request_id)) {
        return(invisible(FALSE))
      }

      update_data_setup_load_progress(
        request_id,
        detail = detail,
        value = current$total_steps
      )
      show_data_setup_final_loading(request_id)
      schedule_data_setup_load_close(request_id)

      session$onFlushed(function() {
        latest <- data_setup_load_progress
        if (is.null(latest) || !identical(latest$request_id %||% NULL, request_id)) {
          return(invisible(NULL))
        }

        session$sendCustomMessage(
          "refreshMetadataAccordion",
          list(
            id = ns("metadata_accordion_content"),
            readyInputId = ns("data_setup_load_ready"),
            requestId = request_id
          )
        )
        invisible(NULL)
      }, once = TRUE)

      invisible(TRUE)
    }


    ## ── Load Data click ─────────────────────────────────────────────────
    output$reset_analysis_button <- renderUI({
      actionButton(
        ns("reset_analysis"),
        "Reset Analysis",
        class = "btn btn-outline-danger btn-sm",
        icon = icon("trash"),
        disabled = if (is.null(rv$data)) "disabled" else NULL
      )
    })

    observeEvent(input$reset_analysis, {
      req(!is.null(rv$data))
      showModal(modalDialog(
        title = "Reset Analysis",
        "This will return the app to its startup state. Loaded data,
         configuration changes, cached results, and all phase outputs
         will be cleared. This cannot be undone.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset_analysis"), "Reset Analysis",
                       class = "btn btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_reset_analysis, {
      removeModal()
      reset_app_to_startup(rv)
      upload_error(NULL)
      metadata_status(NULL)
      session$sendCustomMessage("clearModalBackdrop", list())
      showNotification("Analysis reset and app returned to startup state.",
                       type = "message", duration = 3)
    })

    ## ── Main content (shown after Load Data click) ──────────────────────
    output$main_content <- renderUI({
      metadata_panel_content <- div(
        id = ns("metadata_accordion_content"),
        class = "metadata-accordion-content",
        div(
          class = "d-flex flex-wrap gap-2 align-items-center mb-3",
          downloadButton(ns("download_workbook"), "Download Edited Workbook", class = "btn btn-primary"),
          uiOutput(ns("metadata_apply_status"))
        ),
        div(
          class = "metadata-editor-shell",
          navset_card_pill(
            id = ns("metadata_tabs"),
            nav_panel(
              "Metrics",
              value = "metrics",
              div(
                class = "d-flex flex-wrap justify-content-between align-items-start gap-2 mb-2",
                div(
                  class = "d-flex gap-2 flex-wrap",
                  actionButton(ns("add_metric_row"), "Add Row", class = "btn btn-outline-success btn-sm", icon = icon("plus")),
                  actionButton(ns("edit_metric_row"), "Edit Selected", class = "btn btn-outline-primary btn-sm", icon = icon("pen")),
                  actionButton(ns("delete_metric_rows"), "Delete Selected", class = "btn btn-outline-danger btn-sm", icon = icon("trash"))
                ),
                uiOutput(ns("metadata_metrics_visible_columns_ui"))
              ),
              DT::DTOutput(ns("metadata_metrics_table"))
            ),
            nav_panel(
              "Stratifications",
              value = "stratifications",
              div(
                class = "d-flex flex-wrap justify-content-between align-items-start gap-2 mb-2",
                div(
                  class = "d-flex gap-2 flex-wrap",
                  actionButton(ns("add_strat_row"), "Add Row", class = "btn btn-outline-success btn-sm", icon = icon("plus")),
                  actionButton(ns("edit_strat_row"), "Edit Selected", class = "btn btn-outline-primary btn-sm", icon = icon("pen")),
                  actionButton(ns("delete_strat_rows"), "Delete Selected", class = "btn btn-outline-danger btn-sm", icon = icon("trash"))
                ),
                uiOutput(ns("metadata_strat_visible_columns_ui"))
              ),
              DT::DTOutput(ns("metadata_strat_table"))
            ),
            nav_panel(
              "Predictors",
              value = "predictors",
              div(
                class = "d-flex flex-wrap justify-content-between align-items-start gap-2 mb-2",
                div(
                  class = "d-flex gap-2 flex-wrap",
                  actionButton(ns("add_predictor_row"), "Add Row", class = "btn btn-outline-success btn-sm", icon = icon("plus")),
                  actionButton(ns("edit_predictor_row"), "Edit Selected", class = "btn btn-outline-primary btn-sm", icon = icon("pen")),
                  actionButton(ns("delete_predictor_rows"), "Delete Selected", class = "btn btn-outline-danger btn-sm", icon = icon("trash"))
                ),
                uiOutput(ns("metadata_predictor_visible_columns_ui"))
              ),
              DT::DTOutput(ns("metadata_predictor_table"))
            ),
            nav_panel(
              "Factor Recodes",
              value = "factor_recodes",
              div(
                class = "d-flex flex-wrap justify-content-between align-items-start gap-2 mb-2",
                div(
                  class = "d-flex gap-2 flex-wrap",
                  actionButton(ns("add_recode_row"), "Add Row", class = "btn btn-outline-success btn-sm", icon = icon("plus")),
                  actionButton(ns("edit_recode_row"), "Edit Selected", class = "btn btn-outline-primary btn-sm", icon = icon("pen")),
                  actionButton(ns("delete_recode_rows"), "Delete Selected", class = "btn btn-outline-danger btn-sm", icon = icon("trash"))
                ),
                uiOutput(ns("metadata_recode_visible_columns_ui"))
              ),
              DT::DTOutput(ns("metadata_recode_table"))
            ),
            nav_panel(
              "Custom Groups",
              value = "custom_groups",
              div(
                class = "d-flex flex-wrap justify-content-between align-items-start gap-2 mb-2",
                div(
                  class = "d-flex gap-2 flex-wrap",
                  actionButton(ns("add_custom_grouping"), "Add Grouping", class = "btn btn-outline-success btn-sm", icon = icon("object-group")),
                  actionButton(ns("edit_group_row"), "Edit Selected", class = "btn btn-outline-primary btn-sm", icon = icon("pen")),
                  actionButton(ns("delete_group_rows"), "Delete Selected", class = "btn btn-outline-danger btn-sm", icon = icon("trash"))
                ),
                uiOutput(ns("metadata_group_visible_columns_ui"))
              ),
              DT::DTOutput(ns("metadata_group_table"))
            ),
            nav_panel(
              "Site Masks",
              value = "site_masks",
              div(
                class = "d-flex flex-wrap justify-content-between align-items-start gap-2 mb-2",
                div(
                  class = "d-flex gap-2 flex-wrap",
                  actionButton(ns("manage_site_masks"), "Manage Masks", class = "btn btn-outline-primary btn-sm", icon = icon("pen")),
                  actionButton(ns("delete_site_mask_rows"), "Delete Selected", class = "btn btn-outline-danger btn-sm", icon = icon("trash"))
                ),
                uiOutput(ns("metadata_site_mask_visible_columns_ui"))
              ),
              DT::DTOutput(ns("metadata_site_mask_table"))
            )
          )
        )
      )

      loaded_content <- tagList(
        data_setup_controls_card(ns, app_data_loaded = isTRUE(rv$app_data_loaded)),
        bslib::accordion(
          id = ns("data_accordion"),
          open = FALSE,
          bslib::accordion_panel(
            title = "Workbook Metadata",
            value = "workbook_metadata",
            metadata_panel_content
          ),
          bslib::accordion_panel(
            title = "Pre-Run Validation",
            icon = bsicons::bs_icon("shield-check"),
            uiOutput(ns("validation_warnings"))
          ),
          bslib::accordion_panel(
            title = "QA Log",
            icon = bsicons::bs_icon("journal-text"),
            DT::DTOutput(ns("qa_log_table"))
          ),
          bslib::accordion_panel(
            title = "Missing Data Summary",
            icon = bsicons::bs_icon("exclamation-diamond"),
            DT::DTOutput(ns("missing_table"))
          ),
          bslib::accordion_panel(
            title = "Full Dataset",
            icon = bsicons::bs_icon("table"),
            div(style = "overflow-x: auto;", DT::DTOutput(ns("full_data_table")))
          )
        )
      )

      startup_content <- card(
        class = "border-info",
        card_header("Get Started"),
        card_body(
          p("Upload a StreamCurves workbook (.xlsx) or load a saved session (.rds) to restore your dataset and workspace.")
        )
      )

      if (isTRUE(rv$app_data_loaded)) {
        loaded_content
      } else {
        tagList(
          data_setup_intro_card(),
          data_setup_controls_card(ns, app_data_loaded = FALSE),
          startup_content
        )
      }
    })

    ## ── Upload handler ────────────────────────────────────────────────────
    observeEvent(input$upload_file, {
      req(input$upload_file)
      upload_error(NULL)
      metadata_status(NULL)
      file_info <- input$upload_file
      ext <- tolower(tools::file_ext(file_info$name))
      request_id <- NULL

      tryCatch({
        if (ext != "xlsx") {
          close_data_setup_load_progress(force = TRUE)
          upload_error("Unsupported file type. Please upload an .xlsx workbook.")
          showNotification("Unsupported file type. Please upload an .xlsx workbook.",
                           type = "error", duration = 5)
          return()
        }

        request_id <- begin_data_setup_load_progress(
          total_steps = 5L,
          message = "Loading Workbook",
          detail = "Loading workbook tables..."
        )
        input_bundle <- load_data(file_info$datapath)

        update_data_setup_load_progress(request_id, detail = "Cleaning uploaded data...", value = 1L)
        clean_result <- clean_data(
          input_bundle$raw_data,
          input_bundle$metric_config,
          input_bundle$strat_config,
          input_bundle$factor_recode_config
        )
        update_data_setup_load_progress(request_id, detail = "Deriving analysis variables...", value = 2L)
        derived <- derive_variables(clean_result$data,
                                    input_bundle$factor_recode_config,
                                    input_bundle$predictor_config,
                                    input_bundle$strat_config)

        update_data_setup_load_progress(request_id, detail = "Running pre-run validation...", value = 3L)
        precheck <- run_metric_precheck(derived, input_bundle$metric_config)

        ## Reset analysis since data changed
        update_data_setup_load_progress(request_id, detail = "Applying dataset to the app...", value = 4L)
        reset_all_analysis(rv)

        ## Update reactive state
        rv$metric_config      <- input_bundle$metric_config
        rv$strat_config       <- input_bundle$strat_config
        rv$predictor_config   <- input_bundle$predictor_config
        rv$factor_recode_config <- input_bundle$factor_recode_config
        rv$input_metadata     <- input_bundle$metadata
        rv$site_mask_config   <- input_bundle$site_mask_config
        rv$data               <- derived
        rv$qa_log             <- clean_result$qa_log
        rv$precheck_df        <- precheck
        rv$data_source        <- "upload"
        rv$upload_filename    <- file_info$name
        rv$data_fingerprint <- digest::digest(derived, algo = "md5")
        rv$current_metric   <- names(input_bundle$metric_config)[1] %||% rv$current_metric
        rv$config_version   <- 0L

        rv$app_data_loaded <- TRUE
        metadata_status(NULL)
        finalize_data_setup_load_progress(request_id)

        showNotification(
          paste0("Loaded ", nrow(derived), " rows x ", ncol(derived),
                 " cols from ", file_info$name),
          type = "message", duration = 5
        )
      }, error = function(e) {
        close_data_setup_load_progress(request_id)
        upload_error(conditionMessage(e))
        metadata_status(list(type = "danger", text = paste0("Upload failed: ", conditionMessage(e))))
        showNotification(
          paste0("Upload failed: ", conditionMessage(e)),
          type = "error", duration = 8
        )
      })
    })

    ## ── Switch back to built-in data ─────────────────────────────────────

    ## ── Upload status display ────────────────────────────────────────────
    output$upload_status <- renderUI({
      if (!is.null(upload_error())) {
        div(
          class = "alert alert-danger py-1 px-2 mt-1",
          icon("exclamation-triangle"),
          upload_error()
        )
      }
    })

    output$metadata_apply_status <- renderUI({
      status <- metadata_status()
      if (is.null(status) || is.null(status$text) || !nzchar(trimws(status$text))) {
        return(NULL)
      }
      status_type <- status$type %||% "secondary"
      status_text <- status$text %||% ""
      alert_class <- switch(
        status_type,
        success = "alert alert-success py-2 px-3 mb-0",
        danger = "alert alert-danger py-2 px-3 mb-0",
        warning = "alert alert-warning py-2 px-3 mb-0",
        info = "alert alert-info py-2 px-3 mb-0",
        "alert alert-secondary py-2 px-3 mb-0"
      )

      div(class = alert_class, role = "status", status_text)
    })

    apply_candidate_metadata <- function(candidate_tables,
                                         success_text = "Metadata updated.",
                                         error_prefix = "Metadata update failed") {
      current_metric <- isolate(rv$current_metric)

      tryCatch({
        input_bundle <- build_input_bundle_from_tables(candidate_tables)
        clean_result <- clean_data(
          input_bundle$raw_data,
          input_bundle$metric_config,
          input_bundle$strat_config,
          input_bundle$factor_recode_config
        )
        derived <- derive_variables(
          clean_result$data,
          input_bundle$factor_recode_config,
          input_bundle$predictor_config,
          input_bundle$strat_config
        )
        precheck <- run_metric_precheck(derived, input_bundle$metric_config)

        reset_all_analysis(rv)

        rv$metric_config <- input_bundle$metric_config
        rv$strat_config <- input_bundle$strat_config
        rv$predictor_config <- input_bundle$predictor_config
        rv$factor_recode_config <- input_bundle$factor_recode_config
        rv$input_metadata <- input_bundle$metadata
        rv$site_mask_config <- input_bundle$site_mask_config
        rv$data <- derived
        rv$qa_log <- clean_result$qa_log
        rv$precheck_df <- precheck
        rv$data_fingerprint <- digest::digest(derived, algo = "md5")
        rv$current_metric <- if (!is.null(current_metric) && current_metric %in% names(input_bundle$metric_config)) {
          current_metric
        } else {
          names(input_bundle$metric_config)[1] %||% current_metric
        }
        rv$config_version <- isolate(rv$config_version %||% 0L) + 1L
        rv$app_data_loaded <- TRUE
        rv$custom_groupings <- list()
        rv$custom_grouping_counter <- list()

        metadata_status(list(type = "success", text = success_text))
        notify_workspace_refresh(rv)
        TRUE
      }, error = function(e) {
        message_text <- paste0(error_prefix, ": ", conditionMessage(e))
        metadata_status(list(type = "danger", text = message_text))
        showNotification(message_text, type = "error", duration = 8)
        FALSE
      })
    }

    current_metadata_tab <- reactive({
      input$metadata_tabs %||% "metrics"
    })

    observeEvent(current_metadata_tab(), {
      req(isTRUE(rv$app_data_loaded))
      session$onFlushed(function() {
        session$sendCustomMessage(
          "refreshMetadataAccordion",
          list(id = ns("metadata_accordion_content"))
        )
      }, once = TRUE)
    }, ignoreInit = FALSE)

    get_visible_metadata_columns <- function(tab_key) {
      req(rv$input_metadata)
      current <- metadata_visible_columns[[tab_key]]
      normalize_visible_metadata_columns(tab_key, current, rv$input_metadata)
    }

    set_visible_metadata_columns <- function(tab_key, selected_columns) {
      req(rv$input_metadata)
      normalized <- normalize_visible_metadata_columns(tab_key, selected_columns, rv$input_metadata)
      if (!identical(isolate(metadata_visible_columns[[tab_key]]), normalized)) {
        metadata_visible_columns[[tab_key]] <- normalized
      }
      invisible(normalized)
    }

    render_column_picker <- function(tab_key, output_name, input_name) {
      output[[output_name]] <- renderUI({
        req(rv$app_data_loaded, rv$input_metadata)
        available_columns <- names(editor_df_for_tab(rv$input_metadata, tab_key))
        choice_labels <- vapply(available_columns, metadata_pretty_label, character(1))
        choices <- stats::setNames(available_columns, choice_labels)
        selected <- get_visible_metadata_columns(tab_key)

        div(
          class = "metadata-column-picker",
          `data-tab-key` = tab_key,
          shinyWidgets::pickerInput(
            inputId = ns(input_name),
            label = "Visible Columns",
            choices = choices,
            selected = selected,
            multiple = TRUE,
            stateInput = TRUE,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              selectedTextFormat = "count > 2",
              countSelectedText = "{0} of {1} shown"
            ),
            width = "100%"
          )
        )
      })
    }

    register_column_picker_commit <- function(tab_key, input_name) {
      open_input_name <- paste0(input_name, "_open")
      observeEvent(input[[open_input_name]], {
        if (isTRUE(input[[open_input_name]])) {
          return(invisible(NULL))
        }

        set_visible_metadata_columns(tab_key, input[[input_name]] %||% character(0))
      }, ignoreInit = TRUE)
    }

    observeEvent(rv$input_metadata, {
      req(rv$input_metadata)
      for (tab_key in names(metadata_tab_titles())) {
        set_visible_metadata_columns(tab_key, isolate(metadata_visible_columns[[tab_key]]))
      }
    }, ignoreInit = FALSE)

    selected_metadata_rows <- function(tab_key, modal = FALSE) {
      selected <- input[[metadata_selection_input_id(tab_key, modal)]]
      as.integer(selected %||% integer(0))
    }

    row_editor_state <- reactiveValues(
      tab_key = NULL,
      row_index = NULL
    )
    site_mask_modal_state <- reactiveValues(open = FALSE)

    editor_base_row <- reactive({
      req(row_editor_state$tab_key, row_editor_state$row_index, rv$input_metadata)
      editor_df <- editor_df_for_tab(rv$input_metadata, row_editor_state$tab_key)
      req(nrow(editor_df) >= row_editor_state$row_index)
      editor_df[row_editor_state$row_index, , drop = FALSE]
    })

    editor_current_row <- reactive({
      row <- editor_base_row()
      if (nrow(row) == 0) {
        return(row)
      }

      for (column_name in names(row)) {
        input_value <- input[[paste0("editor_", column_name)]]
        if (is.null(input_value)) {
          next
        }

        if (length(input_value) > 1) {
          row[[column_name]] <- collapse_pipe_text(input_value)
        } else if (length(input_value) == 1) {
          row[[column_name]] <- as.character(input_value[[1]] %||% "")
        } else {
          row[[column_name]] <- ""
        }
      }

      row
    })

    output$metadata_row_editor_ui <- renderUI({
      req(row_editor_state$tab_key, rv$input_metadata)
      row <- editor_current_row()
      tables <- rv$input_metadata

      tagList(lapply(names(row), function(column_name) {
        spec <- metadata_field_spec(row_editor_state$tab_key, column_name, tables, row)
        metadata_editor_input(
          ns(paste0("editor_", column_name)),
          spec,
          row[[column_name]][[1]] %||% ""
        )
      }))
    })

    show_metadata_row_editor <- function(tab_key, row_index) {
      row_editor_state$tab_key <- tab_key
      row_editor_state$row_index <- row_index

      showModal(modalDialog(
        title = paste0("Edit ", metadata_tab_titles()[[tab_key]], " Row"),
        uiOutput(ns("metadata_row_editor_ui")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_metadata_row"), "Save Changes", class = "btn btn-primary")
        ),
        easyClose = TRUE,
        size = "l",
        class = "metadata-row-editor-modal"
      ))
    }

    output$site_mask_manager_preview <- DT::renderDT({
      req(rv$input_metadata)
      label_column <- input$site_mask_label_column %||% site_mask_label_column_from_tables(rv$input_metadata)
      selected_ids <- suppressWarnings(as.integer(input$site_mask_ids %||% integer(0)))
      selected_ids <- sort(unique(selected_ids[!is.na(selected_ids)]))
      preview_df <- tibble::tibble(
        masked_sites = selected_ids,
        site_label = site_mask_labels_for_ids(rv$input_metadata, selected_ids, label_column)
      )

      DT::datatable(
        preview_df,
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        class = "compact stripe"
      )
    })

    show_site_mask_manager <- function() {
      req(rv$input_metadata)
      current_masks <- metadata_table_to_editor_df(rv$input_metadata$site_masks, "site_masks")
      selected_ids <- suppressWarnings(as.integer(current_masks$masked_sites))
      selected_ids <- sort(unique(selected_ids[!is.na(selected_ids)]))
      label_column <- site_mask_label_column_from_tables(rv$input_metadata)

      showModal(modalDialog(
        title = "Manage Site Masks",
        shinyWidgets::pickerInput(
          ns("site_mask_ids"),
          "Masked sites",
          choices = site_mask_choice_vector(rv$input_metadata, label_column),
          selected = as.character(selected_ids),
          multiple = TRUE,
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            size = 10,
            selectedTextFormat = "count > 4",
            countSelectedText = "{0} of {1} selected",
            noneSelectedText = "No sites masked"
          )
        ),
        selectInput(
          ns("site_mask_label_column"),
          "Site label column",
          choices = named_choice_vector(metadata_raw_data_columns(rv$input_metadata)),
          selected = label_column
        ),
        tags$p(
          class = "text-muted small mb-2",
          "Masked site labels are regenerated from the selected label column when the workbook is saved."
        ),
        DT::DTOutput(ns("site_mask_manager_preview")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_site_masks"), "Save Masks", class = "btn btn-primary")
        ),
        easyClose = TRUE,
        size = "l",
        class = "metadata-row-editor-modal"
      ))
    }

    add_metadata_rows <- function(tab_key) {
      req(rv$app_data_loaded, rv$input_metadata)

      candidate_tables <- switch(
        tab_key,
        metrics = add_metric_row_to_tables(isolate(rv$input_metadata)),
        stratifications = add_stratification_row_to_tables(isolate(rv$input_metadata)),
        predictors = add_predictor_row_to_tables(isolate(rv$input_metadata)),
        factor_recodes = add_factor_recode_row_to_tables(isolate(rv$input_metadata)),
        custom_groups = add_custom_grouping_to_tables(isolate(rv$input_metadata)),
        stop(paste0("Unsupported metadata tab: ", tab_key), call. = FALSE)
      )

      apply_candidate_metadata(
        candidate_tables,
        success_text = paste0("Added ", if (identical(tab_key, "custom_groups")) "a custom grouping." else "a metadata row."),
        error_prefix = paste0("Could not add ", tab_key)
      )
    }

    delete_metadata_rows <- function(tab_key, selected_rows) {
      req(rv$app_data_loaded, rv$input_metadata)
      if (length(selected_rows %||% integer(0)) == 0) {
        metadata_status(list(type = "warning", text = "Select at least one row to delete."))
        return(invisible(FALSE))
      }

      candidate_tables <- delete_rows_from_tables(isolate(rv$input_metadata), tab_key, selected_rows)
      apply_candidate_metadata(
        candidate_tables,
        success_text = paste0("Removed selected ", tab_key, " rows."),
        error_prefix = paste0("Could not remove selected ", tab_key)
      )
    }

    render_metadata_editor <- function(tab_key, output_name, modal = FALSE) {
      output[[output_name]] <- DT::renderDT({
        req(rv$app_data_loaded, rv$input_metadata)
        display_df <- metadata_display_df_for_tab(tab_key, rv$input_metadata)
        visible_columns <- get_visible_metadata_columns(tab_key)
        visible_columns <- visible_columns[visible_columns %in% names(display_df)]
        if (length(visible_columns) == 0) {
          visible_columns <- names(display_df)
        }

        display_df <- display_df[, visible_columns, drop = FALSE]
        labels <- unname(metadata_column_labels(tab_key, rv$input_metadata)[visible_columns])

        DT::datatable(
          display_df,
          rownames = FALSE,
          container = metadata_table_container(labels),
          selection = list(mode = "multiple", target = "row"),
          escape = TRUE,
          options = list(
            scrollX = TRUE,
            paging = FALSE,
            autoWidth = FALSE
          ),
          class = "compact stripe cell-border metadata-editor-table"
        )
      })
    }

    render_metadata_editor("metrics", "metadata_metrics_table")
    render_metadata_editor("stratifications", "metadata_strat_table")
    render_metadata_editor("predictors", "metadata_predictor_table")
    render_metadata_editor("factor_recodes", "metadata_recode_table")
    render_metadata_editor("custom_groups", "metadata_group_table")
    render_metadata_editor("site_masks", "metadata_site_mask_table")
    render_column_picker("metrics", "metadata_metrics_visible_columns_ui", "metadata_metrics_visible_columns")
    render_column_picker("stratifications", "metadata_strat_visible_columns_ui", "metadata_strat_visible_columns")
    render_column_picker("predictors", "metadata_predictor_visible_columns_ui", "metadata_predictor_visible_columns")
    render_column_picker("factor_recodes", "metadata_recode_visible_columns_ui", "metadata_recode_visible_columns")
    render_column_picker("custom_groups", "metadata_group_visible_columns_ui", "metadata_group_visible_columns")
    render_column_picker("site_masks", "metadata_site_mask_visible_columns_ui", "metadata_site_mask_visible_columns")

    register_column_picker_commit("metrics", "metadata_metrics_visible_columns")
    register_column_picker_commit("stratifications", "metadata_strat_visible_columns")
    register_column_picker_commit("predictors", "metadata_predictor_visible_columns")
    register_column_picker_commit("factor_recodes", "metadata_recode_visible_columns")
    register_column_picker_commit("custom_groups", "metadata_group_visible_columns")
    register_column_picker_commit("site_masks", "metadata_site_mask_visible_columns")

    edit_selected_metadata_row <- function(tab_key, modal = FALSE) {
      selected_rows <- selected_metadata_rows(tab_key, modal)
      if (length(selected_rows) == 0) {
        metadata_status(list(type = "warning", text = "Select a row to edit."))
        return(invisible(FALSE))
      }

      show_metadata_row_editor(tab_key, selected_rows[[1]])
      invisible(TRUE)
    }

    observeEvent(input$save_metadata_row, {
      req(row_editor_state$tab_key, row_editor_state$row_index, rv$input_metadata)
      editor_df <- editor_df_for_tab(isolate(rv$input_metadata), row_editor_state$tab_key)
      req(nrow(editor_df) >= row_editor_state$row_index)

      updated_row <- editor_current_row()
      editor_df[row_editor_state$row_index, names(updated_row)] <- updated_row[1, , drop = FALSE]
      candidate_tables <- apply_editor_df_to_tables(isolate(rv$input_metadata), row_editor_state$tab_key, editor_df)

      updated <- apply_candidate_metadata(
        candidate_tables,
        success_text = paste0("Updated ", row_editor_state$tab_key, " metadata."),
        error_prefix = paste0("Could not update ", row_editor_state$tab_key)
      )

      if (isTRUE(updated)) {
        removeModal()
      }
    })

    observeEvent(input$add_metric_row, {
      add_metadata_rows("metrics")
    })

    observeEvent(input$edit_metric_row, {
      edit_selected_metadata_row("metrics")
    })

    observeEvent(input$delete_metric_rows, {
      delete_metadata_rows("metrics", input$metadata_metrics_table_rows_selected)
    })

    observeEvent(input$add_strat_row, {
      add_metadata_rows("stratifications")
    })

    observeEvent(input$edit_strat_row, {
      edit_selected_metadata_row("stratifications")
    })

    observeEvent(input$delete_strat_rows, {
      delete_metadata_rows("stratifications", input$metadata_strat_table_rows_selected)
    })

    observeEvent(input$add_predictor_row, {
      add_metadata_rows("predictors")
    })

    observeEvent(input$edit_predictor_row, {
      edit_selected_metadata_row("predictors")
    })

    observeEvent(input$delete_predictor_rows, {
      delete_metadata_rows("predictors", input$metadata_predictor_table_rows_selected)
    })

    observeEvent(input$add_recode_row, {
      add_metadata_rows("factor_recodes")
    })

    observeEvent(input$edit_recode_row, {
      edit_selected_metadata_row("factor_recodes")
    })

    observeEvent(input$delete_recode_rows, {
      delete_metadata_rows("factor_recodes", input$metadata_recode_table_rows_selected)
    })

    observeEvent(input$add_custom_grouping, {
      add_metadata_rows("custom_groups")
    })

    observeEvent(input$edit_group_row, {
      edit_selected_metadata_row("custom_groups")
    })

    observeEvent(input$delete_group_rows, {
      delete_metadata_rows("custom_groups", input$metadata_group_table_rows_selected)
    })

    observeEvent(input$manage_site_masks, {
      req(rv$app_data_loaded, rv$input_metadata)
      show_site_mask_manager()
    })

    observeEvent(input$save_site_masks, {
      req(rv$app_data_loaded, rv$input_metadata)
      selected_ids <- suppressWarnings(as.integer(input$site_mask_ids %||% integer(0)))
      selected_ids <- selected_ids[!is.na(selected_ids)]
      label_column <- input$site_mask_label_column %||% site_mask_label_column_from_tables(rv$input_metadata)

      candidate_tables <- apply_site_mask_selection_to_tables(
        isolate(rv$input_metadata),
        selected_ids,
        label_column
      )

      updated <- apply_candidate_metadata(
        candidate_tables,
        success_text = "Updated site masks.",
        error_prefix = "Could not update site masks"
      )

      if (isTRUE(updated)) {
        removeModal()
      }
    })

    observeEvent(input$delete_site_mask_rows, {
      delete_metadata_rows("site_masks", input$metadata_site_mask_table_rows_selected)
    })

    output$download_workbook <- downloadHandler(
      filename = function() {
        base_name <- tools::file_path_sans_ext(rv$upload_filename %||% rv$session_name %||% "streamcurves_workbook")
        paste0(base_name, "_edited.xlsx")
      },
      content = function(file) {
        req(rv$app_data_loaded, rv$input_metadata)
        write_input_workbook(
          rv$input_metadata,
          file,
          script_path = file.path(project_root, "scripts", "write_workbook_from_json.py")
        )
      }
    )

    output$qa_log_table <- DT::renderDT({
      DT::datatable(
        rv$qa_log,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    output$missing_table <- DT::renderDT({
      missing_counts <- sapply(rv$data, function(x) sum(is.na(x)))
      missing_df <- data.frame(
        Column = names(missing_counts),
        Missing = as.integer(missing_counts),
        Percent = round(100 * missing_counts / nrow(rv$data), 1),
        stringsAsFactors = FALSE
      )
      missing_df <- missing_df[missing_df$Missing > 0, ]
      missing_df <- missing_df[order(-missing_df$Missing), ]

      DT::datatable(
        missing_df,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    output$full_data_table <- DT::renderDT({
      DT::datatable(
        rv$data,
        options = list(pageLength = 15, scrollX = TRUE, scrollY = "400px"),
        rownames = FALSE,
        class = "compact stripe",
        filter = "top"
      )
    })

    ## ── Validation warnings ─────────────────────────────────────────────
    output$validation_warnings <- renderUI({
      warnings <- list()

      ## Check for sparse ecoregions
      if ("Ecoregion" %in% names(rv$data)) {
        eco_counts <- table(rv$data$Ecoregion)
        sparse <- eco_counts[eco_counts < 10]
        if (length(sparse) > 0) {
          for (eco in names(sparse)) {
            warnings <- c(warnings, list(
              div(class = "alert alert-warning py-1 px-2 mb-1",
                  icon("exclamation-triangle"),
                  sprintf(" Ecoregion '%s' has only %d sites", eco, sparse[[eco]]))
            ))
          }
        }
      }

      ## Check metrics with few observations
      precheck <- rv$precheck_df
      if (!is.null(precheck) && nrow(precheck) > 0) {
        sparse_metrics <- precheck |> dplyr::filter(n_obs < 15)
        if (nrow(sparse_metrics) > 0) {
          warnings <- c(warnings, list(
            div(class = "alert alert-info py-1 px-2 mb-1",
                icon("info-circle"),
                sprintf(" %d metric(s) have fewer than 15 observations",
                        nrow(sparse_metrics)))
          ))
        }
      }

      ## Check DACAT=3
      if ("DACAT" %in% names(rv$data)) {
        dacat3_n <- sum(rv$data$DACAT == 3, na.rm = TRUE)
        if (dacat3_n < 10) {
          warnings <- c(warnings, list(
            div(class = "alert alert-warning py-1 px-2 mb-1",
                icon("exclamation-triangle"),
                sprintf(" DACAT=3 (large watershed) has only %d sites", dacat3_n))
          ))
        }
      }

      if (length(warnings) == 0) {
        div(class = "text-success", icon("check-circle"), " No validation warnings.")
      } else {
        tagList(warnings)
      }
    })

    ## ── Session save ────────────────────────────────────────────────────
    ## ── Refresh session list ────────────────────────────────────────────
    observeEvent(rv$app_reset_nonce, {
      close_data_setup_load_progress(force = TRUE)
      upload_error(NULL)
      metadata_status(NULL)
      updateTextInput(session, "session_name", value = "")
      session$sendCustomMessage("clearFileInput",
                                list(id = ns("upload_file")))
      session$sendCustomMessage("clearFileInput",
                                list(id = ns("load_session_file")))
      session$sendCustomMessage("clearModalBackdrop", list())
    }, ignoreInit = TRUE)

    observeEvent(input$data_setup_load_ready, {
      request_id <- input$data_setup_load_ready %||% NULL
      req(!is.null(request_id), nzchar(as.character(request_id)))
      close_data_setup_load_progress(as.character(request_id))
    }, ignoreInit = TRUE)

    ## ── Session load ────────────────────────────────────────────────────
    observeEvent(input$legacy_session_restore_trigger_unused, {
      if (!legacy_browserless_session_restore_disabled) {
        return(invisible(NULL))
      }

      req(input$legacy_session_restore_choice_unused, input$legacy_session_restore_choice_unused != "(none)")
      session_name <- input$legacy_session_restore_choice_unused

      load_path <- file.path(project_root, "outputs", "sessions",
                             paste0(session_name, ".rds"))
      if (!file.exists(load_path)) {
        showNotification("Session file not found.", type = "error")
        return()
      }

      session_data <- readRDS(load_path)

      if (identical(session_data$data_source %||% NULL, "builtin")) {
        showNotification(
          paste0(
            "Session '", session_name,
            "' uses the removed built-in example dataset and cannot be restored. Upload a dataset first."
          ),
          type = "error",
          duration = 8
        )
        return()
      }

      ## Verify data fingerprint
      current_fp <- rv$data_fingerprint
      if (!is.null(session_data$data_fingerprint) &&
          !is.null(current_fp) && !is.na(current_fp) &&
          session_data$data_fingerprint != current_fp) {
        showNotification(
          "Warning: Dataset has changed since session was saved. Results may be stale.",
          type = "warning", duration = 8
        )
      }

      ## Restore state — handle both v1.2 and v2.0 formats
      session_version <- session_data$version %||% "1.0"

      if (!is.null(session_data$completed_metrics)) {
        rv$completed_metrics <- session_data$completed_metrics
      }
      if (!is.null(session_data$decision_log)) {
        rv$decision_log <- session_data$decision_log
      }
      if (!is.null(session_data$all_layer1_results)) {
        rv$all_layer1_results <- session_data$all_layer1_results
      }
      if (!is.null(session_data$all_layer2_results)) {
        rv$all_layer2_results <- session_data$all_layer2_results
      }
      if (!is.null(session_data$cross_metric_consistency)) {
        rv$cross_metric_consistency <- session_data$cross_metric_consistency
      }
      rv$phase2_settings <- empty_phase2_settings()

      if (session_version >= "2.0") {
        ## v2.0 fields
        rv$phase2_metric_overrides <- list()
        rv$summary_available_overrides <- list()
        rv$summary_edit_notes <- list()
        if (!is.null(session_data$phase1_candidates)) {
          rv$phase1_candidates <- session_data$phase1_candidates
        }
        if (!is.null(session_data$phase2_ranking)) {
          rv$phase2_ranking <- session_data$phase2_ranking
        }
        if (!is.null(session_data$phase2_settings)) {
          rv$phase2_settings <- session_data$phase2_settings
        }
        if (!is.null(session_data$phase2_metric_overrides)) {
          rv$phase2_metric_overrides <- session_data$phase2_metric_overrides
        }
        if (!is.null(session_data$summary_available_overrides)) {
          rv$summary_available_overrides <- session_data$summary_available_overrides
        }
        if (!is.null(session_data$summary_edit_notes)) {
          rv$summary_edit_notes <- session_data$summary_edit_notes
        }
        if (!is.null(session_data$phase3_verification)) {
          rv$phase3_verification <- session_data$phase3_verification
        }
        if (!is.null(session_data$metric_phase_cache)) {
          rv$metric_phase_cache <- session_data$metric_phase_cache
        }
        if (!is.null(session_data$stratum_results)) {
          rv$stratum_results <- session_data$stratum_results
        }
      } else {
        ## v1.2 compatibility: map metric_step_cache -> metric_phase_cache
        if (!is.null(session_data$metric_step_cache)) {
          rv$metric_phase_cache <- session_data$metric_step_cache
        }
      }

      ## Restore configs if present
      if (!is.null(session_data$current_metric)) {
        rv$current_metric <- session_data$current_metric
      }
      if (!is.null(session_data$input_metadata)) {
        rv$input_metadata <- session_data$input_metadata
      }
      if (!is.null(session_data$metric_config)) {
        rv$metric_config <- session_data$metric_config
      }
      if (!is.null(session_data$strat_config)) {
        rv$strat_config <- session_data$strat_config
      }
      if (!is.null(session_data$predictor_config)) {
        rv$predictor_config <- session_data$predictor_config
      }
      if (!is.null(session_data$factor_recode_config)) {
        rv$factor_recode_config <- session_data$factor_recode_config
      }
      if (!is.null(session_data$output_config)) {
        rv$output_config <- utils::modifyList(
          deep_copy_value(rv$startup_output_config),
          session_data$output_config,
          keep.null = TRUE
        )
      }
      if (!is.null(session_data$config_version)) {
        rv$config_version <- session_data$config_version
      }

      if (is.null(rv$current_metric) || !rv$current_metric %in% names(rv$metric_config)) {
        rv$current_metric <- names(rv$metric_config)[1] %||% rv$current_metric
      }
      rv$phase2_settings <- normalize_phase2_settings(rv, rv$phase2_settings)

      ## Restore per-phase state for whichever metric is currently selected
      if (!is.null(rv$current_metric) &&
          rv$current_metric %in% names(rv$metric_phase_cache)) {
        restore_metric_phase_state(rv, rv$current_metric)
      }

      rv$app_data_loaded <- !is.null(rv$data)
      metadata_status(NULL)

      showNotification(
        paste0("Session '", session_name, "' loaded. ",
               length(rv$completed_metrics), " completed metrics restored."),
        type = "message", duration = 5
      )
    })

    restore_session_into_rv <- function(session_data, source_name = NULL, progress_cb = NULL) {
      if (is.function(progress_cb)) {
        progress_cb("Resolving session contents...", value = 1L)
      }
      snapshot <- resolve_session_snapshot(session_data, progress_cb = progress_cb)

      if (is.function(progress_cb)) {
        progress_cb("Restoring saved analysis state...", value = 4L)
      }

      reset_all_analysis(rv)

      rv$data <- snapshot$data %||% NULL
      rv$qa_log <- snapshot$qa_log %||% NULL
      rv$precheck_df <- snapshot$precheck_df %||% NULL

      rv$data_source <- snapshot$data_source %||% "session_file"
      rv$data_fingerprint <- snapshot$data_fingerprint %||% digest::digest(snapshot$data, algo = "md5")
      rv$upload_filename <- snapshot$upload_filename %||% NULL
      rv$input_metadata <- snapshot$input_metadata %||% NULL
      rv$site_mask_config <- snapshot$site_mask_config %||% NULL

      rv$metric_config <- snapshot$metric_config %||% deep_copy_value(rv$startup_metric_config)
      rv$strat_config <- snapshot$strat_config %||% deep_copy_value(rv$startup_strat_config)
      rv$predictor_config <- snapshot$predictor_config %||% deep_copy_value(rv$startup_predictor_config)
      rv$factor_recode_config <- snapshot$factor_recode_config %||% deep_copy_value(rv$startup_factor_recode_config)
      rv$output_config <- if (!is.null(snapshot$output_config)) {
        utils::modifyList(
          deep_copy_value(rv$startup_output_config),
          snapshot$output_config,
          keep.null = TRUE
        )
      } else {
        deep_copy_value(rv$startup_output_config)
      }
      rv$config_version <- snapshot$config_version %||% (rv$startup_config_version %||% 0L)

      rv$phase1_candidates <- snapshot$phase1_candidates %||% list()
      rv$all_layer1_results <- snapshot$all_layer1_results %||% list()
      rv$all_layer2_results <- snapshot$all_layer2_results %||% list()
      rv$phase2_ranking <- snapshot$phase2_ranking %||% NULL
      rv$cross_metric_consistency <- snapshot$cross_metric_consistency %||% NULL
      rv$phase2_settings <- normalize_phase2_settings(rv, snapshot$phase2_settings %||% empty_phase2_settings())
      rv$phase2_metric_overrides <- snapshot$phase2_metric_overrides %||% list()
      rv$curve_stratification <- snapshot$curve_stratification %||% list()
      rv$summary_available_overrides <- snapshot$summary_available_overrides %||% list()
      rv$summary_edit_notes <- snapshot$summary_edit_notes %||% list()
      rv$phase3_verification <- snapshot$phase3_verification %||% list()
      rv$metric_phase_cache <- snapshot$metric_phase_cache %||% list()
      rv$stratum_results <- snapshot$stratum_results %||% list()
      rv$completed_metrics <- snapshot$completed_metrics %||% list()
      rv$decision_log <- snapshot$decision_log %||% tibble::tibble()
      rv$custom_groupings <- snapshot$custom_groupings %||% list()
      rv$custom_grouping_counter <- snapshot$custom_grouping_counter %||% list()

      rv$session_name <- snapshot$session_name %||% default_session_name(source_name, snapshot$upload_filename %||% NULL)
      rv$current_metric <- snapshot$current_metric %||% names(rv$metric_config)[1] %||% rv$current_metric

      if (is.null(rv$current_metric) || !(rv$current_metric %in% names(rv$metric_config))) {
        rv$current_metric <- names(rv$metric_config)[1] %||% rv$current_metric
      }

      if (!is.null(rv$current_metric) && rv$current_metric %in% names(rv$metric_phase_cache)) {
        restore_metric_phase_state(rv, rv$current_metric)
      }

      rv$app_data_loaded <- !is.null(rv$data)
      metadata_status(NULL)
      notify_workspace_refresh(rv)
      invisible(snapshot)
    }

    output$download_session <- downloadHandler(
      filename = function() {
        paste0(
          sanitize_session_file_stem(input$session_name %||% NULL, rv$upload_filename %||% rv$session_name %||% NULL),
          ".rds"
        )
      },
      content = function(file) {
        req(rv$app_data_loaded)
        session_name <- default_session_name(input$session_name %||% NULL, rv$upload_filename %||% rv$session_name %||% NULL)
        rv$session_name <- session_name
        session_data <- build_session_snapshot(rv, session_name)
        saveRDS(session_data, file)
      }
    )

    clear_session_upload_input <- function() {
      session$sendCustomMessage(
        "clearFileInput",
        list(id = ns("load_session_file"))
      )
      invisible(NULL)
    }

    observeEvent(input$load_session_file, {
      req(input$load_session_file)
      metadata_status(NULL)
      file_info <- input$load_session_file
      ext <- tolower(tools::file_ext(file_info$name %||% ""))
      request_id <- NULL

      if (!identical(ext, "rds")) {
        close_data_setup_load_progress(force = TRUE)
        showNotification("Unsupported session file type. Please upload an .rds session file.", type = "error", duration = 8)
        clear_session_upload_input()
        return(invisible(NULL))
      }

      tryCatch({
        request_id <- begin_data_setup_load_progress(
          total_steps = 5L,
          message = "Loading Session",
          detail = "Reading saved session snapshot..."
        )
        session_data <- readRDS(file_info$datapath)
        snapshot <- restore_session_into_rv(
          session_data,
          source_name = tools::file_path_sans_ext(file_info$name),
          progress_cb = function(detail, value = NULL) {
            update_data_setup_load_progress(request_id, detail = detail, value = value)
          }
        )

        updateTextInput(
          session,
          "session_name",
          value = snapshot$session_name %||% ""
        )
        clear_session_upload_input()
        finalize_data_setup_load_progress(request_id)

        showNotification(
          paste0(
            "Session '",
            snapshot$session_name %||% tools::file_path_sans_ext(file_info$name),
            "' loaded. ",
            length(rv$completed_metrics),
            " completed metrics restored."
          ),
          type = "message",
          duration = 5
        )
      }, error = function(e) {
        message_text <- paste0("Session load failed: ", conditionMessage(e))
        metadata_status(list(type = "danger", text = message_text))
        close_data_setup_load_progress(request_id)
        clear_session_upload_input()
        showNotification(message_text, type = "error", duration = 8)
      })
    })

  })
}
