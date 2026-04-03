## ── Module: Phase 1 — Initial Exploration ────────────────────────────────────────
## Merges precheck, significance screening, and effect size into a single view.
## Per-metric: metric picker + screening + effect size + candidate marking.

library(shiny)
library(bslib)
library(DT)

mod_phase1_exploration_ui <- function(id, dialog_mode = FALSE) {
  ns <- NS(id)
  uiOutput(ns("phase1_page"))
}

mod_phase1_exploration_server <- function(id, rv, dialog_mode = FALSE, workspace_scope = "standalone") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    prev_metric <- reactiveVal(NULL)
    workspace_scope <- match.arg(workspace_scope, c("standalone", "analysis"))

    phase1_workspace_active <- function(isolate_state = FALSE) {
      if (!isTRUE(dialog_mode)) {
        return(TRUE)
      }

      workspace_scope_is_active(
        rv,
        workspace_scope = workspace_scope,
        standalone_modal_type = "phase1",
        isolate_state = isolate_state
      )
    }

    picker_container <- function() {
      if (isTRUE(dialog_mode)) {
        ".modal-dialog.workspace-modal-dialog"
      } else {
        "body"
      }
    }

    metric_display_name <- function(metric) {
      cfg <- rv$metric_config[[metric]]
      cfg$display_name %||% metric
    }

    metric_units_label <- function(metric) {
      cfg <- rv$metric_config[[metric]]
      units <- cfg$units %||% ""
      if (is.null(units) || !nzchar(units)) "" else units
    }

    metric_axis_label <- function(metric) {
      label <- metric_display_name(metric)
      units <- metric_units_label(metric)
      if (!nzchar(units)) label else paste0(label, " (", units, ")")
    }

    metric_column_name <- function(metric) {
      cfg <- rv$metric_config[[metric]]
      cfg$column_name %||% NA_character_
    }

    scatter_context_metric <- function() {
      metric <- if (isTRUE(dialog_mode)) {
        rv$workspace_modal_metric %||% rv$current_metric
      } else {
        rv$current_metric
      }

      metric <- as.character(metric %||% "")
      if (!nzchar(metric)) {
        return(NULL)
      }

      metric
    }

    site_identity_frame <- function(data = rv$data) {
      req(!is.null(data))

      if (all(c(streamcurves_site_id_column, streamcurves_site_label_column) %in% names(data))) {
        return(tibble::tibble(
          site_id = suppressWarnings(as.integer(data[[streamcurves_site_id_column]])),
          site_label = as.character(data[[streamcurves_site_label_column]])
        ))
      }

      tibble::tibble(
        site_id = seq_len(nrow(data)),
        site_label = paste("Site", seq_len(nrow(data)))
      )
    }

    exploratory_site_choices <- function(data = rv$data) {
      req(!is.null(data))
      site_df <- site_identity_frame(data)
      stats::setNames(
        as.character(site_df$site_id),
        paste0(site_df$site_id, " - ", site_df$site_label)
      )
    }

    cached_phase1_quick_mask_ids <- function(metric = scatter_context_metric()) {
      if (is.null(metric) || identical(metric, "")) {
        return(integer(0))
      }

      cached_ids <- rv$metric_phase_cache[[metric]]$phase1_quick_mask_site_ids %||% integer(0)
      cached_ids <- suppressWarnings(as.integer(cached_ids))
      sort(unique(cached_ids[!is.na(cached_ids)]))
    }

    phase1_quick_mask_ids <- reactive({
      ids <- suppressWarnings(as.integer(input$phase1_quick_mask_sites %||% integer(0)))
      sort(unique(ids[!is.na(ids)]))
    })

    scatter_metric_choices <- function(current_metric = scatter_context_metric()) {
      req(!is.null(rv$data))
      keys <- names(rv$metric_config)
      keys <- keys[vapply(keys, function(mk) {
        cfg <- rv$metric_config[[mk]]
        col_name <- cfg$column_name %||% NA_character_
        !identical(mk, current_metric) &&
          !identical(cfg$metric_family, "categorical") &&
          !is.na(col_name) &&
          col_name %in% names(rv$data)
      }, logical(1))]

      labels <- vapply(keys, metric_display_name, character(1))
      ordered <- keys[order(labels, keys)]
      stats::setNames(ordered, labels[match(ordered, keys)])
    }

    scatter_strat_choices <- function(compare_metric, current_metric = scatter_context_metric()) {
      allowed_compare <- get_metric_allowed_strats(rv, compare_metric)
      stats::setNames(
        c("none", allowed_compare),
        c("None", vapply(allowed_compare, function(sk) get_strat_display_name(rv, sk), character(1)))
      )
    }

    scatter_strat_values <- function(strat_key) {
      if (is.null(strat_key) || identical(strat_key, "none") || identical(strat_key, "")) {
        return(rep(NA_character_, nrow(rv$data)))
      }

      sc <- rv$strat_config[[strat_key]]
      if (is.null(sc)) {
        return(rep(NA_character_, nrow(rv$data)))
      }

      if (!is.null(sc$column_name) && sc$column_name %in% names(rv$data)) {
        return(as.character(rv$data[[sc$column_name]]))
      }

      if (identical(sc$type, "paired")) {
        primary_key <- sc$primary %||% NA_character_
        secondary_key <- sc$secondary %||% NA_character_
        primary_col <- rv$strat_config[[primary_key]]$column_name %||% primary_key
        secondary_col <- rv$strat_config[[secondary_key]]$column_name %||% secondary_key

        if (!(primary_col %in% names(rv$data)) || !(secondary_col %in% names(rv$data))) {
          return(rep(NA_character_, nrow(rv$data)))
        }

        primary_vals <- as.character(rv$data[[primary_col]])
        secondary_vals <- as.character(rv$data[[secondary_col]])
        return(ifelse(
          is.na(primary_vals) | is.na(secondary_vals),
          NA_character_,
          paste(primary_vals, secondary_vals, sep = " | ")
        ))
      }

      rep(NA_character_, nrow(rv$data))
    }

    build_empty_scatterplot <- function(message_text) {
      streamcurves_plotly_layout(
        plotly::plot_ly(type = "scatter", mode = "markers"),
        profile = "large_analysis",
        xaxis = streamcurves_plotly_axis_defaults(profile = "large_analysis", visible = FALSE),
        yaxis = streamcurves_plotly_axis_defaults(profile = "large_analysis", visible = FALSE),
        annotations = list(list(
          text = message_text,
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = streamcurves_get_plot_font_profile("large_analysis")$annotation, color = "#5b6570")
        )),
        margin = list(l = 40, r = 40, t = 20, b = 40)
      )
    }

    build_scatter_comparison <- function(compare_metric, strat_key = "none") {
      current_metric <- scatter_context_metric()
      current_col <- metric_column_name(current_metric)
      compare_col <- metric_column_name(compare_metric)

      if (is.na(current_col) || is.na(compare_col) ||
          !(current_col %in% names(rv$data)) || !(compare_col %in% names(rv$data))) {
        return(list(
          metric = compare_metric,
          stratification = strat_key,
          traces = list(),
          warning = paste0(metric_display_name(compare_metric), " is missing a usable data column.")
        ))
      }

      scatter_df <- tibble::tibble(
        current_value = rv$data[[current_col]],
        compare_value = rv$data[[compare_col]],
        site_id = site_identity_frame(rv$data)$site_id,
        site_label = site_identity_frame(rv$data)$site_label
      )

      quick_mask_ids <- phase1_quick_mask_ids()
      if (length(quick_mask_ids) > 0) {
        scatter_df <- scatter_df |>
          dplyr::filter(!(.data$site_id %in% quick_mask_ids))
      }

      if (!identical(strat_key, "none")) {
        scatter_df$strat_label <- scatter_strat_values(strat_key)
        scatter_df <- scatter_df |>
          dplyr::filter(!is.na(current_value), !is.na(compare_value), !is.na(strat_label), nzchar(strat_label))
      } else {
        scatter_df <- scatter_df |>
          dplyr::filter(!is.na(current_value), !is.na(compare_value))
        scatter_df$strat_label <- "All sites"
      }

      if (nrow(scatter_df) == 0) {
        return(list(
          metric = compare_metric,
          stratification = strat_key,
          traces = list(),
          warning = paste0(
            metric_display_name(compare_metric),
            " has no complete paired rows with ",
            metric_display_name(current_metric),
            "."
          )
        ))
      }

      trace_levels <- unique(scatter_df$strat_label)
      trace_levels <- trace_levels[!is.na(trace_levels)]

      traces <- lapply(trace_levels, function(level_label) {
        trace_df <- scatter_df |>
          dplyr::filter(strat_label == level_label)

        if (identical(strat_key, "none")) {
          trace_name <- metric_display_name(compare_metric)
          hover_text <- paste0(
            "Site ID: ", trace_df$site_id,
            "<br>Site Label: ", trace_df$site_label,
            "<br>",
            metric_display_name(current_metric), ": ", format(signif(trace_df$current_value, 4), trim = TRUE),
            "<br>", metric_display_name(compare_metric), ": ", format(signif(trace_df$compare_value, 4), trim = TRUE)
          )
        } else {
          trace_name <- paste0(
            metric_display_name(compare_metric),
            " - ",
            get_strat_display_name(rv, strat_key),
            ": ",
            level_label
          )
          hover_text <- paste0(
            "Site ID: ", trace_df$site_id,
            "<br>Site Label: ", trace_df$site_label,
            "<br>",
            metric_display_name(current_metric), ": ", format(signif(trace_df$current_value, 4), trim = TRUE),
            "<br>", metric_display_name(compare_metric), ": ", format(signif(trace_df$compare_value, 4), trim = TRUE),
            "<br>", get_strat_display_name(rv, strat_key), ": ", level_label
          )
        }

        list(
          data = trace_df,
          name = trace_name,
          hover_text = hover_text
        )
      })

      list(
        metric = compare_metric,
        stratification = strat_key,
        traces = traces,
        warning = NULL
      )
    }

    default_scatter_panel <- function(panel_id = "panel_1", panel_index = 1L,
                                      current_metric = scatter_context_metric()) {
      available_metrics <- unname(scatter_metric_choices(current_metric))
      compare_metric <- if (length(available_metrics) > 0) available_metrics[[1]] else NULL

      list(
        id = panel_id,
        label = paste("Panel", panel_index),
        current_on_x = TRUE,
        compare_metric = compare_metric,
        strat_key = "none"
      )
    }

    next_scatter_panel_index <- function(panels) {
      if (length(panels) == 0) {
        return(1L)
      }

      panel_ids <- vapply(panels, function(panel) panel$id %||% "", character(1))
      numeric_ids <- suppressWarnings(as.integer(sub("^panel_", "", panel_ids)))
      max(c(0L, numeric_ids[!is.na(numeric_ids)]), na.rm = TRUE) + 1L
    }

    scatter_panel_compare_choices <- function(current_metric = scatter_context_metric()) {
      scatter_metric_choices(current_metric)
    }

    scatter_panel_strat_key_choices <- function(panel, current_metric = scatter_context_metric()) {
      compare_metric <- panel$compare_metric %||% panel$primary_metric %||% NULL
      if (is.null(compare_metric) || !nzchar(compare_metric %||% "")) {
        return(stats::setNames("none", "None"))
      }

      scatter_strat_choices(compare_metric, current_metric)
    }

    normalize_scatter_panel <- function(panel, current_metric = scatter_context_metric()) {
      available_metrics <- unname(scatter_metric_choices(current_metric))
      panel_id <- as.character(panel$id %||% "panel_1")
      panel_label <- trimws(as.character(panel$label %||% "Panel 1"))
      if (!nzchar(panel_label)) {
        panel_label <- "Panel 1"
      }

      compare_metric <- as.character(panel$compare_metric %||% panel$primary_metric %||% "")
      if (!nzchar(compare_metric) || !compare_metric %in% available_metrics) {
        compare_metric <- if (length(available_metrics) > 0) available_metrics[[1]] else NA_character_
      }
      if (is.na(compare_metric)) {
        compare_metric <- NULL
      }

      strat_panel <- list(compare_metric = compare_metric)
      strat_choices <- unname(scatter_panel_strat_key_choices(strat_panel, current_metric))
      strat_key <- as.character(panel$strat_key %||% "none")
      if (!nzchar(strat_key) || !strat_key %in% strat_choices) {
        strat_key <- "none"
      }

      list(
        id = panel_id,
        label = panel_label,
        current_on_x = isTRUE(panel$current_on_x %||% TRUE),
        compare_metric = compare_metric,
        strat_key = strat_key
      )
    }

    normalize_scatter_panel_collection <- function(panels,
                                                   current_metric = scatter_context_metric()) {
      if (length(panels) == 0) {
        panels <- list(default_scatter_panel(current_metric = current_metric))
      }

      normalized <- vector("list", length(panels))
      next_index <- next_scatter_panel_index(panels)
      seen_ids <- character(0)

      for (idx in seq_along(panels)) {
        panel <- panels[[idx]]
        if (is.null(panel$label) || !nzchar(trimws(panel$label %||% ""))) {
          panel$label <- paste("Panel", idx)
        }
        panel$id <- as.character(panel$id %||% paste0("panel_", idx))
        if (!nzchar(panel$id) || panel$id %in% seen_ids) {
          panel$id <- paste0("panel_", next_index)
          next_index <- next_index + 1L
        }
        normalized[[idx]] <- normalize_scatter_panel(panel, current_metric)
        seen_ids <- c(seen_ids, normalized[[idx]]$id)
      }

      list(panels = normalized)
    }

    scatter_panel_shells_from_panels <- function(panels) {
      if (length(panels) == 0) {
        return(list())
      }

      lapply(seq_along(panels), function(idx) {
        panel <- panels[[idx]] %||% list()
        panel_label <- trimws(as.character(panel$label %||% paste("Panel", idx)))
        if (!nzchar(panel_label)) {
          panel_label <- paste("Panel", idx)
        }

        list(
          id = as.character(panel$id %||% paste0("panel_", idx)),
          label = panel_label
        )
      })
    }

    scatter_panel_toggle_input_id <- function(panel_id) {
      paste0("scatter_panel_toggle_", panel_id)
    }

    scatter_panel_compare_input_id <- function(panel_id) {
      paste0("scatter_panel_compare_", panel_id)
    }

    scatter_panel_strat_key_input_id <- function(panel_id) {
      paste0("scatter_panel_strat_key_", panel_id)
    }

    scatter_panel_messages_output_id <- function(panel_id) {
      paste0("scatter_panel_messages_", panel_id)
    }

    scatter_panel_plot_output_id <- function(panel_id) {
      paste0("scatter_panel_plot_", panel_id)
    }

    scatter_panel_tabs_input_id <- "scatter_panel_tabs"
    scatter_panel_shells <- reactiveVal(list())
    scatter_panel_shell_version <- reactiveVal(0L)
    scatter_panel_store <- reactiveVal(list())
    scatter_panel_active_tab <- reactiveVal(NULL)
    scatter_panel_loaded_context <- reactiveVal(NULL)
    scatter_panel_input_observers <- reactiveVal(list())

    # IMPORTANT: Scatter comparison controls previously got stuck in reset loops
    # because ordinary edits persisted into rv$metric_phase_cache and phase1_page
    # renderUI was reading cached scatter selections directly. That remounted the
    # entire page, rebuilt the compare/strat/toggle controls, and the rebuilt
    # observers immediately wrote stale browser values back into scatter_panel_store().
    #
    # To avoid reintroducing that bug:
    # - keep compare_metric/strat_key/current_on_x in scatter_panel_store() only
    # - keep shells and active tab structural-only
    # - seed quick-mask selections via updatePickerInput() after context load
    # - keep ignoreInit = TRUE on compare/strat/toggle observers
    # - never add server-side compare/toggle update paths after initial render
    resolve_scatter_panel_active_tab <- function(panel_defs = shiny::isolate(scatter_panel_shells()),
                                                 candidates = list(
                                                   shiny::isolate(input[[scatter_panel_tabs_input_id]]),
                                                   shiny::isolate(scatter_panel_active_tab())
                                                 )) {
      if (length(panel_defs) == 0) {
        return(NULL)
      }

      panel_ids <- vapply(panel_defs, `[[`, character(1), "id")
      candidate_ids <- unlist(candidates, use.names = FALSE)
      candidate_ids <- as.character(candidate_ids %||% character(0))
      candidate_ids <- candidate_ids[!is.na(candidate_ids) & nzchar(candidate_ids)]

      for (candidate_id in candidate_ids) {
        if (candidate_id %in% panel_ids) {
          return(candidate_id)
        }
      }

      panel_ids[[1]]
    }

    set_scatter_panel_active_tab <- function(panel_id = NULL,
                                             panel_defs = shiny::isolate(scatter_panel_shells())) {
      active_id <- resolve_scatter_panel_active_tab(
        panel_defs = panel_defs,
        candidates = list(panel_id)
      )
      scatter_panel_active_tab(active_id)
      invisible(active_id)
    }

    scatter_panel_context_key <- function(metric = scatter_context_metric()) {
      if (is.null(metric) || identical(metric, "") || is.null(rv$data)) {
        return(NULL)
      }

      data_signature <- rv$data_fingerprint %||% paste(nrow(rv$data), ncol(rv$data), sep = "x")
      paste(
        metric,
        rv$config_version %||% 0L,
        rv$app_reset_nonce %||% 0L,
        data_signature,
        sep = "::"
      )
    }

    persist_scatter_panel_state <- function(current_metric = scatter_context_metric(),
                                            panels = scatter_panel_store()) {
      if (is.null(current_metric) || identical(current_metric, "") || length(panels) == 0) {
        return(invisible(NULL))
      }

      if (is.null(rv$metric_phase_cache[[current_metric]])) {
        rv$metric_phase_cache[[current_metric]] <- list()
      }

      rv$metric_phase_cache[[current_metric]]$phase1_scatter_panels <- deep_copy_value(panels)
      invisible(NULL)
    }

    set_scatter_panel_state <- function(panels,
                                        current_metric = scatter_context_metric(),
                                        update_shell = TRUE) {
      normalized <- normalize_scatter_panel_collection(panels, current_metric)
      if (isTRUE(update_shell)) {
        scatter_panel_shells(deep_copy_value(scatter_panel_shells_from_panels(normalized$panels)))
        scatter_panel_shell_version(shiny::isolate(scatter_panel_shell_version()) + 1L)
      }

      if (!identical(shiny::isolate(scatter_panel_store()), normalized$panels)) {
        scatter_panel_store(deep_copy_value(normalized$panels))
      }

      persist_scatter_panel_state(
        current_metric = current_metric,
        panels = normalized$panels
      )

      normalized
    }

    snapshot_scatter_panels <- function() {
      panels <- shiny::isolate(scatter_panel_store())
      normalize_scatter_panel_collection(panels, scatter_context_metric())
    }

    get_scatter_panel_ui_panel <- function(panel, current_metric = scatter_context_metric()) {
      live_panel <- get_scatter_panel_live_panel(panel$id %||% "")
      normalize_scatter_panel(live_panel %||% panel, current_metric)
    }

    get_scatter_panel_live_panel <- function(panel_id) {
      panels <- shiny::isolate(scatter_panel_store())
      if (length(panels) == 0) {
        return(NULL)
      }

      panel_ids <- vapply(panels, `[[`, character(1), "id")
      panel_index <- match(panel_id %||% "", panel_ids, nomatch = 0L)
      if (panel_index == 0L) {
        return(NULL)
      }

      panels[[panel_index]]
    }

    apply_scatter_panel_edit <- function(panel_id, field, value,
                                         current_metric = scatter_context_metric()) {
      panels <- deep_copy_value(shiny::isolate(scatter_panel_store()))
      if (length(panels) == 0) {
        return(invisible(NULL))
      }

      panel_ids <- vapply(panels, `[[`, character(1), "id")
      panel_index <- match(panel_id, panel_ids, nomatch = 0L)
      if (panel_index == 0L) {
        return(invisible(NULL))
      }

      panels[[panel_index]][[field]] <- value
      panels[[panel_index]] <- normalize_scatter_panel(
        panels[[panel_index]],
        current_metric = current_metric
      )
      set_scatter_panel_state(
        panels = panels,
        current_metric = current_metric,
        update_shell = FALSE
      )

      invisible(NULL)
    }

    prune_scatter_panel_input_observers <- function(active_panel_ids = character(0), reset_all = FALSE) {
      registry <- shiny::isolate(scatter_panel_input_observers())
      if (length(registry) == 0) {
        return(invisible(NULL))
      }

      remove_ids <- if (isTRUE(reset_all)) {
        names(registry)
      } else {
        setdiff(names(registry), active_panel_ids)
      }

      for (panel_id in remove_ids) {
        observers <- registry[[panel_id]] %||% list()
        lapply(observers, function(observer) {
          if (!is.null(observer)) {
            try(observer$destroy(), silent = TRUE)
          }
          invisible(NULL)
        })
        registry[[panel_id]] <- NULL
      }

      keep_ids <- setdiff(names(registry), remove_ids)
      registry <- registry[keep_ids]
      scatter_panel_input_observers(registry)
      invisible(NULL)
    }

    scatter_panel_state_map <- reactive({
      panels <- scatter_panel_store()
      # Force scatter panel state to invalidate when the temporary site mask changes.
      phase1_quick_mask_ids()
      if (length(panels) == 0) {
        return(list())
      }

      states <- lapply(panels, build_scatter_panel_state)
      stats::setNames(states, vapply(states, `[[`, character(1), "id"))
    })

    get_scatter_panel_state <- function(panel_id) {
      panel_map <- scatter_panel_state_map()
      panel_map[[panel_id]] %||% NULL
    }

    load_scatter_panel_state <- function(metric = scatter_context_metric(),
                                         force = FALSE,
                                         context_key = scatter_panel_context_key(metric)) {
      req(metric, !is.null(rv$data))

      if (!isTRUE(force) &&
          identical(shiny::isolate(scatter_panel_loaded_context()), context_key) &&
          length(shiny::isolate(scatter_panel_store())) > 0) {
        return(invisible(snapshot_scatter_panels()))
      }

      cache_entry <- rv$metric_phase_cache[[metric]] %||% list()
      panel_snapshot <- cache_entry$phase1_scatter_panels %||% list()
      normalized <- set_scatter_panel_state(
        panels = panel_snapshot,
        current_metric = metric,
        update_shell = TRUE
      )
      prune_scatter_panel_input_observers(reset_all = TRUE)
      set_scatter_panel_active_tab(
        panel_id = normalized$panels[[1]]$id %||% NULL,
        panel_defs = scatter_panel_shells_from_panels(normalized$panels)
      )
      scatter_panel_loaded_context(context_key)
      invisible(normalized)
    }

    build_scatter_panel_state <- function(panel) {
      current_metric <- scatter_context_metric()
      available_metrics <- unname(scatter_metric_choices(current_metric))
      compare_metric <- panel$compare_metric %||% NULL
      warnings <- character(0)

      if (length(available_metrics) == 0) {
        warnings <- c(
          warnings,
          "No eligible comparison metrics are available for this metric."
        )
      }

      if (!is.null(compare_metric) && nzchar(compare_metric %||% "")) {
        strat_choices <- scatter_panel_strat_key_choices(panel, current_metric)
        if (length(unname(strat_choices)) <= 1L) {
          warnings <- c(
            warnings,
            paste0(
              "No stratifications are available for ",
              metric_display_name(compare_metric),
              "."
            )
          )
        }
      }

      comparison <- if (!is.null(compare_metric) && nzchar(compare_metric %||% "")) {
        build_scatter_comparison(compare_metric, panel$strat_key %||% "none")
      } else {
        NULL
      }

      comparison_warnings <- comparison$warning %||% character(0)
      comparison_warnings <- comparison_warnings[!is.na(comparison_warnings) & nzchar(comparison_warnings)]
      warnings <- unique(c(warnings, comparison_warnings))

      list(
        panel = panel,
        id = panel$id,
        label = panel$label,
        current_metric = current_metric,
        current_on_x = isTRUE(panel$current_on_x),
        compare_metric = compare_metric,
        strat_key = panel$strat_key,
        available_metrics = available_metrics,
        comparison = if (!is.null(comparison) && length(comparison$traces) > 0) comparison else NULL,
        warnings = warnings
      )
    }

    scatter_panel_plot_height_px <- function(panel = NULL) {
      if (isTRUE(dialog_mode)) {
        380L
      } else {
        430L
      }
    }

    build_scatter_panel_messages <- function(panel_state) {
      blocks <- list()

      if (length(panel_state$available_metrics) == 0) {
        blocks <- c(blocks, list(
          div(
            class = "alert alert-info py-2",
            "No eligible comparison metrics are available for this metric."
          )
        ))
      } else if (is.null(panel_state$compare_metric) || !nzchar(panel_state$compare_metric %||% "")) {
        blocks <- c(blocks, list(
          div(
            class = "alert alert-info py-2",
            "Select a comparison metric to start this panel."
          )
        ))
      }

      if (length(panel_state$warnings) > 0) {
        blocks <- c(blocks, list(
          div(
            class = "alert alert-warning py-2",
            tags$ul(class = "mb-0", lapply(panel_state$warnings, function(msg) tags$li(msg)))
          )
        ))
      }

      if (length(blocks) == 0) {
        return(NULL)
      }

      tagList(blocks)
    }

    build_scatter_panel_ui <- function(panel) {
      current_metric <- scatter_context_metric()
      panel_values <- get_scatter_panel_ui_panel(panel, current_metric)

      div(
        class = "scatterplot-panel-tab",
        div(
          class = "scatterplot-panel-controls",
          selectInput(
            ns(scatter_panel_compare_input_id(panel$id)),
            "Comparison metric:",
            choices = scatter_panel_compare_choices(current_metric),
            selected = panel_values$compare_metric %||% NULL,
            width = "100%"
          ),
          selectInput(
            ns(scatter_panel_strat_key_input_id(panel$id)),
            "Stratify by comparison metric:",
            choices = scatter_panel_strat_key_choices(panel_values, current_metric),
            selected = panel_values$strat_key %||% "none",
            width = "100%"
          ),
          div(
            class = "scatterplot-panel-toggle",
            checkboxInput(
              ns(scatter_panel_toggle_input_id(panel$id)),
              "Current metric on X axis",
              value = isTRUE(panel_values$current_on_x),
              width = "100%"
            )
          )
        ),
        uiOutput(ns(scatter_panel_messages_output_id(panel$id))),
        plotly::plotlyOutput(
          ns(scatter_panel_plot_output_id(panel$id)),
          width = "100%",
          height = paste0(scatter_panel_plot_height_px(panel_values), "px")
        )
      )
    }

    build_scatter_panel_plot <- function(state) {
      if (length(state$available_metrics) == 0) {
        return(build_empty_scatterplot("No eligible comparison metrics are available for this metric."))
      }

      if (is.null(state$compare_metric) || !nzchar(state$compare_metric %||% "")) {
        return(build_empty_scatterplot("Select a comparison metric to start this panel."))
      }

      if (is.null(state$comparison)) {
        return(build_empty_scatterplot("No complete paired rows are available for the selected comparison."))
      }

      current_metric <- state$current_metric
      compare_metric <- state$compare_metric
      current_on_x <- isTRUE(state$current_on_x)
      fig <- plotly::plot_ly()

      for (trace in state$comparison$traces) {
        fig <- fig |>
          plotly::add_trace(
            data = trace$data,
            x = if (current_on_x) ~current_value else ~compare_value,
            y = if (current_on_x) ~compare_value else ~current_value,
            type = "scatter",
            mode = "markers",
            name = trace$name,
            text = trace$hover_text,
            hoverinfo = "text",
            marker = list(size = 7, opacity = 0.72),
            showlegend = TRUE
          )
      }

      streamcurves_plotly_layout(
        fig,
        profile = "large_analysis",
        xaxis = streamcurves_plotly_axis_defaults(
          profile = "large_analysis",
          title_text = if (current_on_x) metric_axis_label(current_metric) else metric_axis_label(compare_metric),
          standoff = 18
        ),
        yaxis = streamcurves_plotly_axis_defaults(
          profile = "large_analysis",
          title_text = if (current_on_x) metric_axis_label(compare_metric) else metric_axis_label(current_metric),
          standoff = 18
        ),
        legend = streamcurves_plotly_legend_defaults(
          profile = "large_analysis",
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 1.08,
          yanchor = "bottom"
        ),
        hovermode = "closest",
        margin = list(l = 82, r = 24, t = 58, b = 48)
      )
    }

    ## ── Data gate: show alert or full page ────────────────────────────────────
    output$phase1_page <- renderUI({
      if (!isTRUE(phase1_workspace_active())) {
        return(NULL)
      }

      if (is.null(rv$data)) return(no_data_alert())

      phase1_body <- tagList(
        ## ── Sidebar ─────────────────────────────────────────────────────────────
        ## Sidebar omitted in dialog mode.



          ## Bulk screening — button only, no heading or description





        ## ── Main content ────────────────────────────────────────────────────────
        tagList(
          ## Instructional card
          explanation_card(
            "Exploratory Analysis",
            p("Which stratifications show meaningful group separation for this metric?"),
            p("Run screening to test for statistical significance (Kruskal-Wallis) and
               effect sizes (epsilon-squared) in a single pass. Review boxplots and
               summary statistics to identify stratifications that may be useful for
               downstream analysis."),
            p(tags$strong("Effect size thresholds:"), " <0.01 negligible, 0.01\u20130.06 small,
               0.06\u20130.14 medium, >0.14 large.")
          ),

          card(
            fill = FALSE,
            class = "phase1-quick-mask-card",
            card_body(
              class = "py-2 phase1-quick-mask-card-body",
              shinyWidgets::pickerInput(
                ns("phase1_quick_mask_sites"),
                "Temporarily hide sites in exploratory plots:",
                choices = exploratory_site_choices(rv$data),
                # Keep initial selection empty here. Reading cached quick-mask
                # state directly in phase1_page renderUI used to invalidate the
                # entire page on ordinary scatter-panel edits and remount the
                # controls that later reverted.
                selected = character(0),
                multiple = TRUE,
                options = shinyWidgets::pickerOptions(
                  container = picker_container(),
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  selectedTextFormat = "count > 4",
                  countSelectedText = "{0} of {1} hidden",
                  noneSelectedText = "No exploratory site masks"
                )
              ),
              tags$small(
                class = "text-muted d-block mt-2",
                "Affects exploratory scatterplots, histograms, and boxplots only. ",
                "It does not recompute screening statistics or effect sizes."
              )
            )
          ),

          card(
            fill = FALSE,
            class = "scatterplot-comparison-card",
            card_header(
              class = "d-flex flex-wrap align-items-center justify-content-between gap-2",
              tags$span("Scatterplot Comparison Panels"),
              uiOutput(ns("scatter_panel_actions"))
            ),
            card_body(
              class = "scatterplot-comparison-card-body",
              uiOutput(ns("scatter_panel_list_ui"))
            )
          ),

          ## Metric control bar (picker + info + strat controls)
          card(
            fill = FALSE,
            card_body(
              class = "py-2",
              layout_column_wrap(
                width = 1 / 2,
                ## Left: metric selector + info table
                div(
                  if (!isTRUE(dialog_mode)) uiOutput(ns("metric_picker")),
                  uiOutput(ns("metric_info_table"))
                ),
                ## Right: stratifications to test + run/reset buttons
                uiOutput(ns("metric_info_controls"))
              )
            )
          ),

          ## Compact precheck summary
          card(
            fill = FALSE,
            card_header("Metric Summary"),
            card_body(
              layout_column_wrap(
                width = 1 / 2,
                heights_equal = "row",
                DT::DTOutput(ns("precheck_stats")),
                plotOutput(ns("precheck_hist"), height = "200px")
              )
            )
          ),

          uiOutput(ns("artifact_status")),

          ## Results section
          uiOutput(ns("results_ui")),

          ## Candidate marking table
          uiOutput(ns("candidate_ui")),

          ## Save button
          uiOutput(ns("save_ui"))
        )
      )

      if (isTRUE(dialog_mode)) {
        return(div(class = "workspace-phase-body", phase1_body))
      }

      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          title = "Exploratory Analysis",
          uiOutput(ns("bulk_progress")),
          actionButton(ns("run_all_screening"), "Run exploratory screening for all metrics",
                       class = "btn btn-primary btn-sm w-100",
                       icon = icon("play-circle")),
          actionButton(ns("reset_all_metrics"), "Reset All Metrics",
                       class = "btn btn-outline-secondary btn-sm w-100 mt-1",
                       icon = icon("arrows-rotate"))
        ),
        phase1_body
      )
    })
    outputOptions(output, "phase1_page", suspendWhenHidden = FALSE)

    ## ── Build metric picker choices (grouped by family) ───────────────────────
    output$metric_picker <- renderUI({
      mc <- rv$metric_config

      families <- list()
      for (mk in names(mc)) {
        fam <- mc[[mk]]$metric_family
        if (fam == "categorical") next
        fam_label <- switch(fam,
          continuous  = "Continuous",
          proportion  = "Proportion",
          count       = "Count",
          fam
        )
        families[[fam_label]] <- c(families[[fam_label]], setNames(mk, mc[[mk]]$display_name))
      }

      selectInput(
        ns("metric_select"), "Select Metric:",
        choices = families,
        selected = rv$current_metric %||% "perRiffle"
      )
    })

    ## ── Metric change observer ────────────────────────────────────────────────
    observeEvent(input$metric_select, {
      old_metric <- prev_metric()
      new_metric <- input$metric_select

      if (!is.null(old_metric) && identical(old_metric, new_metric)) {
        return(invisible(NULL))
      }

      ## SAVE outgoing metric's state
      if (!is.null(old_metric) && old_metric != "") {
        save_metric_phase_state(rv, old_metric)
      }

      ## Switch
      rv$current_metric <- new_metric
      prev_metric(new_metric)

      ## RESTORE incoming metric's state
      restore_metric_phase_state(rv, new_metric)
      load_screening_results(new_metric)
      if (!isTRUE(dialog_mode)) {
        refresh_screening_artifacts(new_metric, defer = FALSE)
      }
    }, ignoreInit = TRUE)

    ## ── Metric info table (left column) ────────────────────────────────────────
    output$metric_info_table <- renderUI({
      req(rv$current_metric)
      mc <- rv$metric_config[[rv$current_metric]]
      req(mc)

      precheck <- rv$precheck_df |>
        dplyr::filter(metric == rv$current_metric)

      completed <- rv$current_metric %in% names(rv$completed_metrics)

      div(
        class = "metric-info-card",
        tags$table(
          class = "table table-sm mb-0",
          tags$tbody(
            tags$tr(
              tags$td(class = "info-label", "Family:"),
              tags$td(class = "info-value", mc$metric_family)
            ),
            tags$tr(
              tags$td(class = "info-label", "Units:"),
              tags$td(class = "info-value", mc$units)
            ),
            tags$tr(
              tags$td(class = "info-label", "Direction:"),
              tags$td(class = "info-value",
                       if (isTRUE(mc$higher_is_better)) "Higher is better"
                       else if (isFALSE(mc$higher_is_better)) "Lower is better"
                       else "Neutral")
            ),
            if (nrow(precheck) > 0) {
              tagList(
                tags$tr(
                  tags$td(class = "info-label", "n obs:"),
                  tags$td(class = "info-value", precheck$n_obs)
                ),
                tags$tr(
                  tags$td(class = "info-label", "Status:"),
                  tags$td(status_badge(precheck$precheck_status))
                )
              )
            }
          )
        ),
        if (completed) {
          div(class = "mt-1", status_badge("pass", "COMPLETED"))
        }
      )
    })

    ## ── Metric info controls (right column) ─────────────────────────────────────
    output$metric_info_controls <- renderUI({
      req(rv$current_metric)
      mc <- rv$metric_config[[rv$current_metric]]
      req(mc)

      ## Build strat checkbox choices
      allowed <- get_metric_allowed_strats(rv, rv$current_metric)
      choices <- setNames(allowed, sapply(allowed, function(sk) {
        sc <- rv$strat_config[[sk]]
        if (!is.null(sc)) sc$display_name else sk
      }))

      div(
        shinyWidgets::pickerInput(
          ns("strat_checks"), "Stratifications to Test:",
          choices = choices, selected = allowed, multiple = TRUE,
          options = shinyWidgets::pickerOptions(
            container = picker_container(),
            size = 8,
            actionsBox = TRUE, liveSearch = TRUE,
            selectedTextFormat = "count > 3",
            countSelectedText = "{0} of {1} selected",
            noneSelectedText = "None selected"
          )
        ),
        actionButton(ns("run_screening"), "Run this metric screening",
                     class = "btn btn-primary btn-sm w-100",
                     icon = icon("play")),
        actionButton(ns("reset_metric"), "Reset This Metric",
                     class = "btn btn-outline-danger btn-sm w-100 mt-1",
                     icon = icon("arrow-rotate-left"))
      )
    })

    ## ── Compact precheck ──────────────────────────────────────────────────────
    observeEvent(list(rv$current_metric, rv$workspace_modal_metric, rv$data, rv$config_version, rv$app_reset_nonce), {
      current_metric <- scatter_context_metric()
      context_key <- scatter_panel_context_key(current_metric)

      if (is.null(rv$data) || is.null(current_metric) || identical(current_metric, "")) {
        prune_scatter_panel_input_observers(reset_all = TRUE)
        scatter_panel_shells(list())
        scatter_panel_shell_version(shiny::isolate(scatter_panel_shell_version()) + 1L)
        scatter_panel_store(list())
        scatter_panel_active_tab(NULL)
        scatter_panel_loaded_context(NULL)
        return(invisible(NULL))
      }

      load_scatter_panel_state(current_metric, context_key = context_key)
      shinyWidgets::updatePickerInput(
        session,
        "phase1_quick_mask_sites",
        choices = exploratory_site_choices(rv$data),
        selected = as.character(cached_phase1_quick_mask_ids(current_metric))
      )
      invisible(NULL)
    }, ignoreInit = FALSE)

    observe({
      current_metric <- scatter_context_metric()
      req(current_metric, !is.null(rv$data))
      quick_mask_ids <- phase1_quick_mask_ids()
      cache_entry <- shiny::isolate(rv$metric_phase_cache[[current_metric]] %||% list())
      cached_ids <- suppressWarnings(as.integer(cache_entry$phase1_quick_mask_site_ids %||% integer(0)))
      cached_ids <- sort(unique(cached_ids[!is.na(cached_ids)]))
      if (identical(cached_ids, quick_mask_ids)) {
        return(invisible(NULL))
      }

      cache_entry$phase1_quick_mask_site_ids <- quick_mask_ids
      rv$metric_phase_cache[[current_metric]] <- cache_entry
      invisible(NULL)
    })

    register_scatter_panel_input_observers <- function(panel_id) {
      registry <- shiny::isolate(scatter_panel_input_observers())
      if (!is.null(registry[[panel_id]])) {
        return(invisible(NULL))
      }

      current_panel_id <- panel_id
      strat_input_id <- scatter_panel_strat_key_input_id(current_panel_id)

      registry[[current_panel_id]] <- list(
        # Remounted controls must not write their initial browser value back
        # into scatter_panel_store(). ignoreInit = TRUE is part of the fix for
        # the historical compare/strat/toggle reset loop.
        toggle = observeEvent(input[[scatter_panel_toggle_input_id(current_panel_id)]], {
          value <- input[[scatter_panel_toggle_input_id(current_panel_id)]]
          if (is.null(value)) {
            return(invisible(NULL))
          }
          current_panel <- get_scatter_panel_live_panel(current_panel_id)
          normalized_value <- isTRUE(value)
          if (identical(isTRUE(current_panel$current_on_x), normalized_value)) {
            return(invisible(NULL))
          }
          apply_scatter_panel_edit(
            panel_id = current_panel_id,
            field = "current_on_x",
            value = normalized_value
          )
        }, ignoreNULL = TRUE, ignoreInit = TRUE),
        compare = observeEvent(input[[scatter_panel_compare_input_id(current_panel_id)]], {
          value <- input[[scatter_panel_compare_input_id(current_panel_id)]] %||% NULL
          if (is.null(value) || !nzchar(as.character(value))) {
            return(invisible(NULL))
          }
          current_panel <- get_scatter_panel_live_panel(current_panel_id)
          if (identical(current_panel$compare_metric %||% NULL, value)) {
            return(invisible(NULL))
          }
          apply_scatter_panel_edit(
            panel_id = current_panel_id,
            field = "compare_metric",
            value = value
          )
          updated_panel <- get_scatter_panel_live_panel(current_panel_id)
          if (!is.null(updated_panel)) {
            shiny::freezeReactiveValue(input, strat_input_id)
            updateSelectInput(
              session,
              strat_input_id,
              choices = scatter_panel_strat_key_choices(updated_panel, scatter_context_metric()),
              selected = updated_panel$strat_key %||% "none"
            )
          }
        }, ignoreNULL = TRUE, ignoreInit = TRUE),
        strat_key = observeEvent(input[[scatter_panel_strat_key_input_id(current_panel_id)]], {
          value <- input[[scatter_panel_strat_key_input_id(current_panel_id)]] %||% NULL
          if (is.null(value) || identical(as.character(value), "")) {
            return(invisible(NULL))
          }
          current_panel <- get_scatter_panel_live_panel(current_panel_id)
          if (identical(current_panel$strat_key %||% "none", value)) {
            return(invisible(NULL))
          }
          apply_scatter_panel_edit(
            panel_id = current_panel_id,
            field = "strat_key",
            value = value
          )
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
      )

      scatter_panel_input_observers(registry)
      invisible(NULL)
    }

    observe({
      panel_ids <- vapply(scatter_panel_shells(), `[[`, character(1), "id")
      if (length(panel_ids) == 0) {
        prune_scatter_panel_input_observers(reset_all = TRUE)
        return(invisible(NULL))
      }

      lapply(panel_ids, register_scatter_panel_input_observers)
      prune_scatter_panel_input_observers(active_panel_ids = panel_ids)
      invisible(NULL)
    })

    output$scatter_panel_actions <- renderUI({
      panel_defs <- scatter_panel_shells()
      active_panel_id <- resolve_scatter_panel_active_tab(
        panel_defs = panel_defs,
        candidates = list(input[[scatter_panel_tabs_input_id]], scatter_panel_active_tab())
      )
      can_remove <- length(panel_defs) > 1L && !is.null(active_panel_id)

      div(
        class = "d-flex flex-wrap align-items-center gap-2",
        actionButton(
          ns("add_scatter_panel"),
          "Add Panel",
          class = "btn btn-outline-primary btn-sm",
          icon = icon("plus")
        ),
        actionButton(
          ns("remove_scatter_panel"),
          "Remove Panel",
          class = "btn btn-outline-danger btn-sm",
          icon = icon("trash"),
          disabled = if (!isTRUE(can_remove)) "disabled" else NULL
        )
      )
    })

    observeEvent(input[[scatter_panel_tabs_input_id]], {
      active_panel_id <- resolve_scatter_panel_active_tab(
        panel_defs = shiny::isolate(scatter_panel_shells()),
        candidates = list(input[[scatter_panel_tabs_input_id]])
      )
      if (!identical(shiny::isolate(scatter_panel_active_tab()), active_panel_id)) {
        scatter_panel_active_tab(active_panel_id)
      }
      invisible(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$add_scatter_panel, {
      current_metric <- scatter_context_metric()
      req(current_metric, !is.null(rv$data))

      snapshot <- snapshot_scatter_panels()
      next_index <- next_scatter_panel_index(snapshot$panels)
      new_panel <- normalize_scatter_panel(
        default_scatter_panel(
          panel_id = paste0("panel_", next_index),
          panel_index = next_index,
          current_metric = current_metric
        ),
        current_metric
      )
      updated_panels <- append(snapshot$panels, list(new_panel))

      set_scatter_panel_state(
        panels = updated_panels,
        current_metric = current_metric,
        update_shell = TRUE
      )
      set_scatter_panel_active_tab(
        panel_id = new_panel$id,
        panel_defs = scatter_panel_shells_from_panels(updated_panels)
      )
      invisible(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$remove_scatter_panel, {
      current_metric <- scatter_context_metric()
      req(current_metric, !is.null(rv$data))

      snapshot <- snapshot_scatter_panels()
      if (length(snapshot$panels) <= 1L) {
        return(invisible(NULL))
      }

      active_panel_id <- resolve_scatter_panel_active_tab(
        panel_defs = scatter_panel_shells_from_panels(snapshot$panels),
        candidates = list(input[[scatter_panel_tabs_input_id]], scatter_panel_active_tab())
      )
      panel_ids <- vapply(snapshot$panels, `[[`, character(1), "id")
      remove_idx <- match(active_panel_id %||% "", panel_ids, nomatch = 0L)
      if (remove_idx == 0L) {
        return(invisible(NULL))
      }

      remaining <- snapshot$panels[-remove_idx]
      next_active_idx <- min(remove_idx, length(remaining))
      next_active_id <- remaining[[next_active_idx]]$id %||% NULL

      set_scatter_panel_state(
        panels = remaining,
        current_metric = current_metric,
        update_shell = TRUE
      )
      set_scatter_panel_active_tab(
        panel_id = next_active_id,
        panel_defs = scatter_panel_shells_from_panels(remaining)
      )
      invisible(NULL)
    }, ignoreInit = TRUE)

    output$scatter_panel_list_ui <- renderUI({
      req(scatter_context_metric(), !is.null(rv$data))

      shell_version <- scatter_panel_shell_version()
      panel_defs <- scatter_panel_shells()
      selected_panel_id <- resolve_scatter_panel_active_tab(panel_defs = panel_defs)
      req(length(panel_defs) > 0)

      invisible(shell_version)
      do.call(
        bslib::navset_tab,
        c(
          list(
            id = ns(scatter_panel_tabs_input_id),
            selected = selected_panel_id
          ),
          lapply(panel_defs, function(panel) {
            bslib::nav_panel(
              title = panel$label,
              value = panel$id,
              build_scatter_panel_ui(panel)
            )
          })
        )
      )
    })

    observe({
      req(scatter_context_metric(), !is.null(rv$data))

      shell_version <- scatter_panel_shell_version()
      panel_ids <- vapply(scatter_panel_shells(), `[[`, character(1), "id")
      if (length(panel_ids) == 0) {
        return(invisible(NULL))
      }

      invisible(shell_version)
      lapply(panel_ids, function(panel_id) {
        local({
          current_panel_id <- panel_id

          output[[scatter_panel_messages_output_id(current_panel_id)]] <- renderUI({
            req(scatter_context_metric(), !is.null(rv$data))
            panel_state <- get_scatter_panel_state(current_panel_id)
            req(panel_state)

            build_scatter_panel_messages(panel_state)
          })

          output[[scatter_panel_plot_output_id(current_panel_id)]] <- plotly::renderPlotly({
            req(scatter_context_metric(), !is.null(rv$data))
            panel_state <- get_scatter_panel_state(current_panel_id)
            req(panel_state)

            plotly::plotly_build(build_scatter_panel_plot(panel_state))
          })
        })
      })

      invisible(NULL)
    })

    output$precheck_stats <- DT::renderDT({
      req(rv$current_metric)
      row <- rv$precheck_df |> dplyr::filter(metric == rv$current_metric)
      req(nrow(row) == 1)

      stats <- data.frame(
        Statistic = c("n", "Missing", "Min", "Median", "Mean", "Max", "SD"),
        Value = c(
          row$n_obs, row$n_missing,
          round(row$min, 2), round(row$median, 2),
          round(row$mean, 2), round(row$max, 2),
          round(row$sd, 2)
        ),
        stringsAsFactors = FALSE
      )

      DT::datatable(stats, options = list(dom = "t", paging = FALSE),
                     rownames = FALSE, class = "compact")
    })

    output$precheck_hist <- renderPlot({
      req(rv$current_metric)
      mc <- rv$metric_config[[rv$current_metric]]
      col <- mc$column_name
      req(col %in% names(rv$data))

      data <- rv$data
      quick_mask_ids <- phase1_quick_mask_ids()
      if (length(quick_mask_ids) > 0) {
        visible_rows <- !(site_identity_frame(data)$site_id %in% quick_mask_ids)
        data <- data[visible_rows, , drop = FALSE]
      }

      vals <- data[[col]]
      vals <- vals[!is.na(vals)]
      req(length(vals) > 0)

      df <- data.frame(value = vals)
      ggplot2::ggplot(df, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(bins = 12, fill = "steelblue", alpha = 0.7, color = "white") +
        ggplot2::labs(x = mc$units, y = "Count") +
        streamcurves_minimal_plot_theme()
    })

    ## ── Run screening + effect size in single pass ────────────────────────────
    screening_results <- reactiveVal(NULL)
    artifacts_loading <- reactiveVal(FALSE)
    artifacts_error <- reactiveVal(NULL)


    set_screening_state <- function(metric = rv$current_metric) {
      state <- get_metric_phase1_display_state(rv, metric)
      if (!is.null(state) && nrow(state$results) > 0) {
        screening_results(state)
      } else {
        screening_results(NULL)
      }
      invisible(NULL)
    }

    sync_analysis_tab_state <- function(status = NULL, request_id = NULL, complete = FALSE) {
      request_id <- request_id %||% shiny::isolate(rv$analysis_tab_request_id %||% NULL)
      if (!isTRUE(dialog_mode) ||
          !identical(workspace_scope, "analysis") ||
          !isTRUE(phase1_workspace_active(isolate_state = TRUE)) ||
          !analysis_tab_request_is_current(rv, request_id)) {
        return(invisible(NULL))
      }

      resolved_status <- status %||% if (is.null(artifacts_error()) || !nzchar(artifacts_error() %||% "")) {
        "ready"
      } else {
        "error"
      }

      set_analysis_tab_status(rv, "exploratory", resolved_status, request_id)
      if (isTRUE(complete)) {
        complete_analysis_tab_preload(rv, "exploratory", resolved_status, request_id)
      }

      invisible(resolved_status)
    }

    refresh_screening_artifacts <- function(metric = rv$current_metric, defer = FALSE, show_progress = TRUE) {
      if (!isTRUE(phase1_workspace_active(isolate_state = TRUE))) {
        return(invisible(FALSE))
      }

      if (!metric_needs_phase1_artifact_refresh(rv, metric)) {
        artifacts_loading(FALSE)
        artifacts_error(NULL)
        set_screening_state(metric)
        return(invisible(FALSE))
      }

      artifacts_loading(TRUE)
      artifacts_error(NULL)

      run_refresh <- function() {
        tryCatch(
          {
            ensure_metric_phase1_artifacts(rv, metric)
            artifacts_error(NULL)
          },
          error = function(e) {
            artifacts_error(conditionMessage(e))
          },
          finally = {
            artifacts_loading(FALSE)
            set_screening_state(metric)
          }
        )
      }

      if (isTRUE(defer)) {
        target_metric <- metric
        session$onFlushed(function() {
          if (is.null(target_metric) || identical(target_metric, "")) return(invisible(NULL))
          shiny::isolate(run_refresh())
          invisible(NULL)
        }, once = TRUE)
      } else if (isTRUE(show_progress)) {
        withProgress(message = "Loading exploratory workspace...", value = 0, {
          incProgress(0, detail = "Regenerating screening plots and pairwise tables...")
          run_refresh()
          incProgress(1)
        })
      } else {
        run_refresh()
      }

      invisible(TRUE)
    }

    load_screening_results <- function(metric = rv$current_metric) {
      artifacts_loading(FALSE)
      artifacts_error(NULL)
      set_screening_state(metric)
      invisible(NULL)
    }

    observeEvent(rv$workspace_modal_ready_nonce, {
      if (isTRUE(dialog_mode) && isTRUE(phase1_workspace_active())) {
        modal_metric <- rv$workspace_modal_metric %||% rv$current_metric
        if (is.null(modal_metric) || identical(modal_metric, "")) return(invisible(NULL))
        load_screening_results(modal_metric)
      }
    }, ignoreInit = TRUE)

    observeEvent(rv$analysis_tab_preload_nonce, {
      if (!isTRUE(dialog_mode) ||
          !identical(workspace_scope, "analysis") ||
          !isTRUE(phase1_workspace_active()) ||
          !identical(rv$analysis_tab_preload_tab %||% NULL, "exploratory")) {
        return(invisible(NULL))
      }

      request_id <- rv$analysis_tab_request_id %||% NULL
      modal_metric <- rv$workspace_modal_metric %||% rv$current_metric
      if (is.null(modal_metric) || identical(modal_metric, "") ||
          !analysis_tab_request_is_current(rv, request_id)) {
        return(invisible(NULL))
      }

      load_screening_results(modal_metric)
      refresh_screening_artifacts(modal_metric, defer = FALSE, show_progress = FALSE)
      sync_analysis_tab_state(request_id = request_id, complete = TRUE)
    }, ignoreInit = TRUE)

    output$artifact_status <- renderUI({
      loading <- artifacts_loading()
      error_text <- artifacts_error()

      if (isTRUE(loading)) {
        return(div(
          class = "alert alert-info d-flex align-items-center gap-2",
          icon("spinner", class = "fa-spin"),
          tags$span("Loading full exploratory details. Summary results are available while plots and pairwise tables regenerate.")
        ))
      }

      if (!is.null(error_text) && nzchar(error_text)) {
        return(div(
          class = "alert alert-danger d-flex justify-content-between align-items-center flex-wrap gap-2",
          tags$span(paste0("Could not load full exploratory details: ", error_text)),
          actionButton(
            ns("retry_artifacts"),
            "Retry details",
            class = "btn btn-outline-danger btn-sm"
          )
        ))
      }

      NULL
    })

    observeEvent(input$retry_artifacts, {
      refresh_screening_artifacts(rv$current_metric, defer = FALSE)
      sync_analysis_tab_state()
    }, ignoreInit = TRUE)

    observeEvent(input$run_screening, {
      req(rv$current_metric, input$strat_checks)

      withProgress(message = "Running exploratory screening...", value = 0, {
        ## Screening
        results_list <- lapply(input$strat_checks, function(sk) {
          incProgress(0.5 / length(input$strat_checks))
          screen_stratification(
            rv$data, rv$current_metric, sk,
            rv$metric_config, rv$strat_config
          )
        })

        result_rows <- dplyr::bind_rows(lapply(results_list, `[[`, "result_row"))
        pairwise_rows <- dplyr::bind_rows(
          purrr::keep(lapply(results_list, `[[`, "pairwise_df"), ~ nrow(.) > 0)
        )
        plots <- setNames(
          lapply(results_list, `[[`, "plot"),
          input$strat_checks
        )
        plots <- Filter(Negate(is.null), plots)
        plot_specs <- setNames(
          lapply(results_list, `[[`, "plot_spec"),
          input$strat_checks
        )
        plot_specs <- Filter(Negate(is.null), plot_specs)

        rv$phase1_screening <- list(
          results = result_rows,
          pairwise = pairwise_rows,
          plots = plots,
          plot_specs = plot_specs
        )

        ## Cache for cross-metric use
        rv$all_layer1_results[[rv$current_metric]] <- result_rows

        ## Effect sizes
        incProgress(0.3, detail = "Computing effect sizes...")
        strat_keys <- unique(result_rows$stratification)
        es <- compute_effect_sizes(
          rv$data, rv$current_metric, strat_keys,
          rv$metric_config, rv$strat_config
        )
        rv$phase1_effect_sizes <- es
        rv$all_layer2_results[[rv$current_metric]] <- es

        if (is.null(rv$metric_phase_cache[[rv$current_metric]])) {
          rv$metric_phase_cache[[rv$current_metric]] <- list()
        }
        rv$metric_phase_cache[[rv$current_metric]]$phase1_screening <- rv$phase1_screening
        rv$metric_phase_cache[[rv$current_metric]]$phase1_effect_sizes <- rv$phase1_effect_sizes
        rv$metric_phase_cache[[rv$current_metric]]$phase1_artifact_mode <- "full"
        rv$phase1_candidates[[rv$current_metric]] <- build_metric_phase1_candidate_table_from_sources(
          metric = rv$current_metric,
          allowed = get_metric_allowed_strats(rv, rv$current_metric),
          existing = NULL,
          l1 = result_rows,
          l2 = es,
          include_all_allowed = TRUE
        )
      })

      load_screening_results(rv$current_metric)
      notify_workspace_refresh(rv)
    })

    ## ── Results UI ────────────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      res <- screening_results()
      req(res)

      tagList(
        ## Combined stats table
        card(
          card_header("Screening Results"),
          card_body(DT::DTOutput(ns("results_table")))
        ),

        ## Boxplot gallery (tabbed)
        if (length(res$plots) > 0) {
          card(
            card_header(
              class = "d-flex flex-wrap align-items-center justify-content-between gap-2",
              tags$span("Boxplots"),
              shinyWidgets::prettySwitch(
                ns("boxplot_points"),
                "Show datapoints",
                value = FALSE,
                status = "primary",
                fill = TRUE
              )
            ),
            card_body(
              do.call(
                navset_tab,
                lapply(names(res$plots), function(sk) {
                  nav_panel(
                    title = rv$strat_config[[sk]]$display_name %||% sk,
                    plotOutput(ns(paste0("plot_", sk)), height = "500px")
                  )
                })
              )
            )
          )
        },

        ## Effect size bar chart
        if (!is.null(res$effect_sizes) && nrow(res$effect_sizes) > 0) {
          card(
            card_header("Effect Size by Stratification"),
            card_body(plotOutput(ns("effect_bar"), height = "300px"))
          )
        },

        ## Pairwise details (collapsed)
        if (!is.null(res$pairwise) && nrow(res$pairwise) > 0) {
          bslib::accordion(
            bslib::accordion_panel(
              "Pairwise Wilcoxon Details",
              DT::DTOutput(ns("pairwise_table"))
            ),
            open = FALSE
          )
        }
      )
    })

    ## Render combined results table (significance + effect size merged)
    output$results_table <- DT::renderDT({
      res <- screening_results()
      req(res)

      sig_df <- res$results |>
        dplyr::select(stratification, p_value, classification, min_group_n)

      es_df <- if (!is.null(res$effect_sizes) && nrow(res$effect_sizes) > 0) {
        res$effect_sizes |>
          dplyr::select(stratification, epsilon_squared, effect_size_label)
      } else {
        tibble::tibble(stratification = character(0),
                       epsilon_squared = numeric(0),
                       effect_size_label = character(0))
      }

      display_df <- sig_df |>
        dplyr::left_join(es_df, by = "stratification") |>
        dplyr::mutate(
          strat_display = sapply(stratification, function(sk) {
            rv$strat_config[[sk]]$display_name %||% sk
          }),
          p_value = round(p_value, 4),
          epsilon_squared = round(epsilon_squared, 4)
        ) |>
        dplyr::select(
          Stratification = strat_display,
          `p-value` = p_value,
          `Effect (eps-sq)` = epsilon_squared,
          `Effect Label` = effect_size_label,
          `Min Group n` = min_group_n,
          Classification = classification
        )

      DT::datatable(
        display_df,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      ) |>
        DT::formatStyle(
          "p-value",
          backgroundColor = DT::styleInterval(
            c(0.05, 0.10),
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)", "white")
          )
        )
    })

    ## Render pairwise table
    output$pairwise_table <- DT::renderDT({
      res <- screening_results()
      req(res, !is.null(res$pairwise), nrow(res$pairwise) > 0)

      display_df <- res$pairwise |>
        dplyr::mutate(
          statistic = round(statistic, 3),
          p_value = round(p_value, 4),
          p_adjusted = round(p_adjusted, 4)
        )

      DT::datatable(
        display_df,
        options = list(pageLength = 20, scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    ## Render boxplots dynamically
    observe({
      res <- screening_results()
      req(res)

      lapply(names(res$plots), function(sk) {
        local({
          local_sk <- sk
          output[[paste0("plot_", local_sk)]] <- renderPlot({
            current_res <- screening_results()
            req(current_res)

            if (!is.null(current_res$plot_specs[[local_sk]])) {
              return(build_screening_plot_from_spec(
                current_res$plot_specs[[local_sk]],
                rv$metric_config,
                rv$strat_config,
                font_profile = "large_analysis",
                show_points = isTRUE(input$boxplot_points %||% FALSE),
                masked_site_ids = phase1_quick_mask_ids()
              ))
            }

            current_res$plots[[local_sk]]
          })
        })
      })
    })

    ## Render effect size bar chart
    output$effect_bar <- renderPlot({
      res <- screening_results()
      req(res, !is.null(res$effect_sizes), nrow(res$effect_sizes) > 0)

      plot_df <- res$effect_sizes |>
        dplyr::filter(!is.na(epsilon_squared)) |>
        dplyr::mutate(
          strat_label = sapply(stratification, function(sk) {
            rv$strat_config[[sk]]$display_name %||% sk
          })
        )

      if (nrow(plot_df) == 0) return(NULL)

      ggplot2::ggplot(plot_df, ggplot2::aes(
        x = reorder(strat_label, epsilon_squared),
        y = epsilon_squared,
        fill = effect_size_label
      )) +
        ggplot2::geom_col(width = 0.6) +
        ggplot2::scale_fill_manual(
          values = c("large" = "#27ae60", "medium" = "#2980b9",
                     "small" = "#f39c12", "negligible" = "#95a5a6"),
          name = "Effect Size"
        ) +
        ggplot2::geom_hline(yintercept = c(0.01, 0.06, 0.14),
                            linetype = "dashed", color = "gray50", alpha = 0.7) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste0("Effect Size: ", rv$metric_config[[rv$current_metric]]$display_name),
          x = NULL,
          y = expression(epsilon^2 ~ "(Epsilon-squared)")
        ) +
        streamcurves_minimal_plot_theme()
    })

    ## ── Bulk screening progress indicator ──────────────────────────────────────
    output$bulk_progress <- renderUI({
      n_total <- sum(sapply(names(rv$metric_config), function(mk) {
        rv$metric_config[[mk]]$metric_family != "categorical"
      }))
      n_done <- length(rv$all_layer1_results)
      if (n_done > 0 && n_done < n_total) {
        div(class = "alert alert-info py-1 px-2 mb-1",
            sprintf("%d / %d metrics screened", n_done, n_total))
      }
    })

    ## ── Bulk screening handler ──────────────────────────────────────────────────
    observeEvent(input$run_all_screening, {
      withProgress(message = "Running stratification screening...", value = 0, {
        metric_keys <- names(rv$metric_config)
        metric_keys <- metric_keys[sapply(metric_keys, function(mk) {
          rv$metric_config[[mk]]$metric_family != "categorical"
        })]
        n_metrics <- length(metric_keys)

        for (i in seq_along(metric_keys)) {
          mk <- metric_keys[i]
          mc <- rv$metric_config[[mk]]
          allowed <- get_metric_allowed_strats(rv, mk)
          if (is.null(allowed) || length(allowed) == 0) next

          incProgress(1 / n_metrics, detail = paste0("Metric: ", mc$display_name))

          ## Run screening for each allowed stratification
          results_list <- lapply(allowed, function(sk) {
            if (!sk %in% names(rv$strat_config)) return(NULL)
            tryCatch(
              screen_stratification(rv$data, mk, sk,
                                    rv$metric_config, rv$strat_config),
              error = function(e) NULL
            )
          })
          names(results_list) <- allowed
          results_list <- Filter(Negate(is.null), results_list)
          if (length(results_list) == 0) next

          result_rows <- dplyr::bind_rows(lapply(results_list, `[[`, "result_row"))
          pairwise_rows <- dplyr::bind_rows(
            purrr::keep(lapply(results_list, `[[`, "pairwise_df"), ~ nrow(.) > 0)
          )
          plots <- setNames(
            lapply(results_list, `[[`, "plot"),
            names(results_list)
          )
          plots <- Filter(Negate(is.null), plots)
          plot_specs <- setNames(
            lapply(results_list, `[[`, "plot_spec"),
            names(results_list)
          )
          plot_specs <- Filter(Negate(is.null), plot_specs)

          ## Store in global caches (for Phase 2)
          rv$all_layer1_results[[mk]] <- result_rows

          ## Compute effect sizes
          strat_keys_tested <- unique(result_rows$stratification)
          es <- tryCatch(
            compute_effect_sizes(rv$data, mk, strat_keys_tested,
                                 rv$metric_config, rv$strat_config),
            error = function(e) tibble::tibble()
          )
          if (nrow(es) > 0) {
            rv$all_layer2_results[[mk]] <- es
          }

          rv$phase1_candidates[[mk]] <- build_metric_phase1_candidate_table_from_sources(
            metric = mk,
            allowed = allowed,
            existing = NULL,
            l1 = result_rows,
            l2 = es,
            include_all_allowed = TRUE
          )

          ## Store full Phase 1 state in metric_phase_cache
          screening_data <- list(
            results = result_rows,
            pairwise = pairwise_rows,
            plots = plots,
            plot_specs = plot_specs
          )

          if (is.null(rv$metric_phase_cache[[mk]])) {
            rv$metric_phase_cache[[mk]] <- list()
          }
          rv$metric_phase_cache[[mk]]$phase1_screening <- screening_data
          rv$metric_phase_cache[[mk]]$phase1_effect_sizes <- es
          rv$metric_phase_cache[[mk]]$phase1_artifact_mode <- "full"
        }
      })

      ## If current metric was screened, load its results into display
      mk <- rv$current_metric
      cached <- rv$metric_phase_cache[[mk]]
      if (!is.null(cached$phase1_screening)) {
        rv$phase1_screening <- cached$phase1_screening
        rv$phase1_effect_sizes <- cached$phase1_effect_sizes
        screening_results(list(
          results = cached$phase1_screening$results,
          pairwise = cached$phase1_screening$pairwise,
          plots = cached$phase1_screening$plots,
          plot_specs = cached$phase1_screening$plot_specs %||% list(),
          effect_sizes = cached$phase1_effect_sizes
        ))
      }

      showNotification(
        paste0("Bulk exploratory screening complete for ", length(rv$all_layer1_results), " metrics."),
        type = "message", duration = 5
      )
    })

    ## ── Candidate marking UI ──────────────────────────────────────────────────
    output$candidate_ui <- renderUI({
      res <- screening_results()
      req(res)

      candidates <- get_metric_phase1_candidate_table(rv, rv$current_metric)
      req(nrow(candidates) > 0)

      card(
        card_header("Automatic Exploratory Shortlist"),
        card_body(
          p(class = "text-muted", "This shortlist is generated automatically from significance and effect size and is used as guidance for later analysis steps."),
          lapply(seq_len(nrow(candidates)), function(i) {
            row <- candidates[i, , drop = FALSE]
            sk <- row$stratification[1]
            sc <- rv$strat_config[[sk]]
            selected_status <- row$candidate_status[1] %||% "not_promising"

            row_class <- switch(selected_status,
              promising = "candidate-promising",
              possible = "candidate-possible",
              not_promising = "candidate-not-promising",
              ""
            )

            div(
              class = paste("d-flex align-items-center gap-3 mb-2 p-2 rounded", row_class),
              tags$strong(sc$display_name %||% sk, style = "min-width: 220px;"),
              status_badge(
                switch(
                  selected_status,
                  promising = "pass",
                  possible = "caution",
                  "not_applicable"
                ),
                switch(
                  selected_status,
                  promising = "Promising",
                  possible = "Possible",
                  "Not Promising"
                )
              ),
              if (!is.na(row$p_value[1])) p_value_badge(row$p_value[1]),
              if (!is.na(row$epsilon_squared[1])) tags$span(
                class = "badge bg-light text-dark border",
                paste0("eps^2: ", format(round(row$epsilon_squared[1], 4), nsmall = 4, trim = TRUE))
              ),
              if (!is.na(row$effect_size_label[1]) && nzchar(row$effect_size_label[1])) {
                status_badge(
                  switch(row$effect_size_label[1],
                    large = "pass",
                    medium = "pass",
                    small = "caution",
                    "not_applicable"
                  ),
                  paste0("Effect: ", row$effect_size_label[1])
                )
              }
            )
          })
        )
      )
    })

    ## ── Save Phase 1 Results button ──────────────────────────────────────────
    output$save_ui <- renderUI({
      NULL
    })

    observeEvent(input$save_phase1, {
      res <- screening_results()
      req(res)

      strats <- unique(res$results$stratification)

      candidates <- purrr::map_dfr(strats, function(sk) {
        p_row <- res$results |> dplyr::filter(stratification == sk)
        es_row <- if (!is.null(res$effect_sizes)) {
          res$effect_sizes |> dplyr::filter(stratification == sk)
        } else {
          tibble::tibble()
        }

        tibble::tibble(
          metric = rv$current_metric,
          stratification = sk,
          p_value = if (nrow(p_row) > 0) p_row$p_value[1] else NA_real_,
          epsilon_squared = if (nrow(es_row) > 0) es_row$epsilon_squared[1] else NA_real_,
          effect_size_label = if (nrow(es_row) > 0) es_row$effect_size_label[1] else NA_character_,
          min_group_n = if (nrow(p_row) > 0) p_row$min_group_n[1] else NA_integer_,
          candidate_status = input[[paste0("status_", sk)]] %||% "not_promising",
          reviewer_note = input[[paste0("note_", sk)]] %||% ""
        )
      })

      rv$phase1_candidates[[rv$current_metric]] <- candidates

      ## Append to decision log
      new_entry <- tibble::tibble(
        entry_id = paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample(1000:9999, 1)),
        timestamp = Sys.time(),
        reviewer_name = "",
        metric = rv$current_metric,
        decision_stage = "phase1",
        phase = "phase1",
        selected_strat = NA_character_,
        auto_recommended = NA_character_,
        user_agreed = NA,
        strat_mode = NA_character_,
        layer1_p_value = NA_real_,
        layer2_effect_size = NA_real_,
        layer2_effect_label = NA_character_,
        layer3_stability = NA_character_,
        layer4_relevance_score = NA_real_,
        layer5_feasibility = NA_character_,
        selected_predictors = NA_character_,
        selected_bic = NA_real_,
        diagnostics_overall = NA_character_,
        rationale = paste0(
          sum(candidates$candidate_status == "promising"), " promising, ",
          sum(candidates$candidate_status == "possible"), " possible, ",
          sum(candidates$candidate_status == "not_promising"), " not promising"
        ),
        notes = ""
      )
      rv$decision_log <- dplyr::bind_rows(rv$decision_log, new_entry)

      showNotification(
        paste0("Phase 1 results saved for ", rv$metric_config[[rv$current_metric]]$display_name, "."),
        type = "message", duration = 3
      )
      notify_workspace_refresh(rv)
    })

    ## ── Reset this metric ─────────────────────────────────────────────────────
    observeEvent(input$reset_metric, {
      req(rv$current_metric)
      metric <- rv$current_metric

      for (field in PHASE_STATE_FIELDS) {
        rv[[field]] <- NULL
      }

      rv$metric_phase_cache[[metric]]   <- NULL
      rv$completed_metrics[[metric]]    <- NULL
      rv$all_layer1_results[[metric]]   <- NULL
      rv$all_layer2_results[[metric]]   <- NULL
      rv$phase1_candidates[[metric]]    <- NULL
      rv$phase3_verification[[metric]]  <- NULL

      if (!is.null(rv$decision_log) && nrow(rv$decision_log) > 0) {
        rv$decision_log <- rv$decision_log |> dplyr::filter(metric != !!metric)
      }

      screening_results(NULL)
      load_scatter_panel_state(metric)

      showNotification(
        paste0("Reset complete for ", metric, "."),
        type = "message", duration = 3
      )
      notify_workspace_refresh(rv)
    })

    ## ── Reset all metrics ─────────────────────────────────────────────────────
    observeEvent(input$reset_all_metrics, {
      showModal(modalDialog(
        title = "Reset All Metrics",
        "This will clear all completed work for every metric. This cannot be undone.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset_all"), "Reset Everything",
                       class = "btn btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_reset_all, {
      removeModal()
      reset_all_analysis(rv)
      screening_results(NULL)
      showNotification("All metrics reset \u2014 starting fresh.",
                       type = "message", duration = 3)
      notify_workspace_refresh(rv)
    })
  })
}
