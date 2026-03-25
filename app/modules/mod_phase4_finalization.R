## -- Module: Phase 4 -- Reference Curve Finalization ------------------------------
## Empirical scoring curve workflow: compute descriptive statistics and build
## piecewise-linear scoring curves directly from reference-site distributions.
## Stratified metrics show ALL strata simultaneously (no switching).
## Non-stratified metrics delegate to mod_ref_curve sub-module.

library(shiny)
library(bslib)

mod_phase4_finalization_ui <- function(id, dialog_mode = FALSE) {
  ns <- NS(id)
  uiOutput(ns("phase4_page"))
}

mod_phase4_finalization_server <- function(id, rv, dialog_mode = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    prev_metric <- reactiveVal(NULL)

    sync_phase4_inputs <- function(metric = rv$current_metric) {
      mc <- rv$metric_config[[metric]] %||% NULL
      if (is.null(mc)) return(invisible(NULL))
      updateNumericInput(session, "min_sample", value = mc$min_sample_size %||% 10)
      updateRadioButtons(session, "transform", selected = mc$preferred_transform %||% "none")
      invisible(NULL)
    }

    ## -- Data gate: show alert or full page ------------------------------------
    output$phase4_page <- renderUI({
      if (is.null(rv$data)) return(no_data_alert())

      if (isTRUE(dialog_mode)) {
        return(div(
          class = "workspace-phase-body",
          card(
            class = "mb-3",
            card_header("Metric Settings"),
            card_body(
              layout_column_wrap(
                width = 1 / 2,
                uiOutput(ns("metric_info")),
                div(
                  class = "workspace-phase-settings",
                  numericInput(ns("min_sample"), "Min sample size:", value = 10, min = 3, max = 30),
                  radioButtons(ns("transform"), "Transform:",
                               choices = c("None" = "none", "Log" = "log"),
                               selected = "none")
                )
              )
            )
          ),
          uiOutput(ns("strat_confirm_card")),
          uiOutput(ns("stratified_display")),
          uiOutput(ns("unstratified_display"))
        ))
      }

      layout_sidebar(
        ## -- Sidebar ---------------------------------------------------------------
        sidebar = sidebar(
          width = 300,
          title = "Phase 4: Finalize",

          ## Metric picker
          uiOutput(ns("metric_picker")),

          ## Metric info card
          uiOutput(ns("metric_info")),

          ## Advanced settings
          tags$hr(),
          bslib::accordion(
            id = ns("advanced_accordion"),
            open = FALSE,
            bslib::accordion_panel(
              "Advanced Settings",
              numericInput(ns("min_sample"), "Min sample size:", value = 10, min = 3, max = 30),
              radioButtons(ns("transform"), "Transform:",
                           choices = c("None" = "none", "Log" = "log"),
                           selected = "none")
            )
          )
        ),

        ## -- Main content ----------------------------------------------------------
        tagList(
          ## Stratification confirmation card
          uiOutput(ns("strat_confirm_card")),

          ## All-strata display (returns NULL if non-stratified)
          uiOutput(ns("stratified_display")),

          ## Single-stratum mod_ref_curve (returns NULL if stratified)
          uiOutput(ns("unstratified_display"))
        )
      )
    })

    ## -- Auto-derive stratification from Phase 1 if Phase 3 not confirmed ------
    observe({
      req(rv$current_metric)
      ## Only auto-derive if no manual decision exists
      if (!is.null(rv$strat_decision_user)) return()

      metric <- rv$current_metric
      candidates <- rv$phase1_candidates[[metric]]

      if (is.null(candidates) || nrow(candidates) == 0) {
        ## No candidates at all -- set decision to "none"
        rv$strat_decision_user <- tibble::tibble(
          decision_type     = "none",
          selected_strat    = NA_character_,
          selected_p_value  = NA_real_,
          selected_n_groups = NA_integer_,
          selected_min_n    = NA_integer_,
          runner_up_strat   = NA_character_,
          runner_up_p_value = NA_real_,
          needs_review      = TRUE,
          review_reason     = "Auto-derived: no Phase 1 candidates available",
          notes             = "No stratification candidates found in Phase 1 screening"
        )
        return()
      }

      ## Pick best candidate: first "promising", else first "possible"
      best <- candidates |>
        dplyr::filter(candidate_status %in% c("promising", "possible")) |>
        dplyr::arrange(match(candidate_status, c("promising", "possible")), p_value) |>
        dplyr::slice(1)

      if (nrow(best) == 0) {
        ## No promising/possible candidates
        rv$strat_decision_user <- tibble::tibble(
          decision_type     = "none",
          selected_strat    = NA_character_,
          selected_p_value  = NA_real_,
          selected_n_groups = NA_integer_,
          selected_min_n    = NA_integer_,
          runner_up_strat   = NA_character_,
          runner_up_p_value = NA_real_,
          needs_review      = TRUE,
          review_reason     = "Auto-derived: no promising or possible candidates in Phase 1",
          notes             = "All Phase 1 candidates were not_promising"
        )
        return()
      }

      ## Look up detailed stats from all_layer1_results
      strat_name <- best$stratification
      layer1 <- rv$all_layer1_results[[metric]]
      l1_stats <- NULL
      if (!is.null(layer1)) {
        if (is.data.frame(layer1)) {
          l1_stats <- layer1 |> dplyr::filter(stratification == strat_name) |> dplyr::slice(1)
        } else if (is.list(layer1)) {
          l1_df <- tryCatch(dplyr::bind_rows(layer1), error = function(e) NULL)
          if (!is.null(l1_df)) {
            l1_stats <- l1_df |> dplyr::filter(stratification == strat_name) |> dplyr::slice(1)
          }
        }
      }

      rv$strat_decision_user <- tibble::tibble(
        decision_type     = "single",
        selected_strat    = strat_name,
        selected_p_value  = if (!is.null(l1_stats) && nrow(l1_stats) > 0) l1_stats$p_value[1] else best$p_value,
        selected_n_groups = if (!is.null(l1_stats) && nrow(l1_stats) > 0) as.integer(l1_stats$n_groups[1]) else NA_integer_,
        selected_min_n    = if (!is.null(l1_stats) && nrow(l1_stats) > 0) as.integer(l1_stats$min_group_n[1]) else as.integer(best$min_group_n),
        runner_up_strat   = NA_character_,
        runner_up_p_value = NA_real_,
        needs_review      = TRUE,
        review_reason     = "Auto-derived from Phase 1 candidates (Phase 3 not confirmed)",
        notes             = paste0("Auto-selected: ", strat_name)
      )
    })

    ## -- Build metric picker choices -------------------------------------------
    output$metric_picker <- renderUI({
      mc <- rv$metric_config
      families <- list()
      for (mk in names(mc)) {
        fam <- mc[[mk]]$metric_family
        if (fam == "categorical") next
        fam_label <- switch(fam,
          continuous = "Continuous", proportion = "Proportion",
          count = "Count", fam)
        families[[fam_label]] <- c(families[[fam_label]], setNames(mk, mc[[mk]]$display_name))
      }

      selectInput(ns("metric_select"), "Select Metric:",
                  choices = families,
                  selected = rv$current_metric %||% "perRiffle")
    })

    ## -- Stratum info reactive -------------------------------------------------
    stratum_info <- reactive({
      strat <- rv$strat_decision_user

      if (is.null(strat) || is.na(strat$selected_strat) || strat$decision_type != "single") {
        return(list(has_strata = FALSE, levels = NULL, strat_var = NULL))
      }

      strat_var <- strat$selected_strat
      levels <- sort(unique(get_stratification_values(rv$data, strat_var, rv$strat_config)))
      levels <- levels[!is.na(levels)]
      list(has_strata = TRUE, levels = levels, strat_var = strat_var)
    })

    strat_values <- reactive({
      info <- stratum_info()
      req(info$has_strata)
      get_stratification_values(rv$data, info$strat_var, rv$strat_config)
    })

    current_phase4_decision <- reactive({
      req(rv$current_metric)
      get_metric_phase4_decision_state(rv, rv$current_metric)
    })

    ## -- Metric change observer ------------------------------------------------
    observeEvent(input$metric_select, {
      old_metric <- prev_metric()
      new_metric <- input$metric_select
      if (!is.null(old_metric) && identical(old_metric, new_metric)) return()

      ## Save current metric state before switch (no per-stratum state cycling)
      if (!is.null(old_metric) && old_metric != "") {
        save_metric_phase_state(rv, old_metric)
      }
      rv$current_metric <- new_metric
      prev_metric(new_metric)
      restore_metric_phase_state(rv, new_metric)

      ## Update advanced settings from config
      sync_phase4_inputs(new_metric)
    }, ignoreInit = TRUE)

    observeEvent(rv$workspace_modal_ready_nonce, {
      if (isTRUE(dialog_mode) && identical(rv$workspace_modal_type, "phase4")) {
        modal_metric <- rv$workspace_modal_metric %||% rv$current_metric
        session$onFlushed(function() {
          if (is.null(modal_metric) || identical(modal_metric, "")) {
            return(invisible(NULL))
          }
          shiny::isolate({
            sync_phase4_inputs(modal_metric)
            invisible(NULL)
          })
          invisible(NULL)
        }, once = TRUE)
      }
    }, ignoreInit = TRUE)

    ## -- Initialize phase4_data when metric/strat changes ----------------------
    observe({
      req(rv$current_metric)
      ## Always set full dataset; stratified display handles its own filtering
      rv$phase4_data <- rv$data
      rv$current_stratum_level <- NULL
    })

    ## -- Advanced settings write-back ------------------------------------------
    observeEvent(input$min_sample, {
      req(rv$current_metric)
      mc <- isolate(rv$metric_config)
      if (!is.null(mc[[rv$current_metric]]) &&
          !identical(mc[[rv$current_metric]]$min_sample_size, input$min_sample)) {
        mc[[rv$current_metric]]$min_sample_size <- input$min_sample
        rv$metric_config <- mc
      }
    }, ignoreInit = TRUE)

    observeEvent(input$transform, {
      req(rv$current_metric)
      mc <- isolate(rv$metric_config)
      if (!is.null(mc[[rv$current_metric]]) &&
          !identical(mc[[rv$current_metric]]$preferred_transform, input$transform)) {
        mc[[rv$current_metric]]$preferred_transform <- input$transform
        rv$metric_config <- mc
      }
    }, ignoreInit = TRUE)

    ## -- Metric info card ------------------------------------------------------
    output$metric_info <- renderUI({
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
                )
              )
            }
          )
        ),
        if (completed) div(class = "mt-2", status_badge("pass", "COMPLETED"))
      )
    })

    ## -- Stratification confirmation card --------------------------------------
    output$strat_confirm_card <- renderUI({
      strat <- rv$strat_decision_user
      if (is.null(strat)) {
        return(div(
          class = "alert alert-warning mb-3",
          icon("exclamation-triangle"),
          " No stratification selected. Complete Phase 3 first, or proceed without stratification."
        ))
      }

      info <- stratum_info()
      strat_name <- if (strat$decision_type == "single" && !is.na(strat$selected_strat)) {
        sc <- rv$strat_config[[strat$selected_strat]]
        sc$display_name %||% strat$selected_strat
      } else {
        "None"
      }

      ## Auto-derived banner (shown when needs_review is TRUE)
      auto_banner <- NULL
      if (isTRUE(strat$needs_review)) {
        auto_banner <- div(
          class = "alert alert-warning mb-2 py-2",
          icon("robot"),
          tags$strong(" Auto-selected stratification"),
          " \u2014 based on Phase 1 screening results. ",
          "You can return to Phase 3 to review and confirm, or proceed directly."
        )
      }

      if (info$has_strata) {
        ## Build level listing with sample sizes
        level_labels <- sapply(info$levels, function(lvl) {
          n <- sum(strat_values() == lvl, na.rm = TRUE)
          paste0(lvl, " (n=", n, ")")
        })

        tagList(
          auto_banner,
          div(
            class = "alert alert-info mb-3",
            tags$strong("Stratification: "), strat_name,
            " | Mode: Subset (separate curve per level)",
            tags$br(),
            "Levels: ", paste(level_labels, collapse = ", ")
          )
        )
      } else {
        tagList(
          auto_banner,
          div(
            class = "alert alert-info mb-3",
            tags$strong("Stratification: "), strat_name,
          if (strat$decision_type == "single" && !is.na(strat$selected_strat)) {
            tagList(
              " | ",
              sprintf("p = %.4f | groups = %d | min n = %d",
                       strat$selected_p_value %||% NA_real_,
                       strat$selected_n_groups %||% NA_integer_,
                       strat$selected_min_n %||% NA_integer_)
            )
          }
          )
        )
      }
    })

    ## -- Sub-module server (always runs; UI conditionally shown) ----------------
    ref_curve_events <- mod_ref_curve_server("ref_curve", rv)

    ## =========================================================================
    ## NON-STRATIFIED DISPLAY
    ## =========================================================================

    output$unstratified_display <- renderUI({
      info <- stratum_info()
      if (info$has_strata) return(NULL)
      mod_ref_curve_ui(ns("ref_curve"))
    })

    ## =========================================================================
    ## ALL-STRATA SIMULTANEOUS DISPLAY
    ## =========================================================================

    ## -- Compute all strata curves eagerly -------------------------------------
    all_strata_results <- reactive({
      info <- stratum_info()
      req(info$has_strata)
      metric <- rv$current_metric
      req(metric)
      decision_tbl <- current_phase4_decision()

      if (metric_has_phase4_cache(rv, metric, decision_tbl)) {
        cached <- get_metric_phase4_cached_result(rv, metric)
        cached_results <- lapply(info$levels, function(lvl) {
          cached$stratum_results[[lvl]]$reference_curve %||% NULL
        })
        names(cached_results) <- info$levels

        if (all(vapply(cached_results, Negate(is.null), logical(1)))) {
          return(cached_results)
        }
      }

      results <- list()
      strat_vals <- strat_values()
      for (lvl in info$levels) {
        stratum_data <- rv$data[strat_vals == lvl, , drop = FALSE]
        results[[lvl]] <- build_reference_curve(stratum_data, metric,
                                                 rv$metric_config, stratum_label = lvl)
      }
      cache_metric_phase4_results(
        rv,
        metric,
        decision_tbl = decision_tbl,
        stratum_results = lapply(results, function(result) {
          list(reference_curve = result)
        })
      )
      results
    })

    ## -- Bind curve_row tibbles across strata ----------------------------------
    all_strata_curve_rows <- reactive({
      results <- all_strata_results()
      req(results)
      dplyr::bind_rows(purrr::map(results, "curve_row"))
    })

    ## -- Populate rv$stratum_results for persistence ---------------------------
    observe({
      results <- all_strata_results()
      req(results)
      metric <- rv$current_metric
      for (lvl in names(results)) {
        if (is.null(rv$stratum_results[[metric]])) rv$stratum_results[[metric]] <- list()
        rv$stratum_results[[metric]][[lvl]] <- list(reference_curve = results[[lvl]])
      }
    })

    ## -- Stratified display UI -------------------------------------------------
    output$stratified_display <- renderUI({
      info <- stratum_info()
      if (!info$has_strata) return(NULL)

      curve_rows <- all_strata_curve_rows()
      req(curve_rows)

      metric <- rv$current_metric
      mc <- rv$metric_config[[metric]]
      min_n <- mc$min_sample_size %||% 10

      ## Per-stratum sample size warnings
      warnings <- list()
      strat_vals <- strat_values()
      for (lvl in info$levels) {
        n <- sum(strat_vals == lvl, na.rm = TRUE)
        if (n < min_n) {
          warnings[[lvl]] <- div(
            class = "alert alert-warning py-2 mb-2",
            icon("exclamation-triangle"),
            sprintf(" Stratum '%s' has only %d observations (minimum: %d). Results may be unreliable.",
                    lvl, n, min_n)
          )
        }
      }

      ## Check how many strata have valid (complete) curves
      valid_rows <- curve_rows |> dplyr::filter(curve_status == "complete")
      n_valid <- nrow(valid_rows)

      tagList(
        ## Warnings
        if (length(warnings) > 0) tagList(warnings),

        ## Plots: two-column layout
        layout_column_wrap(
          width = 1 / 2,

          ## Left: overlay distributions
          bslib::card(
            class = "mb-3",
            bslib::card_header("Distributions by Stratum"),
            bslib::card_body(
              if (n_valid >= 1) {
                plotOutput(ns("strat_dist_plot"), height = "420px")
              } else {
                div(class = "text-muted", "No valid strata for distribution plot.")
              }
            )
          ),

          ## Right: overlay scoring curves
          bslib::card(
            class = "mb-3",
            bslib::card_header("Scoring Curves by Stratum"),
            bslib::card_body(
              if (n_valid >= 2) {
                plotOutput(ns("strat_curve_plot"), height = "420px")
              } else if (n_valid == 1) {
                tagList(
                  plotOutput(ns("strat_curve_plot_single"), height = "420px"),
                  div(class = "text-muted mt-1",
                      "Only 1 stratum has a valid curve. Overlay requires 2+.")
                )
              } else {
                div(class = "text-muted", "No valid strata for scoring curve plot.")
              }
            )
          )
        ),

        ## Descriptive statistics table (strata as columns)
        bslib::card(
          class = "mb-3",
          bslib::card_header("Descriptive Statistics"),
          bslib::card_body(DT::DTOutput(ns("strat_desc_table")))
        ),

        ## Scoring thresholds table (strata as columns)
        bslib::card(
          class = "mb-3",
          bslib::card_header("Scoring Thresholds"),
          bslib::card_body(DT::DTOutput(ns("strat_threshold_table")))
        ),

        ## Download buttons
        div(
          class = "d-flex gap-2 mb-3",
          downloadButton(ns("dl_strat_curve_png"), "Download Overlay Curve PNG",
                         class = "btn btn-outline-secondary btn-sm"),
          downloadButton(ns("dl_strat_dist_png"), "Download Overlay Distribution PNG",
                         class = "btn btn-outline-secondary btn-sm"),
          downloadButton(ns("dl_strat_csv"), "Download Thresholds CSV",
                         class = "btn btn-outline-secondary btn-sm")
        ),

        ## Mark Complete button (single for all strata)
        div(
          class = "d-flex justify-content-end mt-3",
          actionButton(ns("mark_all_complete"), "Mark All Strata Complete \u2713",
                       class = "btn btn-success btn-proceed",
                       icon = icon("check"))
        )
      )
    })

    ## -- Overlay scoring curves plot -------------------------------------------
    output$strat_curve_plot <- renderPlot({
      curve_rows <- all_strata_curve_rows()
      req(curve_rows)
      p <- build_overlay_curve_plot(curve_rows, rv$metric_config)
      req(p)
      p
    })

    ## -- Single-stratum fallback curve plot ------------------------------------
    output$strat_curve_plot_single <- renderPlot({
      results <- all_strata_results()
      req(results)
      ## Find the first stratum with a valid curve_plot
      for (lvl in names(results)) {
        if (!is.null(results[[lvl]]$curve_plot)) {
          return(results[[lvl]]$curve_plot)
        }
      }
      NULL
    })

    ## -- Overlay distribution plot ---------------------------------------------
    output$strat_dist_plot <- renderPlot({
      info <- stratum_info()
      req(info$has_strata)
      curve_rows <- all_strata_curve_rows()
      req(curve_rows)
      valid_levels <- curve_rows |>
        dplyr::filter(curve_status == "complete") |>
        dplyr::pull(stratum)
      ## Use all levels for distribution even if curve is degenerate
      all_levels <- unique(curve_rows$stratum)
      levels_to_show <- if (length(valid_levels) > 0) all_levels else info$levels
      plot_data <- rv$data
      plot_data$.summary_stratum <- strat_values()
      build_overlay_bar_chart(plot_data, rv$current_metric, rv$metric_config,
                               ".summary_stratum", levels_to_show)
    })

    ## -- Descriptive statistics table (strata as columns) ----------------------
    output$strat_desc_table <- DT::renderDT({
      curve_rows <- all_strata_curve_rows()
      req(curve_rows)

      stats <- c("n", "Min", "Q25", "Median", "Q75", "Max", "IQR", "SD")
      tbl <- data.frame(Statistic = stats, stringsAsFactors = FALSE)

      for (i in seq_len(nrow(curve_rows))) {
        row <- curve_rows[i, ]
        lvl <- row$stratum
        if (row$curve_status == "insufficient_data") {
          tbl[[lvl]] <- rep("N/A", length(stats))
        } else {
          tbl[[lvl]] <- c(
            as.character(row$n_reference),
            round(row$min_val, 2),
            round(row$q25, 2),
            round(row$median_val, 2),
            round(row$q75, 2),
            round(row$max_val, 2),
            round(row$iqr, 2),
            round(row$sd_val, 2)
          )
        }
      }

      DT::datatable(tbl, rownames = FALSE,
                     options = list(dom = "t", paging = FALSE, ordering = FALSE),
                     class = "table table-sm table-striped")
    })

    ## -- Scoring thresholds table (strata as columns) --------------------------
    output$strat_threshold_table <- DT::renderDT({
      curve_rows <- all_strata_curve_rows()
      req(curve_rows)

      categories <- c("Functioning", "At-Risk", "Not Functioning")
      score_ranges <- c("0.70 - 1.00", "0.30 - 0.69", "0.00 - 0.29")

      tbl <- data.frame(
        Category = categories,
        `Score Range` = score_ranges,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      for (i in seq_len(nrow(curve_rows))) {
        row <- curve_rows[i, ]
        lvl <- row$stratum
        if (row$curve_status == "insufficient_data") {
          tbl[[lvl]] <- rep("N/A", 3)
        } else {
          tbl[[lvl]] <- c(
            paste0(round(row$functioning_min, 1), " \u2013 ", round(row$functioning_max, 1)),
            paste0(round(row$at_risk_min, 1), " \u2013 ", round(row$at_risk_max, 1)),
            paste0(round(row$not_functioning_min, 1), " \u2013 ", round(row$not_functioning_max, 1))
          )
        }
      }

      DT::datatable(tbl, rownames = FALSE,
                     options = list(dom = "t", paging = FALSE, ordering = FALSE),
                     class = "table table-sm table-striped") |>
        DT::formatStyle(
          "Category",
          backgroundColor = DT::styleEqual(
            categories,
            c("rgba(39,174,96,0.15)", "rgba(243,156,18,0.15)", "rgba(231,76,60,0.15)")
          )
        )
    })

    ## -- Mark All Complete handler ---------------------------------------------
    observeEvent(input$mark_all_complete, {
      info <- stratum_info()
      req(info$has_strata)
      metric <- rv$current_metric
      req(metric)

      results <- all_strata_results()
      req(results)
      decision_tbl <- current_phase4_decision()

      ## Save all strata to rv$stratum_results
      for (lvl in names(results)) {
        if (is.null(rv$stratum_results[[metric]])) rv$stratum_results[[metric]] <- list()
        rv$stratum_results[[metric]][[lvl]] <- list(reference_curve = results[[lvl]])
      }

      ## Mark metric complete with all strata
      phase4_signature <- cache_metric_phase4_results(
        rv,
        metric,
        decision_tbl = decision_tbl,
        stratum_results = rv$stratum_results[[metric]]
      )
      rv$completed_metrics[[metric]] <- list(
        stratified = TRUE,
        strat_var = info$strat_var,
        strat_decision = decision_tbl,
        stratum_results = rv$stratum_results[[metric]],
        phase4_signature = phase4_signature
      )

      showNotification(
        paste0(rv$metric_config[[metric]]$display_name,
               " \u2014 all ", length(info$levels), " strata marked complete!"),
        type = "message", duration = 5
      )
      notify_workspace_refresh(rv)
    })

    ## -- Download: overlay curve PNG -------------------------------------------
    output$dl_strat_curve_png <- downloadHandler(
      filename = function() {
        paste0(rv$current_metric, "_overlay_scoring_curves.png")
      },
      content = function(file) {
        curve_rows <- all_strata_curve_rows()
        req(curve_rows)
        p <- build_overlay_curve_plot(curve_rows, rv$metric_config)
        if (!is.null(p)) {
          ggplot2::ggsave(file, p, width = 10, height = 6, dpi = 300)
        }
      }
    )

    ## -- Download: overlay distribution PNG ------------------------------------
    output$dl_strat_dist_png <- downloadHandler(
      filename = function() {
        paste0(rv$current_metric, "_overlay_distributions.png")
      },
      content = function(file) {
        info <- stratum_info()
        req(info$has_strata)
        plot_data <- rv$data
        plot_data$.summary_stratum <- strat_values()
        p <- build_overlay_bar_chart(plot_data, rv$current_metric, rv$metric_config,
                                      ".summary_stratum", info$levels)
        if (!is.null(p)) {
          ggplot2::ggsave(file, p, width = 10, height = 6, dpi = 300)
        }
      }
    )

    ## -- Download: comparison CSV ----------------------------------------------
    output$dl_strat_csv <- downloadHandler(
      filename = function() {
        paste0(rv$current_metric, "_strata_thresholds.csv")
      },
      content = function(file) {
        curve_rows <- all_strata_curve_rows()
        req(curve_rows)
        readr::write_csv(curve_rows, file)
      }
    )
  })
}
