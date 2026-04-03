suppressPackageStartupMessages({
  library(shiny)
  library(tibble)
})

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd("app")
source("app.R", local = TRUE)
source("../tests/local_workbook_helper.R", local = TRUE)

project_root <- streamcurves_test_root()
workbook_path <- require_streamcurves_test_workbook("phase1_scatter_primary_stability_checks", project_root = project_root)

build_phase1_test_rv <- function() {
  input_bundle <- read_input_workbook(workbook_path)

  clean_result <- clean_data(
    input_bundle$raw_data,
    input_bundle$metric_config,
    input_bundle$strat_config,
    input_bundle$factor_recode_config
  )

  data <- derive_variables(
    clean_result$data,
    input_bundle$factor_recode_config,
    input_bundle$predictor_config,
    input_bundle$strat_config
  )

  precheck <- run_metric_precheck(data, input_bundle$metric_config)

  shiny::reactiveValues(
    data = data,
    metric_config = input_bundle$metric_config,
    strat_config = input_bundle$strat_config,
    current_metric = "perRiffle",
    workspace_modal_metric = "perRiffle",
    metric_phase_cache = list(),
    precheck_df = precheck,
    completed_metrics = list(),
    phase1_screening = list(),
    phase1_effect_sizes = list(),
    phase1_candidates = list(),
    phase3_verification = list(),
    decision_log = tibble(),
    all_layer1_results = list(),
    all_layer2_results = list(),
    config_version = 1L,
    app_reset_nonce = 0L,
    analysis_tab_preload_nonce = 0L,
    analysis_tab_preload_tab = NULL,
    analysis_tab_request_id = NULL,
    workspace_modal_type = "phase1",
    workspace_modal_ready_nonce = 0L
  )
}

check_single_compare_panels_stay_stable <- function() {
  rv <- build_phase1_test_rv()

  shiny::testServer(
    mod_phase1_exploration_server,
    args = list(rv = rv, dialog_mode = TRUE),
    {
      session$flushReact()

      initial_panels <- scatter_panel_store()
      initial_shells <- scatter_panel_shells()
      initial_shell_version <- scatter_panel_shell_version()
      initial_active_tab <- scatter_panel_active_tab()
      initial_panel_state <- get_scatter_panel_state("panel_1")
      initial_ui_html <- htmltools::renderTags(output$scatter_panel_list_ui)$html
      plot_output_id <- scatter_panel_plot_output_id("panel_1")
      initial_plot_json <- as.character(output[[plot_output_id]])

      stopifnot(length(initial_panels) == 1L)
      stopifnot(length(initial_shells) == 1L)
      stopifnot(identical(initial_panels[[1]]$compare_metric, "wBHR"))
      stopifnot(identical(initial_panels[[1]]$strat_key, "none"))
      stopifnot(isTRUE(initial_panels[[1]]$current_on_x))
      stopifnot(identical(initial_shells[[1]]$id, initial_panels[[1]]$id))
      stopifnot(identical(initial_shells[[1]]$label, initial_panels[[1]]$label))
      stopifnot(identical(initial_active_tab, "panel_1"))
      stopifnot(identical(initial_panel_state$compare_metric, "wBHR"))
      stopifnot(!is.null(initial_panel_state$comparison))
      stopifnot(inherits(build_scatter_panel_plot(initial_panel_state), "plotly"))
      stopifnot(grepl('scatter_panel_tabs', initial_ui_html, fixed = TRUE))
      stopifnot(grepl('scatter_panel_compare_panel_1', initial_ui_html, fixed = TRUE))
      stopifnot(grepl('scatter_panel_strat_key_panel_1', initial_ui_html, fixed = TRUE))
      stopifnot(grepl('scatter_panel_toggle_panel_1', initial_ui_html, fixed = TRUE))

      session$setInputs(scatter_panel_compare_panel_1 = "WDR")
      session$flushReact()
      session$flushReact()

      compare_panels <- scatter_panel_store()
      compare_shells <- scatter_panel_shells()
      compare_ui_html <- htmltools::renderTags(output$scatter_panel_list_ui)$html
      compare_plot_json <- as.character(output[[plot_output_id]])

      stopifnot(identical(compare_panels[[1]]$compare_metric, "WDR"))
      stopifnot(identical(get_scatter_panel_state("panel_1")$compare_metric, "WDR"))
      stopifnot(identical(compare_shells, initial_shells))
      stopifnot(identical(scatter_panel_shell_version(), initial_shell_version))
      stopifnot(!identical(compare_plot_json, initial_plot_json))
      stopifnot(identical(compare_ui_html, initial_ui_html))

      session$setInputs(scatter_panel_compare_panel_1 = NULL)
      session$flushReact()
      stopifnot(identical(scatter_panel_store()[[1]]$compare_metric, "WDR"))

      prune_scatter_panel_input_observers(reset_all = TRUE)
      session$setInputs(scatter_panel_compare_panel_1 = "wBHR")
      session$flushReact()
      register_scatter_panel_input_observers("panel_1")
      session$flushReact()
      stopifnot(identical(scatter_panel_store()[[1]]$compare_metric, "WDR"))
      session$setInputs(scatter_panel_compare_panel_1 = "WDR")
      session$flushReact()
      stopifnot(identical(scatter_panel_store()[[1]]$compare_metric, "WDR"))

      compare_metrics <- unname(scatter_panel_compare_choices("perRiffle"))
      strat_map <- stats::setNames(
        lapply(compare_metrics, function(metric) {
          setdiff(unname(scatter_strat_choices(metric, "perRiffle")), "none")
        }),
        compare_metrics
      )

      invalid_pair <- NULL
      for (source_metric in names(strat_map)) {
        source_strats <- strat_map[[source_metric]]
        if (length(source_strats) == 0L) {
          next
        }

        for (target_metric in setdiff(names(strat_map), source_metric)) {
          invalid_strats <- setdiff(source_strats, strat_map[[target_metric]])
          if (length(invalid_strats) == 0L) {
            next
          }

          invalid_pair <- list(
            source_metric = source_metric,
            target_metric = target_metric,
            strat_key = invalid_strats[[1]]
          )
          break
        }

        if (!is.null(invalid_pair)) {
          break
        }
      }

      stopifnot(!is.null(invalid_pair))

      session$setInputs(scatter_panel_compare_panel_1 = invalid_pair$source_metric)
      session$flushReact()
      source_plot_json <- as.character(output[[plot_output_id]])

      session$setInputs(scatter_panel_strat_key_panel_1 = invalid_pair$strat_key)
      session$flushReact()
      session$flushReact()

      strat_plot_json <- as.character(output[[plot_output_id]])
      strat_ui_html <- htmltools::renderTags(output$scatter_panel_list_ui)$html
      stopifnot(identical(scatter_panel_store()[[1]]$strat_key, invalid_pair$strat_key))
      stopifnot(identical(scatter_panel_shells(), initial_shells))
      stopifnot(identical(scatter_panel_shell_version(), initial_shell_version))
      stopifnot(!identical(strat_plot_json, source_plot_json))
      stopifnot(grepl(get_strat_display_name(rv, invalid_pair$strat_key), strat_plot_json, fixed = TRUE))
      stopifnot(identical(strat_ui_html, initial_ui_html))

      prune_scatter_panel_input_observers(reset_all = TRUE)
      session$setInputs(scatter_panel_strat_key_panel_1 = "none")
      session$flushReact()
      register_scatter_panel_input_observers("panel_1")
      session$flushReact()
      stopifnot(identical(scatter_panel_store()[[1]]$strat_key, invalid_pair$strat_key))
      session$setInputs(scatter_panel_strat_key_panel_1 = invalid_pair$strat_key)
      session$flushReact()
      stopifnot(identical(scatter_panel_store()[[1]]$strat_key, invalid_pair$strat_key))

      session$setInputs(scatter_panel_toggle_panel_1 = FALSE)
      session$flushReact()
      session$flushReact()

      toggle_plot_json <- as.character(output[[plot_output_id]])
      toggle_ui_html <- htmltools::renderTags(output$scatter_panel_list_ui)$html
      stopifnot(identical(scatter_panel_store()[[1]]$current_on_x, FALSE))
      stopifnot(identical(get_scatter_panel_state("panel_1")$current_on_x, FALSE))
      stopifnot(identical(scatter_panel_shells(), initial_shells))
      stopifnot(identical(scatter_panel_shell_version(), initial_shell_version))
      stopifnot(!identical(toggle_plot_json, strat_plot_json))
      stopifnot(identical(toggle_ui_html, initial_ui_html))

      prune_scatter_panel_input_observers(reset_all = TRUE)
      session$setInputs(scatter_panel_toggle_panel_1 = TRUE)
      session$flushReact()
      register_scatter_panel_input_observers("panel_1")
      session$flushReact()
      stopifnot(identical(scatter_panel_store()[[1]]$current_on_x, FALSE))
      session$setInputs(scatter_panel_toggle_panel_1 = FALSE)
      session$flushReact()
      stopifnot(identical(scatter_panel_store()[[1]]$current_on_x, FALSE))

      session$setInputs(scatter_panel_compare_panel_1 = invalid_pair$target_metric)
      session$flushReact()
      session$flushReact()

      stopifnot(identical(scatter_panel_store()[[1]]$compare_metric, invalid_pair$target_metric))
      stopifnot(identical(scatter_panel_store()[[1]]$strat_key, "none"))
      stopifnot(identical(scatter_panel_shells(), initial_shells))
      stopifnot(identical(scatter_panel_shell_version(), initial_shell_version))

      session$setInputs(add_scatter_panel = 1)
      session$flushReact()
      stopifnot(length(scatter_panel_store()) == 2L)
      stopifnot(length(scatter_panel_shells()) == 2L)
      stopifnot(identical(scatter_panel_active_tab(), "panel_2"))
      stopifnot(identical(scatter_panel_shells()[[2]]$id, "panel_2"))

      panel_2_metric <- unname(scatter_panel_compare_choices("perRiffle"))
      panel_2_metric <- panel_2_metric[panel_2_metric != invalid_pair$target_metric][[1]]
      stopifnot(!is.null(panel_2_metric), nzchar(panel_2_metric))

      session$setInputs(scatter_panel_compare_panel_2 = panel_2_metric)
      session$flushReact()
      session$flushReact()

      list_ui_html <- htmltools::renderTags(output$scatter_panel_list_ui)$html
      stopifnot(identical(scatter_panel_store()[[1]]$compare_metric, invalid_pair$target_metric))
      stopifnot(identical(scatter_panel_store()[[2]]$compare_metric, panel_2_metric))
      stopifnot(grepl('scatter_panel_compare_panel_2', list_ui_html, fixed = TRUE))

      session$setInputs(scatter_panel_tabs = "panel_1")
      session$flushReact()
      stopifnot(identical(scatter_panel_active_tab(), "panel_1"))
      stopifnot(identical(scatter_panel_store()[[1]]$compare_metric, invalid_pair$target_metric))
      stopifnot(identical(scatter_panel_store()[[2]]$compare_metric, panel_2_metric))

      session$setInputs(scatter_panel_tabs = "panel_2")
      session$flushReact()
      stopifnot(identical(scatter_panel_active_tab(), "panel_2"))
      stopifnot(identical(scatter_panel_store()[[1]]$compare_metric, invalid_pair$target_metric))
      stopifnot(identical(scatter_panel_store()[[2]]$compare_metric, panel_2_metric))

      session$setInputs(remove_scatter_panel = 1)
      session$flushReact()
      session$flushReact()

      stopifnot(length(scatter_panel_store()) == 1L)
      stopifnot(length(scatter_panel_shells()) == 1L)
      stopifnot(identical(scatter_panel_active_tab(), "panel_1"))
      stopifnot(identical(scatter_panel_store()[[1]]$compare_metric, invalid_pair$target_metric))

      legacy_strat_choices <- unname(scatter_strat_choices("WDR", "perRiffle"))
      legacy_strat_key <- legacy_strat_choices[legacy_strat_choices != "none"][[1]]
      stopifnot(!is.null(legacy_strat_key), nzchar(legacy_strat_key))
      rv$metric_phase_cache[["perRiffle"]]$phase1_scatter_panels <- list(
        list(
          id = "panel_1",
          label = "Legacy Panel",
          current_on_x = FALSE,
          primary_metric = "WDR",
          secondary_metric = "wBHR",
          strat_target = "secondary",
          strat_key = legacy_strat_key
        )
      )

      reloaded <- load_scatter_panel_state("perRiffle", force = TRUE)
      session$flushReact()
      session$flushReact()

      reloaded_panels <- scatter_panel_store()
      reloaded_shells <- scatter_panel_shells()
      reloaded_ui_html <- htmltools::renderTags(output$scatter_panel_list_ui)$html
      stopifnot(identical(scatter_panel_active_tab(), "panel_1"))
      stopifnot(identical(reloaded$panels[[1]]$compare_metric, "WDR"))
      stopifnot(identical(reloaded_panels[[1]]$compare_metric, "WDR"))
      stopifnot(identical(reloaded_panels[[1]]$current_on_x, FALSE))
      stopifnot(identical(reloaded_shells[[1]]$id, reloaded_panels[[1]]$id))
      stopifnot(identical(reloaded_shells[[1]]$label, reloaded_panels[[1]]$label))
      stopifnot(is.null(reloaded_panels[[1]]$secondary_metric))
      stopifnot(is.null(reloaded_panels[[1]]$strat_target))
      stopifnot(grepl('Legacy Panel', reloaded_ui_html, fixed = TRUE))
      stopifnot(grepl('value="WDR"', reloaded_ui_html, fixed = TRUE))

      cache_entry <- rv$metric_phase_cache[["perRiffle"]]
      stopifnot(identical(cache_entry$phase1_scatter_panels[[1]]$compare_metric, "WDR"))
      stopifnot(identical(cache_entry$phase1_scatter_panels[[1]]$current_on_x, FALSE))
      stopifnot(is.null(cache_entry$phase1_scatter_active_panel))
    }
  )
}

check_single_compare_panels_stay_stable()

cat("Phase 1 scatter panel redesign checks passed.\n")
