## ── Pipeline Orchestrator ────────────────────────────────────────────────────
## Loops over metrics and calls modules 01–10 in sequence.

library(yaml)
library(cli)
library(readr)

#' Run the full StreamCurves analysis pipeline
#'
#' @param input_path Path to the input .xlsx workbook
#' @param config_dir Path to config directory (default "config")
#' @param data_dir Path to data directory (default "data")
#' @param output_dir Path to outputs directory (default "outputs")
#' @param metrics Character vector of metric keys to process (NULL = all)
#' @param seed Random seed (default 42)
#' @param n_cores Number of parallel workers (default: all cores minus 1)
#' @return invisible(run_dir) — path to the timestamped output directory
run_pipeline <- function(input_path = NULL,
                         config_dir = "config",
                         data_dir   = "data",
                         output_dir = "outputs",
                         metrics    = NULL,
                         seed       = 42,
                         n_cores    = max(1L, parallel::detectCores() - 1L)) {

  pipeline_start <- Sys.time()
  set.seed(seed)

  if (is.null(input_path) || !nzchar(input_path)) {
    stop(
      "input_path is required. Provide the path to your input .xlsx workbook when calling run_pipeline().",
      call. = FALSE
    )
  }

  if (tolower(tools::file_ext(input_path)) != "xlsx") {
    stop(
      "run_pipeline() requires an .xlsx workbook input.",
      call. = FALSE
    )
  }

  if (!file.exists(input_path)) {
    stop(
      paste0("Input workbook not found: ", input_path),
      call. = FALSE
    )
  }

  ## ── Set up parallel backend ──────────────────────────────────────────────
  if (n_cores > 1L && requireNamespace("future", quietly = TRUE) &&
      requireNamespace("furrr", quietly = TRUE)) {
    future::plan(future::multisession, workers = n_cores)
    on.exit(future::plan(future::sequential), add = TRUE)
    cli::cli_alert_info("Parallel execution: {n_cores} workers")
  } else {
    n_cores <- 1L
    if (!requireNamespace("future", quietly = TRUE) || !requireNamespace("furrr", quietly = TRUE)) {
      cli::cli_alert_warning(
        "Packages {.pkg future} and/or {.pkg furrr} not installed \u2014 running sequentially"
      )
      cli::cli_alert_info("Install with: {.code install.packages(c('future', 'furrr'))}")
    } else {
      cli::cli_alert_info("Sequential execution (n_cores = 1)")
    }
  }

  ## ── Create timestamped output directory ───────────────────────────────────
  run_id <- format(Sys.time(), "%Y%m%d_%H%M")
  run_dir <- file.path(output_dir, paste0("run_", run_id))
  dirs <- c("tables", "plots", "rds", "logs", "manifest", "manifest/config_snapshot")
  for (d in file.path(run_dir, dirs)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  cli::cli_h1("StreamCurves Analysis Pipeline")
  cli::cli_alert_info("Run ID: {run_id}")
  cli::cli_alert_info("Output: {run_dir}")

  ## ── Load output config ────────────────────────────────────────────────────
  cli::cli_h2("Loading output configuration")
  output_config <- yaml::read_yaml(file.path(config_dir, "output_registry.yaml"))
  cli::cli_alert_success("Loaded output_registry.yaml")

  ## ── Source analysis modules ───────────────────────────────────────────────
  cli::cli_h2("Loading analysis modules")

  module_files <- c(
    "R/00_manifest.R",
    "R/00_input_workbook.R",
    "R/01_load_data.R",
    "R/02_clean_data.R",
    "R/03_derive_variables.R",
    "R/04_metric_precheck.R",
    "R/05_stratification_screening.R",
    "R/06_stratification_decision.R",
    "R/07_model_candidates.R",
    "R/08_model_selection.R",
    "R/09_diagnostics.R",
    "R/10_reference_curves.R"
  )

  for (f in module_files) {
    if (file.exists(f)) {
      source(f, local = FALSE)
      cli::cli_alert_success("Loaded {f}")
    } else {
      cli::cli_alert_warning("Module not found: {f}")
    }
  }

  ## Optional extended modules
  extended_modules <- c("R/11_cross_metric.R", "R/12_regional_curves.R")
  for (f in extended_modules) {
    if (file.exists(f)) {
      source(f, local = FALSE)
      cli::cli_alert_success("Loaded {f}")
    }
  }

  ## ── Pipeline log ──────────────────────────────────────────────────────────
  pipeline_log <- tibble::tibble(
    step = character(), metric = character(), status = character(),
    time_sec = numeric(), message = character()
  )

  log_step <- function(step, metric = "all", status = "ok", time_sec = 0, msg = "") {
    pipeline_log <<- dplyr::bind_rows(pipeline_log, tibble::tibble(
      step = step, metric = metric, status = status,
      time_sec = time_sec, message = msg
    ))
  }

  ## ── Step 1: Load workbook ─────────────────────────────────────────────────
  cli::cli_h2("Step 1/12: Loading workbook")
  t0 <- Sys.time()

  input_bundle <- tryCatch({
    load_data(input_path)
  }, error = function(e) {
    cli::cli_alert_danger("Workbook loading failed: {e$message}")
    log_step("load_workbook", status = "error", msg = e$message)
    return(NULL)
  })

  if (is.null(input_bundle)) {
    cli::cli_alert_danger("Pipeline aborted: workbook loading failed")
    return(invisible(NULL))
  }
  log_step("load_workbook", time_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))

  raw_data <- input_bundle$raw_data
  metric_config <- input_bundle$metric_config
  strat_config <- input_bundle$strat_config
  predictor_config <- input_bundle$predictor_config
  factor_recode_config <- input_bundle$factor_recode_config

  if (!is.null(metrics)) {
    metric_config <- metric_config[names(metric_config) %in% metrics]
    cli::cli_alert_info("Filtered to {length(metric_config)} metrics: {paste(names(metric_config), collapse = ', ')}")
  }

  ## ── Step 2: Clean data ────────────────────────────────────────────────────
  cli::cli_h2("Step 2/12: Cleaning data")
  t0 <- Sys.time()

  clean_result <- tryCatch(
    clean_data(raw_data, metric_config, strat_config, factor_recode_config),
    error = function(e) {
      cli::cli_alert_danger("Data cleaning failed: {e$message}")
      log_step("clean_data", status = "error", msg = e$message)
      return(NULL)
    }
  )

  if (is.null(clean_result)) {
    cli::cli_alert_danger("Pipeline aborted: data cleaning failed")
    return(invisible(NULL))
  }

  data <- clean_result$data
  qa_log <- clean_result$qa_log
  log_step("clean_data", time_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))

  ## ── Step 3: Derive variables ──────────────────────────────────────────────
  cli::cli_h2("Step 3/12: Deriving variables")
  t0 <- Sys.time()

  data <- tryCatch(
    derive_variables(data, factor_recode_config, predictor_config, strat_config),
    error = function(e) {
      cli::cli_alert_danger("Variable derivation failed: {e$message}")
      log_step("derive_variables", status = "error", msg = e$message)
      return(data)
    }
  )

  ## Save clean data
  saveRDS(data, file.path(data_dir, "derived", "stream_clean.rds"))
  cli::cli_alert_success("Saved {.file {file.path(data_dir, 'derived', 'stream_clean.rds')}}")
  log_step("derive_variables", time_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))

  ## ── Step 4: Metric precheck ───────────────────────────────────────────────
  cli::cli_h2("Step 4/12: Metric precheck")
  t0 <- Sys.time()

  precheck_df <- tryCatch(
    run_metric_precheck(data, metric_config),
    error = function(e) {
      cli::cli_alert_danger("Metric precheck failed: {e$message}")
      log_step("metric_precheck", status = "error", msg = e$message)
      return(tibble::tibble())
    }
  )

  if (nrow(precheck_df) > 0) {
    readr::write_csv(precheck_df, file.path(run_dir, "tables", "metric_precheck.csv"))
  }
  log_step("metric_precheck", time_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))

  ## ── Step 5: Stratification screening ──────────────────────────────────────
  cli::cli_h2("Step 5/12: Stratification screening")
  t0 <- Sys.time()

  strat_screening <- tryCatch(
    run_all_stratification_screening(data, metric_config, strat_config),
    error = function(e) {
      cli::cli_alert_danger("Stratification screening failed: {e$message}")
      log_step("strat_screening", status = "error", msg = e$message)
      return(list(results = tibble::tibble(), pairwise = tibble::tibble(), plots = list()))
    }
  )

  if (nrow(strat_screening$results) > 0) {
    readr::write_csv(strat_screening$results, file.path(run_dir, "tables", "stratification_screening.csv"))
  }
  if (nrow(strat_screening$pairwise) > 0) {
    readr::write_csv(strat_screening$pairwise, file.path(run_dir, "tables", "stratification_pairwise.csv"))
  }

  ## Save plots
  for (plot_name in names(strat_screening$plots)) {
    tryCatch({
      ggplot2::ggsave(
        filename = file.path(run_dir, "plots", paste0(plot_name, ".png")),
        plot = strat_screening$plots[[plot_name]],
        width = 8, height = 6, dpi = 300
      )
    }, error = function(e) {
      cli::cli_alert_warning("Failed to save plot {plot_name}: {e$message}")
    })
  }

  log_step("strat_screening", time_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))

  ## ── Step 6: Stratification decisions ──────────────────────────────────────
  cli::cli_h2("Step 6/12: Stratification decisions")
  t0 <- Sys.time()

  strat_decisions <- tryCatch(
    make_stratification_decisions(
      strat_screening$results, strat_screening$pairwise,
      metric_config, strat_config
    ),
    error = function(e) {
      cli::cli_alert_danger("Stratification decisions failed: {e$message}")
      log_step("strat_decisions", status = "error", msg = e$message)
      return(tibble::tibble())
    }
  )

  if (nrow(strat_decisions) > 0) {
    readr::write_csv(strat_decisions, file.path(run_dir, "tables", "stratification_decisions.csv"))
  }
  log_step("strat_decisions", time_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))

  ## ── Step 7: Model building ────────────────────────────────────────────────
  cli::cli_h2("Step 7/12: Model building")
  t0 <- Sys.time()

  model_results <- tryCatch(
    run_all_model_building(data, strat_decisions, metric_config, predictor_config, strat_config),
    error = function(e) {
      cli::cli_alert_danger("Model building failed: {e$message}")
      log_step("model_building", status = "error", msg = e$message)
      return(list(all_candidates = tibble::tibble(), all_importance = tibble::tibble(), all_plots = list()))
    }
  )

  if (nrow(model_results$all_candidates) > 0) {
    readr::write_csv(model_results$all_candidates, file.path(run_dir, "tables", "model_candidates.csv"))
  }
  if (nrow(model_results$all_importance) > 0) {
    readr::write_csv(model_results$all_importance, file.path(run_dir, "tables", "predictor_importance.csv"))
  }

  for (plot_name in names(model_results$all_plots)) {
    tryCatch({
      ggplot2::ggsave(
        filename = file.path(run_dir, "plots", paste0(plot_name, ".png")),
        plot = model_results$all_plots[[plot_name]],
        width = 8, height = 6, dpi = 300
      )
    }, error = function(e) {
      cli::cli_alert_warning("Failed to save plot {plot_name}: {e$message}")
    })
  }

  log_step("model_building", time_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))

  ## ── Step 8: Model selection ───────────────────────────────────────────────
  cli::cli_h2("Step 8/12: Model selection")
  t0 <- Sys.time()

  model_selections <- tryCatch(
    select_final_models(model_results$all_candidates, model_results$all_importance, metric_config),
    error = function(e) {
      cli::cli_alert_danger("Model selection failed: {e$message}")
      log_step("model_selection", status = "error", msg = e$message)
      return(tibble::tibble())
    }
  )

  if (nrow(model_selections) > 0) {
    readr::write_csv(model_selections, file.path(run_dir, "tables", "model_selections.csv"))
  }
  log_step("model_selection", time_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))

  ## ── Step 9: Diagnostics ───────────────────────────────────────────────────
  cli::cli_h2("Step 9/12: Diagnostics")
  t0 <- Sys.time()

  diagnostic_results <- tryCatch(
    run_all_diagnostics(data, model_selections, strat_decisions, metric_config),
    error = function(e) {
      cli::cli_alert_danger("Diagnostics failed: {e$message}")
      log_step("diagnostics", status = "error", msg = e$message)
      return(list(summary_df = tibble::tibble(), all_plots = list(), all_models = list()))
    }
  )

  if (nrow(diagnostic_results$summary_df) > 0) {
    readr::write_csv(diagnostic_results$summary_df, file.path(run_dir, "tables", "diagnostic_summary.csv"))
  }

  for (plot_name in names(diagnostic_results$all_plots)) {
    tryCatch({
      suppressWarnings(
        ggplot2::ggsave(
          filename = file.path(run_dir, "plots", paste0(plot_name, ".png")),
          plot = diagnostic_results$all_plots[[plot_name]],
          width = 10, height = 8, dpi = 300
        )
      )
    }, error = function(e) {
      cli::cli_alert_warning("Failed to save diagnostic plot {plot_name}: {e$message}")
    })
  }

  ## Save model objects
  for (mname in names(diagnostic_results$all_models)) {
    saveRDS(diagnostic_results$all_models[[mname]],
            file.path(run_dir, "rds", paste0(mname, "_model_object.rds")))
  }

  log_step("diagnostics", time_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))

  ## ── Step 10: Reference curves ─────────────────────────────────────────────
  cli::cli_h2("Step 10/12: Reference curves")
  t0 <- Sys.time()

  ref_curves <- tryCatch(
    run_all_reference_curves(
      data, model_selections, diagnostic_results$summary_df,
      diagnostic_results$all_models, metric_config
    ),
    error = function(e) {
      cli::cli_alert_danger("Reference curves failed: {e$message}")
      log_step("reference_curves", status = "error", msg = e$message)
      return(list(registry = tibble::tibble(), bar_chart_plots = list(), curve_plots = list()))
    }
  )

  if (nrow(ref_curves$registry) > 0) {
    readr::write_csv(ref_curves$registry, file.path(run_dir, "tables", "reference_curves.csv"))
  }

  for (plot_name in names(ref_curves$bar_chart_plots)) {
    tryCatch({
      ggplot2::ggsave(
        filename = file.path(run_dir, "plots", paste0(plot_name, "_reference_bar_chart.png")),
        plot = ref_curves$bar_chart_plots[[plot_name]],
        width = 8, height = 6, dpi = 300
      )
    }, error = function(e) {
      cli::cli_alert_warning("Failed to save reference bar chart plot {plot_name}: {e$message}")
    })
  }

  for (plot_name in names(ref_curves$curve_plots)) {
    tryCatch({
      ggplot2::ggsave(
        filename = file.path(run_dir, "plots", paste0(plot_name, "_reference_curve.png")),
        plot = ref_curves$curve_plots[[plot_name]],
        width = 8, height = 6, dpi = 300
      )
    }, error = function(e) {
      cli::cli_alert_warning("Failed to save reference curve plot {plot_name}: {e$message}")
    })
  }

  log_step("reference_curves", time_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))

  ## ── Extended analysis (if modules available) ──────────────────────────────

  ## Cross-metric analysis
  if (exists("run_cross_metric_analysis", mode = "function")) {
    cli::cli_h2("Step 11/12: Cross-metric analysis")
    t0 <- Sys.time()

    cross_metric <- tryCatch(
      run_cross_metric_analysis(data, metric_config, ref_curves$registry),
      error = function(e) {
        cli::cli_alert_warning("Cross-metric analysis failed: {e$message}")
        log_step("cross_metric", status = "error", msg = e$message)
        return(list(results = tibble::tibble(), plots = list()))
      }
    )

    if (!is.null(cross_metric$results) && nrow(cross_metric$results) > 0) {
      readr::write_csv(cross_metric$results, file.path(run_dir, "tables", "metric_redundancy_results.csv"))
    }
    for (pname in names(cross_metric$plots)) {
      tryCatch({
        ggplot2::ggsave(
          filename = file.path(run_dir, "plots", paste0(pname, ".png")),
          plot = cross_metric$plots[[pname]],
          width = 10, height = 8, dpi = 300
        )
      }, error = function(e) NULL)
    }

    log_step("cross_metric", time_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  }

  ## Regional curves
  if (exists("run_regional_curves", mode = "function")) {
    cli::cli_h2("Step 12/12: Regional curves")
    t0 <- Sys.time()

    regional <- tryCatch(
      run_regional_curves(data, metric_config),
      error = function(e) {
        cli::cli_alert_warning("Regional curves failed: {e$message}")
        log_step("regional_curves", status = "error", msg = e$message)
        return(list(results = tibble::tibble(), plots = list()))
      }
    )

    if (!is.null(regional$results) && nrow(regional$results) > 0) {
      readr::write_csv(regional$results, file.path(run_dir, "tables", "regional_curves_results.csv"))
    }
    for (pname in names(regional$plots)) {
      tryCatch({
        ggplot2::ggsave(
          filename = file.path(run_dir, "plots", paste0(pname, ".png")),
          plot = regional$plots[[pname]],
          width = 8, height = 6, dpi = 300
        )
      }, error = function(e) NULL)
    }

    log_step("regional_curves", time_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  }

  ## ── Manifest ──────────────────────────────────────────────────────────────
  if (exists("create_run_manifest", mode = "function")) {
    cli::cli_h2("Creating run manifest")
    manifest <- create_run_manifest(run_dir, config_dir, data_dir, input_path = input_path)
    save_manifest(manifest, run_dir, config_dir = config_dir, input_path = input_path)
  }

  ## ── Save pipeline log ─────────────────────────────────────────────────────
  readr::write_csv(pipeline_log, file.path(run_dir, "logs", "pipeline_log.csv"))

  ## Save QA log
  if (exists("qa_log") && nrow(qa_log) > 0) {
    readr::write_csv(qa_log, file.path(run_dir, "logs", "qa_log.csv"))
  }

  ## ── Summary ───────────────────────────────────────────────────────────────
  pipeline_end <- Sys.time()
  elapsed <- as.numeric(difftime(pipeline_end, pipeline_start, units = "secs"))

  cli::cli_h1("Pipeline Complete")
  cli::cli_alert_success("Total time: {round(elapsed, 1)} seconds")
  cli::cli_alert_success("Output directory: {run_dir}")

  if (nrow(model_selections) > 0) {
    cli::cli_alert_info("Metrics processed: {nrow(model_selections)}")
  }
  if (nrow(diagnostic_results$summary_df) > 0) {
    diag_df <- diagnostic_results$summary_df
    n_pass <- sum(diag_df$overall_status == "pass", na.rm = TRUE)
    n_caution <- sum(diag_df$overall_status == "caution", na.rm = TRUE)
    n_fail <- sum(diag_df$overall_status == "fail", na.rm = TRUE)
    cli::cli_alert_info("Diagnostics: {n_pass} pass | {n_caution} caution | {n_fail} fail")
    cli::cli_alert_info("  (pass = all assumptions met; caution = marginal; fail = assumption violated)")
    if (n_fail > 0) {
      fail_metrics <- diag_df |> dplyr::filter(overall_status == "fail") |> dplyr::pull(metric)
      cli::cli_alert_warning("  Failed metrics: {paste(fail_metrics, collapse = ', ')}")
    }
  }
  if (nrow(ref_curves$registry) > 0) {
    cli::cli_alert_info("Reference curves: {sum(ref_curves$registry$curve_status == 'complete')} complete")
  }

  invisible(run_dir)
}
