suppressPackageStartupMessages({
  library(shiny)
  library(tibble)
})

project_root <- normalizePath(".", winslash = "/")
connect_cloud_runtime <- FALSE

source("tests/local_workbook_helper.R", local = TRUE)
source("R/00_input_workbook.R", local = TRUE)
source("R/02_clean_data.R", local = TRUE)
source("R/03_derive_variables.R", local = TRUE)
source("R/04_metric_precheck.R", local = TRUE)
source("app/helpers/notifications.R", local = TRUE)
source("app/helpers/phase_tracker.R", local = TRUE)
source("app/helpers/summary_page.R", local = TRUE)
source("app/modules/mod_data_overview.R", local = TRUE)

workbook_path <- require_streamcurves_test_workbook("session_file_persistence_checks", project_root = project_root)
workbook_name <- basename(workbook_path)

make_session_rv <- function() {
  input_bundle <- read_input_workbook(workbook_path)
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

  list2env(list(
    current_metric = "perRiffle",
    data = derived,
    qa_log = clean_result$qa_log,
    precheck_df = precheck,
    data_source = "upload",
    data_fingerprint = digest::digest(derived, algo = "md5"),
    upload_filename = workbook_name,
    session_name = NULL,
    input_metadata = input_bundle$metadata,
    site_mask_config = input_bundle$site_mask_config,
    metric_config = input_bundle$metric_config,
    strat_config = input_bundle$strat_config,
    predictor_config = input_bundle$predictor_config,
    factor_recode_config = input_bundle$factor_recode_config,
    output_config = list(example = TRUE),
    config_version = 2L,
    phase1_candidates = list(),
    all_layer1_results = list(),
    all_layer2_results = list(),
    phase2_ranking = NULL,
    cross_metric_consistency = NULL,
    phase2_settings = empty_phase2_settings(),
    phase2_metric_overrides = list(),
    curve_stratification = list(perRiffle = "Ecoregion"),
    summary_available_overrides = list(),
    summary_edit_notes = list(),
    phase3_verification = list(),
    metric_phase_cache = list(
      perRiffle = list(
        phase1_quick_mask_site_ids = c(2L, 4L)
      )
    ),
    stratum_results = list(),
    completed_metrics = list(),
    decision_log = tibble(),
    custom_groupings = list(),
    custom_grouping_counter = list(),
    phase1_screening = list(
      results = tibble(stratification = "Ecoregion"),
      pairwise = tibble(),
      plots = list(),
      plot_specs = list()
    ),
    phase1_effect_sizes = tibble(stratification = "Ecoregion", epsilon_squared = 0.1),
    phase3_patterns = NULL,
    phase3_feasibility = NULL,
    strat_decision_user = tibble(metric = "perRiffle", decision_type = "single", selected_strat = "Ecoregion"),
    reference_curve = NULL,
    current_stratum_level = NULL,
    phase4_data = NULL
  ), parent = emptyenv())
}

check_session_snapshot_is_self_contained <- function() {
  rv <- make_session_rv()
  session_data <- build_session_snapshot(rv, "QA Session")

  stopifnot(identical(session_data$version, "4.0"))
  stopifnot(identical(session_data$session_name, "QA Session"))
  stopifnot(is.data.frame(session_data$data))
  stopifnot(!is.null(session_data$qa_log))
  stopifnot(!is.null(session_data$precheck_df))
  stopifnot(identical(session_data$site_mask_config$site_label_column, "ID"))
  stopifnot(identical(session_data$curve_stratification$perRiffle, "Ecoregion"))
  stopifnot("perRiffle" %in% names(session_data$metric_phase_cache))
  stopifnot(!is.null(session_data$metric_phase_cache$perRiffle$phase1_screening))
  stopifnot(identical(
    as.integer(session_data$metric_phase_cache$perRiffle$phase1_quick_mask_site_ids),
    c(2L, 4L)
  ))
}

check_session_snapshot_rebuilds_from_metadata <- function() {
  rv <- make_session_rv()
  session_data <- build_session_snapshot(rv, "Legacy-like Session")

  session_data$data <- NULL
  session_data$qa_log <- NULL
  session_data$precheck_df <- NULL
  session_data$metric_config <- NULL
  session_data$strat_config <- NULL
  session_data$predictor_config <- NULL
  session_data$factor_recode_config <- NULL

  rebuilt <- resolve_session_snapshot(session_data)

  stopifnot(is.data.frame(rebuilt$data))
  stopifnot(nrow(rebuilt$data) == nrow(rv$data))
  stopifnot(length(rebuilt$metric_config) == length(rv$metric_config))
  stopifnot(length(rebuilt$strat_config) == length(rv$strat_config))
  stopifnot(!is.null(rebuilt$precheck_df))
  stopifnot(identical(rebuilt$site_mask_config$site_label_column, "ID"))
}

check_invalid_session_payload_errors <- function() {
  failed <- try(resolve_session_snapshot(123), silent = TRUE)
  stopifnot(inherits(failed, "try-error"))
}

make_data_overview_test_rv <- function() {
  loaded_rv <- make_session_rv()

  shiny::reactiveValues(
    data = NULL,
    qa_log = NULL,
    precheck_df = NULL,
    data_source = NULL,
    data_fingerprint = NULL,
    upload_filename = NULL,
    input_metadata = NULL,
    site_mask_config = NULL,
    metric_config = deep_copy_value(loaded_rv$metric_config),
    strat_config = deep_copy_value(loaded_rv$strat_config),
    predictor_config = deep_copy_value(loaded_rv$predictor_config),
    factor_recode_config = deep_copy_value(loaded_rv$factor_recode_config),
    output_config = deep_copy_value(loaded_rv$output_config),
    startup_metric_config = deep_copy_value(loaded_rv$metric_config),
    startup_strat_config = deep_copy_value(loaded_rv$strat_config),
    startup_predictor_config = deep_copy_value(loaded_rv$predictor_config),
    startup_factor_recode_config = deep_copy_value(loaded_rv$factor_recode_config),
    startup_output_config = deep_copy_value(loaded_rv$output_config),
    startup_config_version = 0L,
    startup_current_metric = "perRiffle",
    config_version = 0L,
    current_metric = "perRiffle",
    phase1_screening = NULL,
    phase1_effect_sizes = NULL,
    phase1_candidates = list(),
    all_layer1_results = list(),
    all_layer2_results = list(),
    phase2_ranking = NULL,
    cross_metric_consistency = NULL,
    phase2_settings = empty_phase2_settings(),
    phase2_metric_overrides = list(),
    curve_stratification = list(),
    summary_available_overrides = list(),
    summary_edit_notes = list(),
    phase3_patterns = NULL,
    phase3_feasibility = NULL,
    phase3_verification = list(),
    strat_decision_user = NULL,
    reference_curve = NULL,
    phase4_data = NULL,
    current_stratum_level = NULL,
    stratum_results = list(),
    completed_metrics = list(),
    decision_log = tibble(),
    metric_phase_cache = list(),
    custom_groupings = list(),
    custom_grouping_counter = list(),
    session_name = NULL,
    app_data_loaded = FALSE,
    app_reset_nonce = 0L,
    workspace_refresh_nonce = 0L
  )
}

check_session_upload_restores_data_overview_state <- function() {
  rv <- make_data_overview_test_rv()
  session_data <- build_session_snapshot(make_session_rv(), "Uploaded Session")
  tmp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_file), add = TRUE)
  saveRDS(session_data, tmp_file)

  shiny::testServer(
    mod_data_overview_server,
    args = list(rv = rv),
    {
      session$setInputs(load_session_file = list(
        name = "uploaded_session.rds",
        datapath = tmp_file
      ))
      session$flushReact()

      stopifnot(isTRUE(rv$app_data_loaded))
      stopifnot(is.data.frame(rv$data))
      stopifnot(!is.null(rv$input_metadata))
      stopifnot(identical(rv$current_metric, "perRiffle"))
      stopifnot(identical(rv$session_name, "Uploaded Session"))
      stopifnot(identical(rv$curve_stratification$perRiffle, "Ecoregion"))
      stopifnot("perRiffle" %in% names(rv$metric_phase_cache))
      stopifnot(identical(
        as.integer(rv$metric_phase_cache$perRiffle$phase1_quick_mask_site_ids),
        c(2L, 4L)
      ))
      stopifnot(identical(rv$upload_filename, workbook_name))
    }
  )
}

check_session_snapshot_is_self_contained()
check_session_snapshot_rebuilds_from_metadata()
check_invalid_session_payload_errors()
check_session_upload_restores_data_overview_state()

cat("session_file_persistence_checks: OK\n")
