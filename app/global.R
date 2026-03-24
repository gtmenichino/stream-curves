## Global setup
## Runs once when the app launches. Sources pipeline modules and loads
## configs. The app starts in a no-data state until the user uploads a dataset.

options(
  shiny.maxRequestSize = 50 * 1024^2,
  shiny.launch.browser = function(url) utils::browseURL(url)
)

## Working directory: project root (one level up from app/)
project_root <- normalizePath(file.path(dirname(getwd()), "."), winslash = "/")
if (basename(getwd()) == "app") {
  project_root <- normalizePath("..", winslash = "/")
} else {
  project_root <- normalizePath(".", winslash = "/")
}

is_connect_cloud_runtime <- function() {
  if (nzchar(Sys.getenv("RSC_PRIMARY_DOC", ""))) {
    return(TRUE)
  }

  rsc_env <- Sys.getenv()
  any(startsWith(names(rsc_env), "RSC_") & nzchar(unname(rsc_env)))
}

connect_cloud_runtime <- is_connect_cloud_runtime()

## Load packages
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(DT)
  library(shinyWidgets)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggpubr)
  library(patchwork)
  library(tibble)
  library(purrr)
  library(yaml)
  library(readr)
  library(readxl)
  library(stringr)
  library(forcats)
  library(leaps)
  library(MASS)
  library(lmtest)
  library(car)
  library(broom)
  library(cli)
})

## Source pipeline modules
r_dir <- file.path(project_root, "R")
source(file.path(r_dir, "01_load_data.R"), local = TRUE)
source(file.path(r_dir, "02_clean_data.R"), local = TRUE)
source(file.path(r_dir, "03_derive_variables.R"), local = TRUE)
source(file.path(r_dir, "04_metric_precheck.R"), local = TRUE)
source(file.path(r_dir, "05_stratification_screening.R"), local = TRUE)
source(file.path(r_dir, "05b_effect_size.R"), local = TRUE)
source(file.path(r_dir, "05c_pattern_stability.R"), local = TRUE)
source(file.path(r_dir, "05d_cross_metric_consistency.R"), local = TRUE)
source(file.path(r_dir, "05e_feasibility.R"), local = TRUE)
source(file.path(r_dir, "06_stratification_decision.R"), local = TRUE)
source(file.path(r_dir, "07_model_candidates.R"), local = TRUE)
source(file.path(r_dir, "08_model_selection.R"), local = TRUE)
source(file.path(r_dir, "09_diagnostics.R"), local = TRUE)
source(file.path(r_dir, "10_reference_curves.R"), local = TRUE)
source(file.path(r_dir, "11_cross_metric.R"), local = TRUE)
source(file.path(r_dir, "12_regional_curves.R"), local = TRUE)

## Source app helpers
source("helpers/theme.R", local = TRUE)
source("helpers/badges.R", local = TRUE)
source("helpers/phase_tracker.R", local = TRUE)
source("helpers/summary_page.R", local = TRUE)

## Source app modules
source("modules/mod_data_overview.R", local = TRUE)
source("modules/mod_precheck.R", local = TRUE)
source("modules/mod_ref_curve.R", local = TRUE)
source("modules/mod_regional_curve.R", local = TRUE)
source("modules/mod_config_editor.R", local = TRUE)

## Source 4-phase modules
source("modules/mod_landing_v2.R", local = TRUE)
source("modules/mod_phase1_exploration.R", local = TRUE)
source("modules/mod_phase2_consistency.R", local = TRUE)
source("modules/mod_phase3_verification.R", local = TRUE)
source("modules/mod_phase4_finalization.R", local = TRUE)
source("modules/mod_summary_page.R", local = TRUE)
source("modules/mod_summary_export.R", local = TRUE)

## Load configs
config_dir <- file.path(project_root, "config")
metric_config <- yaml::read_yaml(file.path(config_dir, "metric_registry.yaml"))
strat_config <- yaml::read_yaml(file.path(config_dir, "stratification_registry.yaml"))
predictor_config <- yaml::read_yaml(file.path(config_dir, "predictor_registry.yaml"))
factor_recode_config <- yaml::read_yaml(file.path(config_dir, "factor_recode_registry.yaml"))
output_config <- yaml::read_yaml(file.path(config_dir, "output_registry.yaml"))

## Create sessions directory
if (!isTRUE(connect_cloud_runtime)) {
  dir.create(
    file.path(project_root, "outputs", "sessions"),
    showWarnings = FALSE,
    recursive = TRUE
  )
}

cli::cli_alert_success("App startup complete: configs loaded, awaiting dataset upload")
