## ── Global Setup ─────────────────────────────────────────────────────────────
## Runs once when the app launches. Sources pipeline modules, loads configs
## and data, precomputes precheck.

## ── Upload size limit (50 MB) ────────────────────────────────────────────────
options(
  shiny.maxRequestSize = 50 * 1024^2,
  shiny.launch.browser = function(url) utils::browseURL(url)
)

## ── Working directory: project root (one level up from app/) ─────────────────
project_root <- normalizePath(file.path(dirname(getwd()), "."), winslash = "/")
if (basename(getwd()) == "app") {
  project_root <- normalizePath("..", winslash = "/")
} else {
  project_root <- normalizePath(".", winslash = "/")
}

## ── Load packages ────────────────────────────────────────────────────────────
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

## ── Source pipeline modules ──────────────────────────────────────────────────
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

## ── Source app helpers ───────────────────────────────────────────────────────
source("helpers/theme.R", local = TRUE)
source("helpers/badges.R", local = TRUE)
source("helpers/phase_tracker.R", local = TRUE)
source("helpers/summary_page.R", local = TRUE)

## ── Source app modules (unchanged sub-modules) ──────────────────────────────
source("modules/mod_data_overview.R", local = TRUE)
source("modules/mod_precheck.R", local = TRUE)
## Retired: model-fitting sub-modules (kept on disk for potential future use)
## source("modules/mod_model_build.R", local = TRUE)
## source("modules/mod_model_select.R", local = TRUE)
## source("modules/mod_diagnostics.R", local = TRUE)
source("modules/mod_ref_curve.R", local = TRUE)
source("modules/mod_regional_curve.R", local = TRUE)
source("modules/mod_config_editor.R", local = TRUE)

## ── Source new 4-phase modules ──────────────────────────────────────────────
source("modules/mod_landing_v2.R", local = TRUE)
source("modules/mod_phase1_exploration.R", local = TRUE)
source("modules/mod_phase2_consistency.R", local = TRUE)
source("modules/mod_phase3_verification.R", local = TRUE)
source("modules/mod_phase4_finalization.R", local = TRUE)
source("modules/mod_summary_page.R", local = TRUE)
source("modules/mod_summary_export.R", local = TRUE)

## ── Load configs ─────────────────────────────────────────────────────────────
config_dir <- file.path(project_root, "config")
metric_config    <- yaml::read_yaml(file.path(config_dir, "metric_registry.yaml"))
strat_config     <- yaml::read_yaml(file.path(config_dir, "stratification_registry.yaml"))
predictor_config <- yaml::read_yaml(file.path(config_dir, "predictor_registry.yaml"))
factor_recode_config <- yaml::read_yaml(file.path(config_dir, "factor_recode_registry.yaml"))
output_config    <- yaml::read_yaml(file.path(config_dir, "output_registry.yaml"))

## ── Load and prepare data ────────────────────────────────────────────────────
data_dir <- file.path(project_root, "data")
csv_path <- file.path(data_dir, "raw", "data_example.csv")
raw_data <- load_data(csv_path)
clean_result <- clean_data(raw_data, metric_config, strat_config)
data <- derive_variables(clean_result$data, factor_recode_config, predictor_config)
qa_log <- clean_result$qa_log

## ── Precompute precheck ──────────────────────────────────────────────────────
precheck_df <- run_metric_precheck(data, metric_config)

## ── Create sessions directory ────────────────────────────────────────────────
dir.create(file.path(project_root, "outputs", "sessions"),
           showWarnings = FALSE, recursive = TRUE)

cli::cli_alert_success("App startup complete: {nrow(data)} rows, {ncol(data)} cols, {nrow(precheck_df)} metrics prechecked")
