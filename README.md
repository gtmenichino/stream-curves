# StreamCurves

StreamCurves is an R pipeline and Shiny application for evaluating stream and geomorphic metrics, selecting defensible stratifications, and building reference and regional curves from a config-driven workflow.

The repository includes:

- A Shiny app for interactive data loading, reference-curve analysis, and regional-curve analysis
- A scripted pipeline for running the same analysis from the command line
- YAML registries that define metrics, predictors, stratifications, and output behavior
- Report templates, archive material, and reference documents

## Requirements

- R 4.3 or newer
- RStudio is recommended for interactive use

Install the main packages before running the project:

```r
install.packages(c(
  "broom", "bsicons", "bslib", "car", "cli", "DT",
  "dplyr", "forcats", "future", "furrr", "ggplot2", "ggpubr",
  "jsonlite", "leaps", "lmtest", "MASS", "patchwork", "purrr",
  "readr", "readxl", "shiny", "shinyWidgets", "stringr",
  "tibble", "tidyr", "yaml"
))
```

Optional packages for report rendering:

```r
install.packages(c("gt", "quarto", "reactable"))
```

## Quick Start

1. Clone the repository.
2. Open `stream-curves.Rproj` in RStudio, or start R in the project root.
3. Install the required packages listed above.
4. Run either the Shiny app or the scripted pipeline.

## Run the Shiny App

```r
shiny::runApp("app")
```

This is the recommended local-development entrypoint. The repo-root `app.R` is reserved for Posit Connect Cloud deployment.

The app starts without data and accepts user uploads in CSV or XLSX format.

Current top-level app areas:

- `Data & Setup`: upload a dataset, review validation output, and edit config-backed settings
- `Reference Curves`: recompute per-metric reference-curve analyses and open the four analysis workspaces
- `Regional Curves`: fit and review regional-curve models

## Run the Pipeline

Run the full pipeline:

```r
source("run_all.R")
```

Set `csv_path` in `run_all.R` before running it.

Run a single metric:

```r
source("R/run_pipeline.R")
run_pipeline(csv_path = "path/to/data.csv", metrics = "perRiffle")
```

Outputs are written to timestamped folders under `outputs/`, which is intentionally ignored from version control.

## Deploy to Posit Connect Cloud

Connect Cloud requires a committed `manifest.json` in the same directory as the deployment primary file. In this repository, the deployment primary file is the repo-root `app.R`.

1. Install the app dependencies from the Requirements section.
2. Install `rsconnect` if it is not already available:

```r
install.packages("rsconnect")
```

3. Regenerate the deployment manifest from the repo root:

```r
source("scripts/write_connect_manifest.R")
```

4. Commit and push the updated `manifest.json`.
5. In Posit Connect Cloud, publish or republish this repository using the repo-root `app.R` as the primary file.

Notes:

- The local app entrypoint remains `app/app.R`, typically launched with `shiny::runApp("app")`.
- Session save/load is disabled in the cloud deployment because Connect Cloud does not provide durable app-local file persistence for that feature.

## Repository Layout

```text
stream-curves/
|- app/          Shiny app entrypoints, modules, helpers, and static assets
|- app.R         Repo-root deployment entrypoint for Connect Cloud
|- R/            Analysis pipeline functions
|- config/       YAML registries for metrics, predictors, stratifications, and outputs
|- data/
|  `- derived/   Derived datasets created by the pipeline (ignored)
|- reports/      Quarto report templates
|- scripts/      Project utility scripts, including manifest generation
|- docs/         Reference material retained with the project
|- archive/      Historical prototypes and legacy reference files
|- outputs/      Run outputs and saved sessions (ignored)
|- run_all.R     Pipeline entrypoint
`- .Rprofile     Project options and reproducibility seed
```

## Configuration

All analysis behavior is controlled by YAML files in `config/`:

- `metric_registry.yaml`
- `stratification_registry.yaml`
- `predictor_registry.yaml`
- `factor_recode_registry.yaml`
- `output_registry.yaml`

The app and the scripted pipeline both read from the same configuration layer.

## Data Notes

- Uploaded datasets can be provided as CSV or XLSX in the app.
- The scripted pipeline currently accepts CSV input via the required `csv_path` argument.
- Character fields are trimmed during import, and derived variables are computed when required source columns are available.

## Reports

The `reports/` directory contains Quarto templates for dashboard, summary, poster, and appendix outputs. These templates read analysis outputs produced by the pipeline rather than recomputing results on their own.

## Included Reference Material

- `docs/` contains supporting reference documents used during development
- `archive/` contains historical prototypes and legacy files that are kept for context, not active runtime use

## Version Control Notes

- Generated outputs are ignored.
- Local R session files are ignored.
- Local assistant and planning artifacts are ignored.
