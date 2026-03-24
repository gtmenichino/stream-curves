# StreamCurves

StreamCurves is an R pipeline and Shiny application for evaluating stream and geomorphic metrics, selecting defensible stratifications, and building reference and regional curves from a config-driven workflow.

The repository includes:

- A Shiny app for interactive data loading, reference-curve analysis, and regional-curve analysis
- A scripted pipeline for running the same analysis from the command line
- YAML registries that define metrics, predictors, stratifications, and output behavior
- A small example dataset used by the app and pipeline
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

The app ships with a built-in example dataset and also accepts user uploads in CSV or XLSX format.

Current top-level app areas:

- `Data & Setup`: load demo data or upload a dataset, review validation output, and edit config-backed settings
- `Reference Curves`: recompute per-metric reference-curve analyses and open the four analysis workspaces
- `Regional Curves`: fit and review regional-curve models

## Run the Pipeline

Run the full pipeline:

```r
source("run_all.R")
```

Run a single metric:

```r
source("R/run_pipeline.R")
run_pipeline(metrics = "perRiffle")
```

Outputs are written to timestamped folders under `outputs/`, which is intentionally ignored from version control.

## Repository Layout

```text
stream-curves/
|- app/          Shiny app entrypoints, modules, helpers, and static assets
|- R/            Analysis pipeline functions
|- config/       YAML registries for metrics, predictors, stratifications, and outputs
|- data/
|  |- raw/       Source data files, including the example dataset
|  `- derived/   Derived datasets created by the pipeline (ignored)
|- reports/      Quarto report templates
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

- `data/raw/data_example.csv` is a small example dataset included for demonstration and testing.
- Uploaded datasets can be provided as CSV or XLSX.
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

