# StreamCurves

StreamCurves is an R pipeline and Shiny application for evaluating stream and geomorphic metrics, selecting defensible stratifications, and building reference and regional curves from a workbook-driven workflow.

The repository includes:

- A Shiny app for interactive data loading, reference-curve analysis, and regional-curve analysis
- A scripted pipeline for running the same analysis from the command line
- A workbook format that defines raw data, metrics, predictors, stratifications, and factor recodes
- A runtime output registry in `config/output_registry.yaml`
- Report templates and reference documents

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

The app starts without data and accepts workbook uploads in `.xlsx` format.

Current top-level app areas:

- `Data & Setup`: upload a workbook, review validation output, and inspect parsed workbook metadata
- `Reference Curves`: recompute per-metric reference-curve analyses and open the four analysis workspaces
- `Regional Curves`: fit and review regional-curve models

## Run the Pipeline

Run the full pipeline:

```r
source("run_all.R")
```

Set `input_path` in `run_all.R` before running it.

Run a single metric:

```r
source("R/run_pipeline.R")
run_pipeline(input_path = "path/to/data.xlsx", metrics = "perRiffle")
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
|- config/       Runtime output registry
|- data/
|  `- derived/   Derived datasets created by the pipeline (ignored)
|- reports/      Quarto report templates
|- scripts/      Project utility scripts, including manifest generation
|- docs/         Reference material retained with the project
|- outputs/      Run outputs and saved sessions (ignored)
|- run_all.R     Pipeline entrypoint
`- .Rprofile     Project options and reproducibility seed
```

## Workbook Format

The input workbook is the source of truth for raw data and analysis metadata. Core sheets:

- `data`
- `metrics`
- `metric_predictors`
- `metric_stratifications`
- `stratifications`
- `strat_groups`
- `predictors`
- `factor_recodes`

Optional site-mask sheets:

- `site_masks`
- `site_mask_settings`

Continuous custom stratifications are defined in `strat_groups.rule_expression` with comparisons joined by `&`. Supported operators are `<`, `<=`, `>`, and `>=`.

Examples:

```text
<= 1
> 1 & <= 5
> 5
```

Use `stratifications.strat_type = custom_group` with:

- `source_data_type = categorical` and `strat_groups.source_values` for categorical regrouping
- `source_data_type = continuous` and `strat_groups.rule_expression` for numeric binning

Derived predictors are defined in the `predictors` sheet. Factor recodes are defined in the `factor_recodes` sheet and can also be referenced as stratifications.

Global site masks are stored in:

- `site_masks.masked_sites`: 1-based site ids from the original `data` sheet
- `site_masks.site_label`: saved label text for masked sites
- `site_mask_settings.site_label_column`: the raw data column used to generate those labels

If the site-mask sheets are missing, the app defaults to no masks and uses the first `data` column as the label source.

The only remaining runtime config file outside the workbook is `config/output_registry.yaml`.

## Data Notes

- The app accepts `.xlsx` workbooks only.
- The scripted pipeline accepts `.xlsx` workbooks via the required `input_path` argument.
- The repository does not ship a workbook with data. Keep local workbooks untracked.
- Character fields are trimmed during import, and derived variables are computed when required source columns are available.

To generate a workbook from a source CSV and the YAML registries:

```bash
python scripts/migrate_registry_to_workbook.py --csv path/to/source.csv --output path/to/workbook.xlsx
```

For workbook-dependent tests, either set `STREAMCURVES_TEST_WORKBOOK` to a local `.xlsx` path or place a local workbook at `.local/test_workbook.xlsx`.

## Reports

The `reports/` directory contains Quarto templates for dashboard, summary, poster, and appendix outputs. These templates read analysis outputs produced by the pipeline rather than recomputing results on their own.

## Included Reference Material

- `docs/` contains supporting reference documents used during development

## Version Control Notes

- Generated outputs are ignored.
- Local workbooks and other sensitive inputs are ignored.
- Local R session files are ignored.
- Local assistant and planning artifacts are ignored.
