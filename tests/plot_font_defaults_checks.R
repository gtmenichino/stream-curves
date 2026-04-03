helper_lines <- readLines("R/00_plot_theme.R", warn = FALSE)
helper_text <- paste(helper_lines, collapse = "\n")
global_lines <- readLines("app/global.R", warn = FALSE)
global_text <- paste(global_lines, collapse = "\n")

stopifnot(any(grepl("streamcurves_plot_font_profiles <- list\\(", helper_lines)))
stopifnot(any(grepl("default = list\\(", helper_lines)))
stopifnot(any(grepl("large_analysis = list\\(", helper_lines)))
stopifnot(any(grepl("base = 12", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("axis_text = 12", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("legend_text = 12", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("annotation = 12", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("base = 14", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("subtitle = 14", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("axis_title = 14", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("legend_title = 14", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("annotation = 14", helper_lines, fixed = TRUE)))
stopifnot(any(grepl("streamcurves_get_plot_font_profile <- function\\(", helper_lines)))
stopifnot(any(grepl("streamcurves_geom_text_size <- function\\(", helper_lines)))
stopifnot(any(grepl("streamcurves_plot_text_theme <- function\\(", helper_lines)))
stopifnot(any(grepl("streamcurves_minimal_plot_theme <- function\\(", helper_lines)))
stopifnot(any(grepl("streamcurves_plotly_axis_defaults <- function\\(", helper_lines)))
stopifnot(any(grepl("streamcurves_plotly_legend_defaults <- function\\(", helper_lines)))
stopifnot(any(grepl("streamcurves_plotly_layout <- function\\(", helper_lines)))
stopifnot(grepl('source\\(file\\.path\\(r_dir, "00_plot_theme\\.R"\\), local = TRUE\\)', global_text))

helper_expectations <- list(
  "R/05_stratification_screening.R" = "streamcurves_plot_text_theme\\(",
  "R/05c_pattern_stability.R" = "streamcurves_minimal_plot_theme\\(",
  "R/05d_cross_metric_consistency.R" = "streamcurves_minimal_plot_theme\\(",
  "R/07_model_candidates.R" = "streamcurves_minimal_plot_theme\\(",
  "R/09_diagnostics.R" = "streamcurves_minimal_plot_theme\\(",
  "R/10_reference_curves.R" = "streamcurves_minimal_plot_theme\\(",
  "R/11_cross_metric.R" = "streamcurves_minimal_plot_theme\\(",
  "R/12_regional_curves.R" = "streamcurves_plot_text_theme\\(",
  "app/modules/mod_precheck.R" = "streamcurves_minimal_plot_theme\\(",
  "app/modules/mod_phase1_exploration.R" = "streamcurves_plotly_layout\\("
)

for (path in names(helper_expectations)) {
  file_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  stopifnot(grepl(helper_expectations[[path]], file_text))
}

minimal_theme_files <- c(
  "R/05c_pattern_stability.R",
  "R/05d_cross_metric_consistency.R",
  "R/07_model_candidates.R",
  "R/09_diagnostics.R",
  "R/10_reference_curves.R",
  "R/11_cross_metric.R",
  "R/12_regional_curves.R",
  "app/modules/mod_precheck.R",
  "app/modules/mod_phase1_exploration.R"
)

for (path in minimal_theme_files) {
  file_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  stopifnot(!grepl("theme_minimal\\(", file_text))
}

screening_text <- paste(readLines("R/05_stratification_screening.R", warn = FALSE), collapse = "\n")
stopifnot(grepl("font_profile = \"default\"", screening_text, fixed = TRUE))
stopifnot(grepl("streamcurves_geom_text_size\\(font_profile\\)", screening_text))

phase1_text <- paste(readLines("app/modules/mod_phase1_exploration.R", warn = FALSE), collapse = "\n")
stopifnot(grepl("streamcurves_plotly_axis_defaults\\(", phase1_text))
stopifnot(grepl("streamcurves_plotly_legend_defaults\\(", phase1_text))
stopifnot(grepl("profile = \"large_analysis\"", phase1_text, fixed = TRUE))
stopifnot(grepl("font_profile = \"large_analysis\"", phase1_text, fixed = TRUE))
stopifnot(!grepl("theme_minimal\\(base_size = 10\\)", phase1_text, fixed = TRUE))
stopifnot(!grepl("plotly::layout\\(", phase1_text, fixed = TRUE))

consistency_text <- paste(readLines("R/05d_cross_metric_consistency.R", warn = FALSE), collapse = "\n")
cross_metric_text <- paste(readLines("R/11_cross_metric.R", warn = FALSE), collapse = "\n")
stopifnot(!grepl("axis.text.x = .*size = 9", consistency_text))
stopifnot(!grepl("axis.text.y = .*size = 9", consistency_text))
stopifnot(!grepl("axis.text.x = .*size = 7", cross_metric_text))
stopifnot(!grepl("axis.text.y = .*size = 7", cross_metric_text))

pattern_text <- paste(readLines("R/05c_pattern_stability.R", warn = FALSE), collapse = "\n")
stopifnot(grepl("streamcurves_minimal_plot_theme\\(profile = \"large_analysis\"\\)", pattern_text))

reference_curve_text <- paste(readLines("R/10_reference_curves.R", warn = FALSE), collapse = "\n")
stopifnot(grepl("streamcurves_minimal_plot_theme\\(profile = \"large_analysis\"", reference_curve_text))
stopifnot(grepl("streamcurves_geom_text_size\\(\"large_analysis\"\\)", reference_curve_text))

phase3_text <- paste(readLines("app/modules/mod_phase3_verification.R", warn = FALSE), collapse = "\n")
stopifnot(grepl("phase1_state <- get_metric_phase1_display_state\\(rv, rv\\$current_metric\\)", phase3_text))
stopifnot(grepl("phase1_state\\$plot_specs\\[\\[local_sk\\]\\]", phase3_text))
stopifnot(grepl("build_screening_plot_from_spec\\(", phase3_text))
stopifnot(grepl("font_profile = \"large_analysis\"", phase3_text, fixed = TRUE))
stopifnot(!grepl("rv\\$phase1_screening\\$plots\\[\\[local_sk\\]\\]", phase3_text, fixed = TRUE))

cat("plot_font_defaults_checks: OK\n")
