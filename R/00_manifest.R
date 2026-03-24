## ── 00: Run Manifest & QA ───────────────────────────────────────────────────
## Captures session info, config snapshots, and QA summary.

library(jsonlite)
library(cli)

#' Create run manifest
#'
#' @param output_dir Path to timestamped run output directory
#' @param config_dir Path to config directory
#' @param data_dir Path to data directory
#' @return Named list with manifest contents
create_run_manifest <- function(output_dir, config_dir, data_dir) {

  cli::cli_alert_info("Creating run manifest...")

  ## Git hash (if available)
  git_hash <- tryCatch(
    trimws(system("git rev-parse HEAD", intern = TRUE)),
    error = function(e) NA_character_,
    warning = function(w) NA_character_
  )

  ## File checksums
  data_files <- list.files(file.path(data_dir, "raw"), full.names = TRUE)
  checksums <- sapply(data_files, function(f) {
    tryCatch(tools::md5sum(f), error = function(e) NA_character_)
  })
  names(checksums) <- basename(names(checksums))

  ## Config files

  config_files <- list.files(config_dir, pattern = "\\.yaml$", full.names = TRUE)

  ## Package versions
  pkg_versions <- sapply(c("tidyverse", "readr", "dplyr", "ggplot2", "ggpubr",
                            "leaps", "lmtest", "car", "MASS", "yaml",
                            "jsonlite", "cli", "patchwork", "broom",
                            "performance", "see", "forcats", "purrr",
                            "tibble", "tidyr", "stringr", "readxl"),
    function(pkg) {
      tryCatch(as.character(packageVersion(pkg)), error = function(e) "not_installed")
    }
  )

  manifest <- list(
    run_id      = basename(output_dir),
    timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    r_version   = paste(R.version$major, R.version$minor, sep = "."),
    platform    = R.version$platform,
    git_hash    = git_hash,
    packages    = as.list(pkg_versions),
    data_checksums = as.list(checksums),
    config_files   = basename(config_files),
    output_dir     = output_dir
  )

  manifest
}


#' Save manifest and related files
#'
#' @param manifest List from create_run_manifest()
#' @param output_dir Path to timestamped run output directory
#' @return invisible(NULL)
save_manifest <- function(manifest, output_dir) {

  manifest_dir <- file.path(output_dir, "manifest")
  dir.create(manifest_dir, recursive = TRUE, showWarnings = FALSE)

  ## Save manifest JSON
  jsonlite::write_json(
    manifest,
    path = file.path(manifest_dir, "run_manifest.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  cli::cli_alert_success("Saved {.file run_manifest.json}")

  ## Save session info
  session_text <- utils::capture.output(utils::sessionInfo())
  writeLines(session_text, file.path(manifest_dir, "session_info.txt"))
  cli::cli_alert_success("Saved {.file session_info.txt}")

  ## Copy config files to snapshot
  config_snapshot_dir <- file.path(manifest_dir, "config_snapshot")
  dir.create(config_snapshot_dir, recursive = TRUE, showWarnings = FALSE)

  config_files <- list.files("config", pattern = "\\.yaml$", full.names = TRUE)
  for (cf in config_files) {
    file.copy(cf, file.path(config_snapshot_dir, basename(cf)), overwrite = TRUE)
  }
  cli::cli_alert_success("Config snapshot saved ({length(config_files)} files)")

  invisible(NULL)
}
