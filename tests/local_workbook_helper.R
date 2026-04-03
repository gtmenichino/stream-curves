suppressPackageStartupMessages({})

streamcurves_test_root <- function(start = getwd()) {
  root <- normalizePath(start, winslash = "/", mustWork = TRUE)
  if (identical(basename(root), "app")) {
    return(dirname(root))
  }
  root
}

resolve_streamcurves_test_workbook <- function(project_root = streamcurves_test_root()) {
  env_path <- Sys.getenv("STREAMCURVES_TEST_WORKBOOK", "")
  env_candidates <- character(0)

  if (nzchar(env_path)) {
    env_candidates <- c(
      env_path,
      file.path(project_root, env_path)
    )
  }

  candidates <- c(
    env_candidates,
    file.path(project_root, ".local", "test_workbook.xlsx")
  )

  existing <- unique(candidates[nzchar(candidates) & file.exists(candidates)])
  if (length(existing) == 0) {
    return(NULL)
  }

  normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
}

require_streamcurves_test_workbook <- function(test_name, project_root = streamcurves_test_root()) {
  workbook_path <- resolve_streamcurves_test_workbook(project_root = project_root)

  if (is.null(workbook_path)) {
    cat(
      sprintf(
        "%s: SKIPPED (set STREAMCURVES_TEST_WORKBOOK or place a local workbook at .local/test_workbook.xlsx)\n",
        test_name
      )
    )
    quit(save = "no", status = 0L)
  }

  workbook_path
}
