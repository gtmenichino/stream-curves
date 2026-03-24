## Regenerate the Posit Connect Cloud deployment manifest from the repo root.

command_args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", command_args, value = TRUE)

if (length(script_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
} else {
  project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
}

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop(
    paste(
      "Package 'rsconnect' is required.",
      "Install it with install.packages('rsconnect') and rerun this script."
    ),
    call. = FALSE
  )
}

setwd(project_root)

deploy_paths <- c(
  "app.R",
  "app",
  "R",
  "config",
  "data/raw/data_example.csv"
)

missing_paths <- deploy_paths[!file.exists(deploy_paths)]
if (length(missing_paths) > 0) {
  stop(
    paste("Missing deployment paths:", paste(missing_paths, collapse = ", ")),
    call. = FALSE
  )
}

expand_deploy_path <- function(path) {
  if (!dir.exists(path)) {
    return(path)
  }

  files <- list.files(
    path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE,
    include.dirs = FALSE,
    no.. = TRUE
  )

  files <- files[file.exists(files)]
  sub(paste0("^", normalizePath(path = project_root, winslash = "/", mustWork = TRUE), "/?"), "", normalizePath(files, winslash = "/", mustWork = TRUE))
}

deploy_files <- unique(unlist(lapply(deploy_paths, expand_deploy_path), use.names = FALSE))
deploy_files <- deploy_files[nzchar(deploy_files)]

deps <- tryCatch(
  rsconnect::appDependencies(
    appDir = project_root,
    appFiles = deploy_files,
    appMode = "shiny"
  ),
  error = function(e) {
    stop(
      paste(
        "Failed to discover app dependencies.",
        "Install the required R packages from README.md, then rerun this script.",
        "Original error:",
        conditionMessage(e)
      ),
      call. = FALSE
    )
  }
)

cat("Discovered", nrow(deps), "package dependencies.\n")
if ("Package" %in% names(deps)) {
  cat(paste(sort(unique(deps$Package)), collapse = ", "), "\n")
}

rsconnect::writeManifest(
  appDir = project_root,
  appFiles = deploy_files,
  appPrimaryDoc = "app.R",
  appMode = "shiny"
)

cat("Wrote manifest.json in", project_root, "\n")
