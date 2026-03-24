## StreamCurves deployment entrypoint for Posit Connect Cloud.
## Local development entrypoint remains app/app.R.

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Package 'shiny' is required to launch StreamCurves.", call. = FALSE)
}

shiny::shinyAppDir(appDir = "app")
