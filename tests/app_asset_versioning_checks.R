app_lines <- readLines("app/app.R", warn = FALSE)
app_text <- paste(app_lines, collapse = "\n")

stopifnot(any(grepl("versioned_www_asset <- function\\(asset_name\\)", app_lines)))
stopifnot(any(grepl('tags\\$link\\(rel = "stylesheet", href = versioned_www_asset\\("custom\\.css"\\)\\)', app_lines)))
stopifnot(any(grepl('tags\\$script\\(src = versioned_www_asset\\("custom\\.js"\\)\\)', app_lines)))
stopifnot(!grepl('href = "custom\\.css"', app_text))
stopifnot(!grepl('src = "custom\\.js"', app_text))
stopifnot(grepl('format\\(info\\$mtime, "%Y%m%d%H%M%S"\\)', app_text))

cat("app_asset_versioning_checks: OK\n")
