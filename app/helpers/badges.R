## ── Status Badge Helpers ──────────────────────────────────────────────────────
## Consistent pass/caution/fail/not_applicable badges throughout the app.

library(shiny)

#' Create a status badge span
#' @param status Character: "pass", "caution", "fail", "not_applicable"
#' @param label Optional override label
#' @return HTML span element
status_badge <- function(status, label = NULL) {
  badge_class <- switch(status,
    pass           = "bg-success",
    caution        = "bg-warning text-dark",
    fail           = "bg-danger",
    not_applicable = "bg-secondary",
    "bg-secondary"
  )
  label <- label %||% status
  tags$span(class = paste("badge", badge_class), label)
}

#' Create a p-value badge
#' @param p Numeric p-value
#' @return HTML span element
p_value_badge <- function(p) {

  if (is.na(p)) return(status_badge("not_applicable", "NA"))
  if (p < 0.01)  return(status_badge("pass", sprintf("p = %.4f", p)))
  if (p < 0.05)  return(status_badge("pass", sprintf("p = %.3f", p)))
  if (p < 0.10)  return(status_badge("caution", sprintf("p = %.3f", p)))
  status_badge("not_applicable", sprintf("p = %.3f", p))
}

#' Create an explanation card
#' @param title Card title
#' @param ... Card body content
#' @return bslib card element
explanation_card <- function(title, ...) {
  bslib::card(
    class = "border-info mb-2",
    fill = FALSE,
    bslib::card_header(class = "bg-info text-white", title),
    bslib::card_body(...)
  )
}

#' Create a centered "No Data Loaded" alert
#' @return HTML div element
no_data_alert <- function() {
  div(
    class = "d-flex justify-content-center align-items-center",
    style = "min-height: 300px;",
    div(
      class = "text-center text-muted",
      icon("database", class = "fa-3x mb-3"),
      tags$h4("No Data Loaded"),
      p("Go to the ", tags$strong("Data & Setup"), " tab and click ",
        tags$strong("Load Data"), " to get started.")
    )
  )
}
