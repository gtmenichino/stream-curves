## Shared helpers for app-owned loading notifications.

analysis_launch_spinner_notification_id <- function(request_id) {
  resolved_request_id <- if (is.null(request_id) || length(request_id) == 0L) {
    ""
  } else {
    as.character(request_id)[1]
  }

  if (!nzchar(resolved_request_id)) {
    return("")
  }

  paste0("analysis-launch-spinner-", resolved_request_id)
}

final_loading_notification_ui <- function(title, detail, close_button = TRUE) {
  div(
    class = "streamcurves-final-loading-notification",
    div(
      class = "streamcurves-final-loading-header",
      div(
        class = "streamcurves-final-loading-heading",
        tags$span(class = "streamcurves-final-loading-spinner", `aria-hidden` = "true"),
        tags$span(class = "streamcurves-final-loading-title", title)
      ),
      if (isTRUE(close_button)) {
        tags$button(
          type = "button",
          class = "streamcurves-final-loading-close",
          `aria-label` = "Dismiss loading notification",
          onclick = "var notification = this.closest('.shiny-notification'); if (notification) { notification.remove(); } return false;",
          "x"
        )
      }
    ),
    div(
      class = "streamcurves-final-loading-detail",
      detail
    )
  )
}

show_analysis_launch_spinner_notification <- function(session, request_id, title, detail = "Loading page, please wait.") {
  notification_id <- analysis_launch_spinner_notification_id(request_id)

  if (!nzchar(notification_id)) {
    return(invisible(NULL))
  }

  show_final_loading_notification(
    session,
    notification_id,
    title,
    detail,
    close_button = TRUE
  )
}

show_final_loading_notification <- function(session, id, title, detail, close_button = TRUE) {
  notification_id <- if (is.null(id) || length(id) == 0L) "" else as.character(id)[1]

  if (is.null(session) || !nzchar(notification_id)) {
    return(invisible(NULL))
  }

  shiny::showNotification(
    ui = final_loading_notification_ui(title, detail, close_button = close_button),
    session = session,
    id = notification_id,
    duration = NULL,
    closeButton = FALSE,
    type = "message"
  )

  invisible(notification_id)
}

remove_analysis_launch_spinner_notification <- function(session, request_id) {
  notification_id <- analysis_launch_spinner_notification_id(request_id)

  if (!nzchar(notification_id)) {
    return(invisible(FALSE))
  }

  remove_final_loading_notification(session, notification_id)
}

remove_final_loading_notification <- function(session, id) {
  notification_id <- if (is.null(id) || length(id) == 0L) "" else as.character(id)[1]

  if (is.null(session) || !nzchar(notification_id)) {
    return(invisible(FALSE))
  }

  shiny::removeNotification(notification_id, session = session)
  invisible(TRUE)
}
