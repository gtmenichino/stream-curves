## Shared plot typography defaults for active app plots and plot exports.

streamcurves_plot_font_profiles <- list(
  default = list(
    base = 12,
    title = 14,
    subtitle = 12,
    axis_text = 12,
    axis_title = 12,
    legend_text = 12,
    legend_title = 12,
    strip_text = 12,
    annotation = 12
  ),
  large_analysis = list(
    base = 14,
    title = 14,
    subtitle = 14,
    axis_text = 14,
    axis_title = 14,
    legend_text = 14,
    legend_title = 14,
    strip_text = 14,
    annotation = 14
  )
)

streamcurves_get_plot_font_profile <- function(profile = "default") {
  resolved_profile <- profile %||% "default"
  sizes <- streamcurves_plot_font_profiles[[resolved_profile]]
  if (is.null(sizes)) {
    stop(sprintf("Unknown plot font profile: %s", resolved_profile), call. = FALSE)
  }

  sizes
}

streamcurves_geom_text_size <- function(profile = "default", field = "annotation") {
  sizes <- streamcurves_get_plot_font_profile(profile)
  pt_value <- sizes[[field]] %||% sizes$annotation %||% sizes$base
  pt_value / ggplot2::.pt
}

streamcurves_plot_text_theme <- function(profile = "default",
                                         legend_position = NULL,
                                         axis_text_x_angle = NULL,
                                         axis_text_x_hjust = NULL,
                                         axis_text_x_vjust = NULL,
                                         axis_text_x_blank = FALSE,
                                         axis_text_y_blank = FALSE,
                                         axis_ticks_x_blank = FALSE,
                                         axis_ticks_y_blank = FALSE,
                                         panel_grid_blank = FALSE) {
  sizes <- streamcurves_get_plot_font_profile(profile)
  theme_args <- list(
    plot.title = ggplot2::element_text(size = sizes$title),
    plot.subtitle = ggplot2::element_text(size = sizes$subtitle),
    axis.title.x = ggplot2::element_text(size = sizes$axis_title),
    axis.title.y = ggplot2::element_text(size = sizes$axis_title),
    axis.text.x = ggplot2::element_text(size = sizes$axis_text),
    axis.text.y = ggplot2::element_text(size = sizes$axis_text),
    legend.text = ggplot2::element_text(size = sizes$legend_text),
    legend.title = ggplot2::element_text(size = sizes$legend_title),
    strip.text = ggplot2::element_text(size = sizes$strip_text)
  )

  if (!is.null(legend_position)) {
    theme_args$legend.position <- legend_position
  }

  if (!is.null(axis_text_x_angle) ||
      !is.null(axis_text_x_hjust) ||
      !is.null(axis_text_x_vjust)) {
    axis_text_x_args <- list(size = sizes$axis_text)
    if (!is.null(axis_text_x_angle)) axis_text_x_args$angle <- axis_text_x_angle
    if (!is.null(axis_text_x_hjust)) axis_text_x_args$hjust <- axis_text_x_hjust
    if (!is.null(axis_text_x_vjust)) axis_text_x_args$vjust <- axis_text_x_vjust
    theme_args$axis.text.x <- do.call(ggplot2::element_text, axis_text_x_args)
  }

  if (isTRUE(axis_text_x_blank)) {
    theme_args$axis.text.x <- ggplot2::element_blank()
  }
  if (isTRUE(axis_text_y_blank)) {
    theme_args$axis.text.y <- ggplot2::element_blank()
  }
  if (isTRUE(axis_ticks_x_blank)) {
    theme_args$axis.ticks.x <- ggplot2::element_blank()
  }
  if (isTRUE(axis_ticks_y_blank)) {
    theme_args$axis.ticks.y <- ggplot2::element_blank()
  }
  if (isTRUE(panel_grid_blank)) {
    theme_args$panel.grid <- ggplot2::element_blank()
  }

  do.call(ggplot2::theme, theme_args)
}

streamcurves_minimal_plot_theme <- function(profile = "default",
                                            base_size = NULL,
                                            ...) {
  sizes <- streamcurves_get_plot_font_profile(profile)
  resolved_base_size <- base_size %||% sizes$base

  ggplot2::theme_minimal(base_size = resolved_base_size) +
    streamcurves_plot_text_theme(profile = profile, ...)
}

streamcurves_plotly_axis_defaults <- function(profile = "default",
                                              title_text = NULL,
                                              visible = TRUE,
                                              standoff = NULL,
                                              zeroline = FALSE,
                                              ...) {
  sizes <- streamcurves_get_plot_font_profile(profile)
  axis <- utils::modifyList(
    list(
      visible = visible,
      automargin = TRUE,
      zeroline = zeroline,
      tickfont = list(size = sizes$axis_text)
    ),
    list(...)
  )

  if (isTRUE(visible)) {
    title_defaults <- list(font = list(size = sizes$axis_title))
    if (!is.null(title_text)) {
      title_defaults$text <- title_text
    }
    if (!is.null(standoff)) {
      title_defaults$standoff <- standoff
    }
    axis$title <- utils::modifyList(title_defaults, axis$title %||% list())
  }

  axis
}

streamcurves_plotly_legend_defaults <- function(profile = "default", ...) {
  sizes <- streamcurves_get_plot_font_profile(profile)
  utils::modifyList(
    list(
      font = list(size = sizes$legend_text),
      title = list(font = list(size = sizes$legend_title))
    ),
    list(...)
  )
}

streamcurves_plotly_layout <- function(fig,
                                       profile = "default",
                                       xaxis = NULL,
                                       yaxis = NULL,
                                       legend = NULL,
                                       annotations = NULL,
                                       margin = NULL,
                                       ...) {
  sizes <- streamcurves_get_plot_font_profile(profile)
  layout_args <- c(
    list(
      fig,
      font = list(size = sizes$base)
    ),
    list(...)
  )

  if (!is.null(xaxis)) layout_args$xaxis <- xaxis
  if (!is.null(yaxis)) layout_args$yaxis <- yaxis
  if (!is.null(legend)) layout_args$legend <- legend
  if (!is.null(annotations)) layout_args$annotations <- annotations
  if (!is.null(margin)) layout_args$margin <- margin

  do.call(plotly::layout, layout_args)
}
