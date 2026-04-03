## -- 10: Reference Curve Development -------------------------------------------
## Reference curve helpers for Phase 4, including manual point editing support.

library(dplyr)
library(ggplot2)
library(cli)

empty_reference_curve_points <- function() {
  tibble::tibble(
    point_order = integer(),
    metric_value = numeric(),
    index_score = numeric()
  )
}

normalize_reference_curve_points <- function(curve_points) {
  if (is.null(curve_points)) {
    return(empty_reference_curve_points())
  }

  if (inherits(curve_points, "tbl_df") || is.data.frame(curve_points)) {
    points <- tibble::as_tibble(curve_points)
  } else if (is.list(curve_points)) {
    points <- tibble::as_tibble(curve_points)
  } else {
    stop("Curve points must be NULL, a data frame, or a list.", call. = FALSE)
  }

  if (!("metric_value" %in% names(points)) && "x" %in% names(points)) {
    points$metric_value <- points$x
  }
  if (!("index_score" %in% names(points)) && "y" %in% names(points)) {
    points$index_score <- points$y
  }

  if (!("metric_value" %in% names(points)) || !("index_score" %in% names(points))) {
    return(empty_reference_curve_points())
  }

  if (!("point_order" %in% names(points))) {
    points$point_order <- seq_len(nrow(points))
  }

  original_order <- seq_len(nrow(points))
  point_order <- suppressWarnings(as.integer(points$point_order))
  point_order[is.na(point_order)] <- original_order[is.na(point_order)]

  points <- tibble::tibble(
    point_order = point_order,
    metric_value = suppressWarnings(as.numeric(points$metric_value)),
    index_score = suppressWarnings(as.numeric(points$index_score)),
    original_order = original_order
  )

  points <- points |>
    dplyr::filter(!(is.na(metric_value) & is.na(index_score))) |>
    dplyr::arrange(point_order, original_order) |>
    dplyr::mutate(point_order = dplyr::row_number()) |>
    dplyr::select(point_order, metric_value, index_score)

  points
}

empty_reference_curve_intervals <- function() {
  tibble::tibble(
    min = numeric(),
    max = numeric()
  )
}

reference_curve_format_number <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("N/A")
  }

  format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE)
}

reference_curve_unique_numeric <- function(values, digits = 10) {
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    return(numeric())
  }

  rounded <- round(as.numeric(values), digits)
  rounded[!duplicated(rounded)]
}

reference_curve_as_interval_tbl <- function(intervals) {
  if (is.null(intervals)) {
    return(empty_reference_curve_intervals())
  }

  if (inherits(intervals, "tbl_df") || is.data.frame(intervals)) {
    tbl <- tibble::as_tibble(intervals)
  } else if (is.list(intervals)) {
    tbl <- tibble::as_tibble(intervals)
  } else {
    return(empty_reference_curve_intervals())
  }

  if (!all(c("min", "max") %in% names(tbl))) {
    return(empty_reference_curve_intervals())
  }

  tbl <- tibble::tibble(
    min = suppressWarnings(as.numeric(tbl$min)),
    max = suppressWarnings(as.numeric(tbl$max))
  ) |>
    dplyr::filter(is.finite(min), is.finite(max)) |>
    dplyr::mutate(
      lower = pmin(min, max),
      upper = pmax(min, max)
    ) |>
    dplyr::transmute(min = lower, max = upper)

  if (nrow(tbl) == 0) {
    return(empty_reference_curve_intervals())
  }

  tbl
}

reference_curve_merge_intervals <- function(intervals, tol = 1e-9) {
  intervals <- reference_curve_as_interval_tbl(intervals)
  if (nrow(intervals) == 0) {
    return(empty_reference_curve_intervals())
  }

  intervals <- intervals |>
    dplyr::arrange(min, max)

  merged <- vector("list", nrow(intervals))
  out_idx <- 1L
  current_min <- intervals$min[1]
  current_max <- intervals$max[1]

  if (nrow(intervals) > 1) {
    for (i in 2:nrow(intervals)) {
      next_min <- intervals$min[i]
      next_max <- intervals$max[i]

      if (next_min <= (current_max + tol)) {
        current_max <- max(current_max, next_max)
      } else {
        merged[[out_idx]] <- tibble::tibble(min = current_min, max = current_max)
        out_idx <- out_idx + 1L
        current_min <- next_min
        current_max <- next_max
      }
    }
  }

  merged[[out_idx]] <- tibble::tibble(min = current_min, max = current_max)
  dplyr::bind_rows(merged[seq_len(out_idx)])
}

reference_curve_score_band_for_value <- function(index_score, tol = 1e-9) {
  if (!is.finite(index_score)) {
    return(NA_character_)
  }

  if (index_score >= (0.70 - tol)) {
    return("functioning")
  }

  if (index_score >= (0.30 - tol)) {
    return("at_risk")
  }

  "not_functioning"
}

reference_curve_segment_breaks <- function(x1, y1, x2, y2,
                                           thresholds = c(0.30, 0.70),
                                           tol = 1e-9) {
  breaks <- c(x1, x2)

  if (abs(y2 - y1) > tol) {
    lo <- min(y1, y2)
    hi <- max(y1, y2)

    for (target in thresholds) {
      if ((target + tol) < lo || (target - tol) > hi) {
        next
      }

      x_val <- x1 + (target - y1) * (x2 - x1) / (y2 - y1)
      if (is.finite(x_val)) {
        breaks <- c(breaks, x_val)
      }
    }
  }

  sort(reference_curve_unique_numeric(breaks))
}

reference_curve_band_intervals <- function(points, tol = 1e-9) {
  points <- normalize_reference_curve_points(points)
  if (nrow(points) < 2) {
    return(list(
      functioning = empty_reference_curve_intervals(),
      at_risk = empty_reference_curve_intervals(),
      not_functioning = empty_reference_curve_intervals()
    ))
  }

  intervals <- list(
    functioning = empty_reference_curve_intervals(),
    at_risk = empty_reference_curve_intervals(),
    not_functioning = empty_reference_curve_intervals()
  )

  add_interval <- function(bucket, x_min, x_max) {
    if (!is.finite(x_min) || !is.finite(x_max)) {
      return(invisible(NULL))
    }

    intervals[[bucket]] <<- dplyr::bind_rows(
      intervals[[bucket]],
      tibble::tibble(
        min = min(x_min, x_max),
        max = max(x_min, x_max)
      )
    )
    invisible(NULL)
  }

  for (i in seq_len(nrow(points) - 1L)) {
    x1 <- points$metric_value[i]
    y1 <- points$index_score[i]
    x2 <- points$metric_value[i + 1L]
    y2 <- points$index_score[i + 1L]

    if (!all(is.finite(c(x1, y1, x2, y2)))) {
      next
    }

    if (abs(x2 - x1) <= tol) {
      y_low <- min(y1, y2)
      y_high <- max(y1, y2)

      if (y_high >= (0.70 - tol)) {
        add_interval("functioning", x1, x1)
      }
      if (y_low < (0.70 - tol) && y_high >= (0.30 - tol)) {
        add_interval("at_risk", x1, x1)
      }
      if (y_low < (0.30 - tol)) {
        add_interval("not_functioning", x1, x1)
      }
      next
    }

    breaks <- reference_curve_segment_breaks(x1, y1, x2, y2, tol = tol)
    if (length(breaks) < 2) {
      next
    }

    for (j in seq_len(length(breaks) - 1L)) {
      x_min <- breaks[j]
      x_max <- breaks[j + 1L]
      if (x_max < (x_min - tol)) {
        next
      }

      mid_x <- if (abs(x_max - x_min) <= tol) x_min else (x_min + x_max) / 2
      mid_y <- y1 + (mid_x - x1) * (y2 - y1) / (x2 - x1)
      bucket <- reference_curve_score_band_for_value(mid_y, tol = tol)

      if (!is.na(bucket)) {
        add_interval(bucket, x_min, x_max)
      }
    }
  }

  lapply(intervals, reference_curve_merge_intervals, tol = tol)
}

reference_curve_score_relation <- function(index_score, target_score, tol = 1e-9) {
  if (is.na(index_score)) {
    return(NA_integer_)
  }

  if (index_score < (target_score - tol)) {
    return(-1L)
  }

  if (index_score > (target_score + tol)) {
    return(1L)
  }

  0L
}

reference_curve_threshold_crossings <- function(points, target_score, tol = 1e-9) {
  points <- normalize_reference_curve_points(points)
  if (nrow(points) < 2) {
    return(numeric())
  }

  relations <- vapply(
    points$index_score,
    reference_curve_score_relation,
    integer(1),
    target_score = target_score,
    tol = tol
  )
  candidates <- numeric()

  for (i in seq_len(nrow(points) - 1L)) {
    rel_1 <- relations[i]
    rel_2 <- relations[i + 1L]
    if (is.na(rel_1) || is.na(rel_2) || rel_1 == 0L || rel_2 == 0L || rel_1 == rel_2) {
      next
    }

    x1 <- points$metric_value[i]
    x2 <- points$metric_value[i + 1L]
    y1 <- points$index_score[i]
    y2 <- points$index_score[i + 1L]

    if (!all(is.finite(c(x1, x2, y1, y2)))) {
      next
    }

    if (abs(x2 - x1) <= tol) {
      candidates <- c(candidates, x1)
    } else {
      x_val <- x1 + (target_score - y1) * (x2 - x1) / (y2 - y1)
      if (is.finite(x_val)) {
        candidates <- c(candidates, x_val)
      }
    }
  }

  idx <- 1L
  while (idx <= nrow(points)) {
    if (!identical(relations[idx], 0L)) {
      idx <- idx + 1L
      next
    }

    run_start <- idx
    while (idx <= nrow(points) && identical(relations[idx], 0L)) {
      idx <- idx + 1L
    }
    run_end <- idx - 1L

    left_candidates <- if (run_start > 1L) which(relations[seq_len(run_start - 1L)] != 0L) else integer(0)
    right_candidates <- if (idx <= nrow(points)) which(relations[idx:nrow(points)] != 0L) else integer(0)

    if (length(left_candidates) == 0 || length(right_candidates) == 0) {
      next
    }

    left_rel <- relations[left_candidates[[length(left_candidates)]]]
    right_rel <- relations[idx + right_candidates[[1]] - 1L]
    if (left_rel == right_rel) {
      next
    }

    if (left_rel < right_rel) {
      candidates <- c(candidates, points$metric_value[run_start])
    } else {
      candidates <- c(candidates, points$metric_value[run_end])
    }
  }

  sort(reference_curve_unique_numeric(candidates))
}

reference_curve_crossings_text <- function(crossings, digits = 2) {
  crossings <- reference_curve_unique_numeric(crossings)
  if (length(crossings) == 0) {
    return("N/A")
  }

  paste(
    vapply(crossings, reference_curve_format_number, character(1), digits = digits),
    collapse = ", "
  )
}

reference_curve_interval_ranges_text <- function(ranges, digits = 2, tol = 1e-9) {
  ranges <- reference_curve_merge_intervals(ranges, tol = tol)
  if (nrow(ranges) == 0) {
    return("N/A")
  }

  paste(
    vapply(seq_len(nrow(ranges)), function(i) {
      x_min <- ranges$min[i]
      x_max <- ranges$max[i]
      if (abs(x_max - x_min) <= tol) {
        return(reference_curve_format_number(x_min, digits = digits))
      }

      paste0(
        reference_curve_format_number(x_min, digits = digits),
        " - ",
        reference_curve_format_number(x_max, digits = digits)
      )
    }, character(1)),
    collapse = ", "
  )
}

reference_curve_row_range_display <- function(curve_row, range_name, digits = 2) {
  if (is.null(curve_row) || nrow(curve_row) == 0) {
    return("N/A")
  }

  row <- tibble::as_tibble(curve_row)[1, , drop = FALSE]
  display_field <- paste0(range_name, "_ranges_display")
  list_field <- paste0(range_name, "_ranges")
  min_field <- paste0(range_name, "_min")
  max_field <- paste0(range_name, "_max")

  if (display_field %in% names(row)) {
    display_raw <- row[[display_field]][1]
    display_value <- if (length(display_raw) == 0 || is.null(display_raw) || is.na(display_raw)) {
      ""
    } else {
      as.character(display_raw)
    }
    if (nzchar(display_value) && !identical(display_value, "NA")) {
      return(display_value)
    }
  }

  if (list_field %in% names(row)) {
    return(reference_curve_interval_ranges_text(row[[list_field]][[1]], digits = digits))
  }

  if (all(c(min_field, max_field) %in% names(row))) {
    return(reference_curve_interval_ranges_text(
      tibble::tibble(
        min = suppressWarnings(as.numeric(row[[min_field]][1])),
        max = suppressWarnings(as.numeric(row[[max_field]][1]))
      ),
      digits = digits
    ))
  }

  "N/A"
}

reference_curve_points_from_row <- function(curve_row, higher_is_better = NULL) {
  if (is.null(curve_row) || nrow(curve_row) == 0) {
    return(empty_reference_curve_points())
  }

  row <- tibble::as_tibble(curve_row)[1, , drop = FALSE]

  if ("curve_points" %in% names(row)) {
    stored_points <- row$curve_points[[1]]
    normalized <- normalize_reference_curve_points(stored_points)
    if (nrow(normalized) >= 2) {
      return(normalized)
    }
  }

  point_x_cols <- grep("^curve_point[0-9]+_x$", names(row), value = TRUE)
  if (length(point_x_cols) == 0) {
    return(empty_reference_curve_points())
  }

  point_ids <- sub("^curve_point([0-9]+)_x$", "\\1", point_x_cols)
  point_ids <- point_ids[order(as.integer(point_ids))]

  points <- purrr::map_dfr(point_ids, function(idx) {
    x_col <- paste0("curve_point", idx, "_x")
    y_col <- paste0("curve_point", idx, "_y")
    if (!(x_col %in% names(row)) || !(y_col %in% names(row))) {
      return(NULL)
    }

    x_val <- suppressWarnings(as.numeric(row[[x_col]][1]))
    y_val <- suppressWarnings(as.numeric(row[[y_col]][1]))
    if (is.na(x_val) || is.na(y_val)) {
      return(NULL)
    }

    tibble::tibble(
      point_order = as.integer(idx),
      metric_value = x_val,
      index_score = y_val
    )
  })

  normalize_reference_curve_points(points)
}

reference_curve_metric_at_score <- function(points, target_score, prefer = c("left", "right")) {
  prefer <- match.arg(prefer)
  points <- normalize_reference_curve_points(points)
  if (nrow(points) < 2) {
    return(NA_real_)
  }

  tol <- 1e-9
  candidates <- numeric()

  for (i in seq_len(nrow(points) - 1L)) {
    x1 <- points$metric_value[i]
    y1 <- points$index_score[i]
    x2 <- points$metric_value[i + 1L]
    y2 <- points$index_score[i + 1L]

    lo <- min(y1, y2)
    hi <- max(y1, y2)
    if ((target_score + tol) < lo || (target_score - tol) > hi) {
      next
    }

    if (abs(y2 - y1) <= tol) {
      if (abs(y1 - target_score) <= tol) {
        candidates <- c(candidates, if (identical(prefer, "left")) min(x1, x2) else max(x1, x2))
      }
      next
    }

    x_val <- x1 + (target_score - y1) * (x2 - x1) / (y2 - y1)
    if (is.finite(x_val)) {
      candidates <- c(candidates, x_val)
    }
  }

  if (length(candidates) == 0) {
    return(NA_real_)
  }

  candidates <- unique(round(candidates, 10))
  if (identical(prefer, "left")) min(candidates) else max(candidates)
}

validate_reference_curve_points <- function(points, higher_is_better) {
  points <- normalize_reference_curve_points(points)
  errors <- character(0)
  tol <- 1e-9

  if (nrow(points) < 2) {
    errors <- c(errors, "At least 2 curve points are required.")
  }

  if (anyNA(points$metric_value) || anyNA(points$index_score)) {
    errors <- c(errors, "Metric score and index score must be numeric for every point.")
  }

  if (length(errors) == 0) {
    metric_diffs <- diff(points$metric_value)
    if (any(metric_diffs < -tol, na.rm = TRUE)) {
      errors <- c(
        errors,
        "Metric score must be non-decreasing from top to bottom. Equal consecutive values are allowed."
      )
    }
  }

  if (any(points$index_score < -tol | points$index_score > (1 + tol), na.rm = TRUE)) {
    errors <- c(errors, "Index score values must be between 0 and 1.")
  }

  if (length(errors) == 0) {
    score_30_crossings <- reference_curve_threshold_crossings(points, 0.30, tol = tol)
    score_70_crossings <- reference_curve_threshold_crossings(points, 0.70, tol = tol)

    if (length(score_30_crossings) > 2) {
      errors <- c(errors, "Manual curves can cross index score 0.30 at most twice.")
    }
    if (length(score_70_crossings) > 2) {
      errors <- c(errors, "Manual curves can cross index score 0.70 at most twice.")
    }
  }

  if (length(errors) == 0) {
    score_min <- min(points$index_score, na.rm = TRUE)
    score_max <- max(points$index_score, na.rm = TRUE)
    if (score_min > (0.30 + tol) || score_max < (0.70 - tol)) {
      errors <- c(errors, "Curve points must span index scores 0.30 and 0.70.")
    }
  }

  list(
    valid = length(errors) == 0,
    errors = errors,
    points = points
  )
}

reference_curve_summary_stats <- function(ref_values) {
  ref_values <- ref_values[is.finite(ref_values)]
  if (length(ref_values) == 0) {
    return(list(
      n_reference = 0L,
      median_val = NA_real_,
      mean_val = NA_real_,
      sd_val = NA_real_,
      min_val = NA_real_,
      max_val = NA_real_,
      q25 = NA_real_,
      q75 = NA_real_,
      iqr = NA_real_
    ))
  }

  q25 <- as.numeric(stats::quantile(ref_values, 0.25, na.rm = TRUE, names = FALSE))
  q75 <- as.numeric(stats::quantile(ref_values, 0.75, na.rm = TRUE, names = FALSE))

  list(
    n_reference = length(ref_values),
    median_val = as.numeric(stats::median(ref_values, na.rm = TRUE)),
    mean_val = as.numeric(mean(ref_values, na.rm = TRUE)),
    sd_val = as.numeric(stats::sd(ref_values, na.rm = TRUE)),
    min_val = as.numeric(min(ref_values, na.rm = TRUE)),
    max_val = as.numeric(max(ref_values, na.rm = TRUE)),
    q25 = q25,
    q75 = q75,
    iqr = as.numeric(q75 - q25)
  )
}

build_reference_curve_row <- function(metric_key, metric_config, stats, curve_points,
                                      curve_source = "auto", stratum_label = NULL,
                                      curve_status = "complete") {
  mc <- metric_config[[metric_key]]
  higher_is_better <- isTRUE(mc$higher_is_better)
  points <- normalize_reference_curve_points(curve_points)
  points_min <- if (nrow(points) > 0) min(points$metric_value, na.rm = TRUE) else NA_real_
  points_max <- if (nrow(points) > 0) max(points$metric_value, na.rm = TRUE) else NA_real_
  band_ranges <- reference_curve_band_intervals(points)

  prefer_threshold <- if (higher_is_better) "left" else "right"
  score_30_metric <- reference_curve_metric_at_score(points, 0.30, prefer = prefer_threshold)
  score_70_metric <- reference_curve_metric_at_score(points, 0.70, prefer = prefer_threshold)
  score_100_metric <- reference_curve_metric_at_score(points, 1.00, prefer = prefer_threshold)
  score_30_crossing_values <- reference_curve_threshold_crossings(points, 0.30)
  score_70_crossing_values <- reference_curve_threshold_crossings(points, 0.70)
  score_100_crossing_values <- reference_curve_threshold_crossings(points, 1.00)
  score_30_crossings_display <- reference_curve_crossings_text(score_30_crossing_values)
  score_70_crossings_display <- reference_curve_crossings_text(score_70_crossing_values)
  score_100_crossings_display <- reference_curve_crossings_text(score_100_crossing_values)

  if (higher_is_better) {
    functioning_min <- score_70_metric
    functioning_max <- if (is.na(score_100_metric)) points_max else score_100_metric
    at_risk_min <- score_30_metric
    at_risk_max <- score_70_metric
    not_functioning_min <- points_min
    not_functioning_max <- score_30_metric
  } else {
    functioning_min <- if (is.na(score_100_metric)) points_min else score_100_metric
    functioning_max <- score_70_metric
    at_risk_min <- score_70_metric
    at_risk_max <- score_30_metric
    not_functioning_min <- score_30_metric
    not_functioning_max <- points_max
  }

  if (identical(curve_status, "complete")) {
    if (length(score_30_crossing_values) > 2 || length(score_70_crossing_values) > 2) {
      curve_status <- "unsupported_multi_crossing"
    } else if (anyNA(c(score_30_metric, score_70_metric))) {
      curve_status <- "degenerate_curve"
    }
  }

  point_fields <- list()
  for (idx in seq_len(3L)) {
    if (nrow(points) >= idx) {
      point_fields[[paste0("curve_point", idx, "_x")]] <- points$metric_value[idx]
      point_fields[[paste0("curve_point", idx, "_y")]] <- points$index_score[idx]
    } else {
      point_fields[[paste0("curve_point", idx, "_x")]] <- NA_real_
      point_fields[[paste0("curve_point", idx, "_y")]] <- NA_real_
    }
  }

  tibble::tibble(
    metric = metric_key,
    display_name = mc$display_name,
    n_reference = stats$n_reference,
    median_val = stats$median_val,
    mean_val = stats$mean_val,
    sd_val = stats$sd_val,
    min_val = stats$min_val,
    max_val = stats$max_val,
    q25 = stats$q25,
    q75 = stats$q75,
    iqr = stats$iqr,
    functioning_min = as.numeric(functioning_min),
    functioning_max = as.numeric(functioning_max),
    at_risk_min = as.numeric(at_risk_min),
    at_risk_max = as.numeric(at_risk_max),
    not_functioning_min = as.numeric(not_functioning_min),
    not_functioning_max = as.numeric(not_functioning_max),
    higher_is_better = higher_is_better,
    !!!point_fields,
    curve_n_points = nrow(points),
    curve_source = curve_source,
    score_30_metric = as.numeric(score_30_metric),
    score_70_metric = as.numeric(score_70_metric),
    score_30_crossings = list(score_30_crossing_values),
    score_70_crossings = list(score_70_crossing_values),
    score_100_crossings = list(score_100_crossing_values),
    score_30_crossing_count = length(score_30_crossing_values),
    score_70_crossing_count = length(score_70_crossing_values),
    score_100_crossing_count = length(score_100_crossing_values),
    score_30_crossings_display = score_30_crossings_display,
    score_70_crossings_display = score_70_crossings_display,
    score_100_crossings_display = score_100_crossings_display,
    functioning_ranges = list(band_ranges$functioning),
    at_risk_ranges = list(band_ranges$at_risk),
    not_functioning_ranges = list(band_ranges$not_functioning),
    functioning_ranges_display = reference_curve_interval_ranges_text(band_ranges$functioning),
    at_risk_ranges_display = reference_curve_interval_ranges_text(band_ranges$at_risk),
    not_functioning_ranges_display = reference_curve_interval_ranges_text(band_ranges$not_functioning),
    curve_status = curve_status,
    stratum = if (is.null(stratum_label)) NA_character_ else as.character(stratum_label),
    curve_points = list(points)
  )
}

reference_curve_x_range <- function(ref_values, points) {
  x_values <- c(ref_values, points$metric_value)
  x_values <- x_values[is.finite(x_values)]
  if (length(x_values) == 0) {
    return(c(0, 1))
  }

  x_min <- min(x_values)
  x_max <- max(x_values)
  if (identical(x_min, x_max)) {
    x_min <- x_min - 1
    x_max <- x_max + 1
  }

  padding <- max((x_max - x_min) * 0.08, 0.5)
  c(x_min - padding, x_max + padding)
}

build_reference_distribution_plot <- function(ref_values, curve_row, metric_config,
                                              metric_key, stratum_label = NULL) {
  if (length(ref_values) < 2 || is.null(curve_row) || nrow(curve_row) == 0) {
    return(NULL)
  }

  row <- curve_row[1, , drop = FALSE]
  mc <- metric_config[[metric_key]]
  higher_is_better <- isTRUE(row$higher_is_better[1])
  x_range <- reference_curve_x_range(ref_values, reference_curve_points_from_row(row, higher_is_better))
  score_30_metric <- suppressWarnings(as.numeric(row$score_30_metric[1]))
  score_70_metric <- suppressWarnings(as.numeric(row$score_70_metric[1]))

  annotation_size <- streamcurves_geom_text_size("large_analysis")

  p <- ggplot2::ggplot(tibble::tibble(value = ref_values), ggplot2::aes(x = value))

  if (is.finite(score_70_metric)) {
    if (higher_is_better) {
      p <- p + ggplot2::annotate(
        "rect",
        xmin = score_70_metric, xmax = Inf,
        ymin = -Inf, ymax = Inf,
        fill = "#2ca25f", alpha = 0.12
      )
    } else {
      p <- p + ggplot2::annotate(
        "rect",
        xmin = -Inf, xmax = score_70_metric,
        ymin = -Inf, ymax = Inf,
        fill = "#2ca25f", alpha = 0.12
      )
    }
  }

  if (is.finite(score_30_metric) && is.finite(score_70_metric)) {
    p <- p + ggplot2::annotate(
      "rect",
      xmin = min(score_30_metric, score_70_metric),
      xmax = max(score_30_metric, score_70_metric),
      ymin = -Inf, ymax = Inf,
      fill = "#f0ad4e", alpha = 0.12
    )
  }

  if (is.finite(score_30_metric)) {
    if (higher_is_better) {
      p <- p + ggplot2::annotate(
        "rect",
        xmin = -Inf, xmax = score_30_metric,
        ymin = -Inf, ymax = Inf,
        fill = "#d9534f", alpha = 0.12
      )
    } else {
      p <- p + ggplot2::annotate(
        "rect",
        xmin = score_30_metric, xmax = Inf,
        ymin = -Inf, ymax = Inf,
        fill = "#d9534f", alpha = 0.12
      )
    }
  }

  p <- p +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 12,
      fill = "steelblue",
      alpha = 0.45,
      color = "white"
    ) +
    ggplot2::geom_rug(sides = "b", color = "steelblue", alpha = 0.65) +
    ggplot2::coord_cartesian(xlim = x_range) +
    ggplot2::labs(
      title = paste0(
        mc$display_name,
        ": Reference Distribution",
        if (!is.null(stratum_label)) paste0(" (", stratum_label, ")") else ""
      ),
      subtitle = paste0(
        "n = ", row$n_reference[1],
        " | Q25 = ", round(row$q25[1], 2),
        " | Q75 = ", round(row$q75[1], 2)
      ),
      x = paste0(mc$display_name, " (", mc$units, ")"),
      y = "Density"
    ) +
    streamcurves_minimal_plot_theme(profile = "large_analysis")

  if (is.finite(score_30_metric)) {
    p <- p +
      ggplot2::geom_vline(xintercept = score_30_metric, linetype = "dashed", color = "#b22222", linewidth = 0.8) +
      ggplot2::annotate(
        "text",
        x = score_30_metric,
        y = Inf,
        label = paste0("Score 0.30 = ", round(score_30_metric, 2)),
        vjust = 1.5,
        hjust = -0.02,
        size = annotation_size,
        color = "#b22222"
      )
  }

  if (is.finite(score_70_metric)) {
    p <- p +
      ggplot2::geom_vline(xintercept = score_70_metric, linetype = "dashed", color = "#1b7837", linewidth = 0.8) +
      ggplot2::annotate(
        "text",
        x = score_70_metric,
        y = Inf,
        label = paste0("Score 0.70 = ", round(score_70_metric, 2)),
        vjust = 3,
        hjust = -0.02,
        size = annotation_size,
        color = "#1b7837"
      )
  }

  p
}

build_reference_curve_plot <- function(curve_points, curve_row, metric_config,
                                       metric_key, stratum_label = NULL) {
  points <- normalize_reference_curve_points(curve_points)
  if (nrow(points) < 2 || is.null(curve_row) || nrow(curve_row) == 0) {
    return(NULL)
  }

  row <- curve_row[1, , drop = FALSE]
  mc <- metric_config[[metric_key]]
  x_range <- reference_curve_x_range(numeric(0), points)
  annotation_size <- streamcurves_geom_text_size("large_analysis")

  p <- ggplot2::ggplot(points, ggplot2::aes(x = metric_value, y = index_score)) +
    ggplot2::annotate("rect", xmin = x_range[1], xmax = x_range[2], ymin = 0.70, ymax = 1.00, fill = "#2ca25f", alpha = 0.10) +
    ggplot2::annotate("rect", xmin = x_range[1], xmax = x_range[2], ymin = 0.30, ymax = 0.70, fill = "#f0ad4e", alpha = 0.10) +
    ggplot2::annotate("rect", xmin = x_range[1], xmax = x_range[2], ymin = 0.00, ymax = 0.30, fill = "#d9534f", alpha = 0.10) +
    ggplot2::geom_hline(yintercept = 0.70, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 0.30, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1.2) +
    ggplot2::geom_point(color = "steelblue", size = 2.8) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1.02)) +
    ggplot2::coord_cartesian(xlim = x_range, ylim = c(0, 1.02), expand = FALSE) +
    ggplot2::labs(
      title = paste0(
        mc$display_name,
        ": Reference Curve",
        if (!is.null(stratum_label)) paste0(" (", stratum_label, ")") else ""
      ),
      subtitle = paste0(
        if (identical(row$curve_source[1], "manual")) "Manual point set" else "Auto-generated point set",
        " | n = ", row$n_reference[1]
      ),
      x = paste0(mc$display_name, " (", mc$units, ")"),
      y = "Index Score"
    ) +
    streamcurves_minimal_plot_theme(profile = "large_analysis")

  if (is.finite(row$score_30_metric[1])) {
    p <- p +
      ggplot2::geom_vline(xintercept = row$score_30_metric[1], linetype = "dashed", color = "#b22222", linewidth = 0.7) +
      ggplot2::annotate(
        "label",
        x = row$score_30_metric[1],
        y = 0.03,
        label = paste0("0.30 = ", round(row$score_30_metric[1], 2)),
        size = annotation_size,
        color = "#b22222",
        fill = "white",
        alpha = 0.75,
        border.colour = NA,
        label.padding = ggplot2::unit(0.15, "lines")
      )
  }

  if (is.finite(row$score_70_metric[1])) {
    p <- p +
      ggplot2::geom_vline(xintercept = row$score_70_metric[1], linetype = "dashed", color = "#1b7837", linewidth = 0.7) +
      ggplot2::annotate(
        "label",
        x = row$score_70_metric[1],
        y = 0.03,
        label = paste0("0.70 = ", round(row$score_70_metric[1], 2)),
        size = annotation_size,
        color = "#1b7837",
        fill = "white",
        alpha = 0.75,
        border.colour = NA,
        label.padding = ggplot2::unit(0.15, "lines")
      )
  }

  p +
    ggplot2::annotate("text", x = x_range[2], y = 0.85, label = "Functioning", hjust = 1, size = annotation_size, color = "#1b7837", fontface = "italic") +
    ggplot2::annotate("text", x = x_range[2], y = 0.50, label = "At-Risk", hjust = 1, size = annotation_size, color = "#8a6d3b", fontface = "italic") +
    ggplot2::annotate("text", x = x_range[2], y = 0.15, label = "Not Functioning", hjust = 1, size = annotation_size, color = "#b22222", fontface = "italic")
}

normalize_reference_curve_result <- function(result, metric_config = NULL,
                                             metric_key = NULL, stratum_label = NULL) {
  if (is.null(result)) {
    return(NULL)
  }

  target <- result
  if (is.list(result) && !is.null(result$reference_curve) && is.null(result$curve_row)) {
    target <- result$reference_curve
  }

  curve_row <- NULL
  if (!is.null(target$curve_row) && nrow(target$curve_row) > 0) {
    curve_row <- tibble::as_tibble(target$curve_row)[1, , drop = FALSE]
  }

  if (is.null(metric_key) && !is.null(curve_row) && "metric" %in% names(curve_row)) {
    metric_key <- curve_row$metric[1]
  }

  higher_is_better <- NULL
  if (!is.null(curve_row) && "higher_is_better" %in% names(curve_row)) {
    higher_is_better <- isTRUE(curve_row$higher_is_better[1])
  } else if (!is.null(metric_key) && !is.null(metric_config[[metric_key]])) {
    higher_is_better <- isTRUE(metric_config[[metric_key]]$higher_is_better)
  }

  curve_points <- normalize_reference_curve_points(target$curve_points)
  if (nrow(curve_points) < 2 && !is.null(curve_row)) {
    curve_points <- reference_curve_points_from_row(curve_row, higher_is_better)
  }

  curve_source <- if (!is.null(target$curve_source)) {
    as.character(target$curve_source)
  } else if (!is.null(curve_row) && "curve_source" %in% names(curve_row)) {
    as.character(curve_row$curve_source[1])
  } else {
    "auto"
  }

  if (!is.null(curve_row) && !is.null(metric_key) && !is.null(metric_config[[metric_key]])) {
    stats <- list(
      n_reference = curve_row$n_reference[1],
      median_val = curve_row$median_val[1],
      mean_val = curve_row$mean_val[1],
      sd_val = curve_row$sd_val[1],
      min_val = curve_row$min_val[1],
      max_val = curve_row$max_val[1],
      q25 = curve_row$q25[1],
      q75 = curve_row$q75[1],
      iqr = curve_row$iqr[1]
    )
    current_status <- if ("curve_status" %in% names(curve_row)) curve_row$curve_status[1] else "complete"
    current_stratum <- if (!is.null(stratum_label)) stratum_label else if ("stratum" %in% names(curve_row)) curve_row$stratum[1] else NA_character_

    rebuilt_row <- build_reference_curve_row(
      metric_key,
      metric_config = metric_config,
      stats = stats,
      curve_points = curve_points,
      curve_source = curve_source,
      stratum_label = current_stratum,
      curve_status = current_status
    )

    for (nm in names(curve_row)) {
      if (!(nm %in% names(rebuilt_row)) || identical(nm, "curve_points")) {
        rebuilt_row[[nm]] <- curve_row[[nm]]
      }
    }
    curve_row <- rebuilt_row[, unique(c(names(rebuilt_row), names(curve_row))), drop = FALSE]
  }

  list(
    curve_row = curve_row,
    curve_points = curve_points,
    curve_source = curve_source,
    bar_chart_plot = target$bar_chart_plot,
    curve_plot = target$curve_plot
  )
}

strip_reference_curve_result <- function(result, metric_config = NULL,
                                         metric_key = NULL, stratum_label = NULL) {
  normalized <- normalize_reference_curve_result(
    result,
    metric_config = metric_config,
    metric_key = metric_key,
    stratum_label = stratum_label
  )

  if (is.null(normalized)) {
    return(NULL)
  }

  normalized$bar_chart_plot <- NULL
  normalized$curve_plot <- NULL
  normalized
}

hydrate_reference_curve_result <- function(result, data, metric_key, metric_config,
                                           stratum_label = NULL,
                                           artifact_mode = c("full", "summary")) {
  artifact_mode <- match.arg(artifact_mode)
  normalized <- normalize_reference_curve_result(
    result,
    metric_config = metric_config,
    metric_key = metric_key,
    stratum_label = stratum_label
  )

  if (is.null(normalized)) {
    return(NULL)
  }

  if (identical(artifact_mode, "summary")) {
    normalized$bar_chart_plot <- NULL
    normalized$curve_plot <- NULL
    return(normalized)
  }

  curve_source <- if (is.null(normalized$curve_source)) "auto" else normalized$curve_source
  curve_points <- normalize_reference_curve_points(normalized$curve_points)
  curve_status <- normalized$curve_row$curve_status[1] %||% "complete"

  if (!identical(curve_status, "complete")) {
    return(normalized)
  }

  if (!is.null(normalized$bar_chart_plot) && !is.null(normalized$curve_plot)) {
    return(normalized)
  }

  if (identical(curve_source, "manual") && nrow(curve_points) >= 2) {
    return(build_reference_curve_from_points(
      data = data,
      metric_key = metric_key,
      metric_config = metric_config,
      curve_points = curve_points,
      stratum_label = stratum_label,
      build_plots = TRUE
    ))
  }

  build_reference_curve(
    data = data,
    metric_key = metric_key,
    metric_config = metric_config,
    stratum_label = stratum_label,
    build_plots = TRUE
  )
}

reference_curve_rows_for_export <- function(rows) {
  if (is.null(rows)) {
    return(tibble::tibble())
  }

  rows <- tibble::as_tibble(rows)
  crossing_fields <- c("score_30_crossings", "score_70_crossings", "score_100_crossings")
  range_fields <- c("functioning_ranges", "at_risk_ranges", "not_functioning_ranges")

  for (field in crossing_fields) {
    if (field %in% names(rows) && is.list(rows[[field]])) {
      rows[[field]] <- vapply(rows[[field]], reference_curve_crossings_text, character(1))
    }
  }

  for (field in range_fields) {
    if (field %in% names(rows) && is.list(rows[[field]])) {
      rows[[field]] <- vapply(rows[[field]], reference_curve_interval_ranges_text, character(1))
    }
  }

  drop_fields <- c(
    "score_30_crossings_display",
    "score_70_crossings_display",
    "score_100_crossings_display",
    "functioning_ranges_display",
    "at_risk_ranges_display",
    "not_functioning_ranges_display"
  )
  rows <- rows[, setdiff(names(rows), drop_fields), drop = FALSE]

  if ("curve_points" %in% names(rows)) {
    rows$curve_points <- NULL
  }
  rows
}

build_reference_curve_from_components <- function(data, metric_key, metric_config, curve_points,
                                                  curve_source = "auto", stratum_label = NULL,
                                                  curve_status = "complete",
                                                  build_plots = TRUE) {
  mc <- metric_config[[metric_key]]
  col_name <- mc$column_name
  ref_values <- data[[col_name]]
  ref_values <- ref_values[!is.na(ref_values)]
  stats <- reference_curve_summary_stats(ref_values)

  curve_row <- build_reference_curve_row(
    metric_key = metric_key,
    metric_config = metric_config,
    stats = stats,
    curve_points = curve_points,
    curve_source = curve_source,
    stratum_label = stratum_label,
    curve_status = curve_status
  )

  points <- normalize_reference_curve_points(curve_points)
  bar_chart_plot <- NULL
  curve_plot <- NULL

  if (isTRUE(build_plots) && identical(curve_row$curve_status[1], "complete")) {
    bar_chart_plot <- build_reference_distribution_plot(ref_values, curve_row, metric_config, metric_key, stratum_label)
    curve_plot <- build_reference_curve_plot(points, curve_row, metric_config, metric_key, stratum_label)
  }

  list(
    curve_row = curve_row,
    curve_points = points,
    curve_source = curve_source,
    bar_chart_plot = bar_chart_plot,
    curve_plot = curve_plot
  )
}

build_reference_curve <- function(data, metric_key, metric_config, stratum_label = NULL,
                                  build_plots = TRUE) {
  mc <- metric_config[[metric_key]]
  col_name <- mc$column_name
  higher_is_better <- isTRUE(mc$higher_is_better)
  ref_values <- data[[col_name]]
  ref_values <- ref_values[!is.na(ref_values)]
  stats <- reference_curve_summary_stats(ref_values)

  if (length(ref_values) < 5) {
    cli::cli_alert_warning("{metric_key}: too few reference values ({length(ref_values)})")
    empty_row <- build_reference_curve_row(
      metric_key = metric_key,
      metric_config = metric_config,
      stats = stats,
      curve_points = empty_reference_curve_points(),
      curve_source = "auto",
      stratum_label = stratum_label,
      curve_status = "insufficient_data"
    )

    return(list(
      curve_row = empty_row,
      curve_points = empty_reference_curve_points(),
      curve_source = "auto",
      bar_chart_plot = NULL,
      curve_plot = NULL
    ))
  }

  if (higher_is_better && (!is.finite(stats$q25) || stats$q25 <= 0)) {
    cli::cli_alert_warning("{metric_key}: Q25 <= 0, scoring curve is degenerate")
    return(build_reference_curve_from_components(
      data = data,
      metric_key = metric_key,
      metric_config = metric_config,
      curve_points = tibble::tibble(
        point_order = 1:3,
        metric_value = c(0, stats$q25, stats$q75),
        index_score = c(0.00, 0.70, 1.00)
      ),
      curve_source = "auto",
      stratum_label = stratum_label,
      curve_status = "degenerate_q25",
      build_plots = build_plots
    ))
  }

  if (!is.finite(stats$iqr) || stats$iqr < 0) {
    return(build_reference_curve_from_components(
      data = data,
      metric_key = metric_key,
      metric_config = metric_config,
      curve_points = empty_reference_curve_points(),
      curve_source = "auto",
      stratum_label = stratum_label,
      curve_status = "degenerate_curve",
      build_plots = build_plots
    ))
  }

  auto_points <- if (higher_is_better) {
    tibble::tibble(
      point_order = 1:5,
      metric_value = c(
        0,
        as.numeric(stats$q25 * 3 / 7),
        as.numeric(stats$q25),
        as.numeric(stats$q75),
        as.numeric(stats$q75 + stats$iqr * 0.3)
      ),
      index_score = c(0.00, 0.30, 0.70, 1.00, 1.00)
    )
  } else {
    tibble::tibble(
      point_order = 1:5,
      metric_value = c(
        as.numeric(max(0, stats$q25 - stats$iqr * 0.3)),
        as.numeric(stats$q25),
        as.numeric(stats$q75),
        as.numeric(stats$q75 + stats$iqr * 4 / 3),
        as.numeric(stats$q75 + stats$iqr * 7 / 3)
      ),
      index_score = c(1.00, 1.00, 0.70, 0.30, 0.00)
    )
  }

  validation <- validate_reference_curve_points(auto_points, higher_is_better)
  status <- if (isTRUE(validation$valid)) "complete" else "degenerate_curve"

  build_reference_curve_from_components(
    data = data,
    metric_key = metric_key,
    metric_config = metric_config,
    curve_points = if (isTRUE(validation$valid)) validation$points else auto_points,
    curve_source = "auto",
    stratum_label = stratum_label,
    curve_status = status,
    build_plots = build_plots
  )
}

build_reference_curve_from_points <- function(data, metric_key, metric_config, curve_points,
                                              stratum_label = NULL,
                                              build_plots = TRUE) {
  higher_is_better <- isTRUE(metric_config[[metric_key]]$higher_is_better)
  validation <- validate_reference_curve_points(curve_points, higher_is_better)
  if (!isTRUE(validation$valid)) {
    stop(paste(validation$errors, collapse = " "), call. = FALSE)
  }

  build_reference_curve_from_components(
    data = data,
    metric_key = metric_key,
    metric_config = metric_config,
    curve_points = validation$points,
    curve_source = "manual",
    stratum_label = stratum_label,
    curve_status = "complete",
    build_plots = build_plots
  )
}

build_overlay_curve_plot <- function(curve_rows, metric_config) {
  if (is.null(curve_rows) || nrow(curve_rows) < 2) {
    return(NULL)
  }

  curve_rows <- tibble::as_tibble(curve_rows)
  metric_key <- curve_rows$metric[1]
  mc <- metric_config[[metric_key]]

  all_curves <- purrr::map_dfr(seq_len(nrow(curve_rows)), function(i) {
    row <- curve_rows[i, , drop = FALSE]
    if (!identical(as.character(row$curve_status[1]), "complete")) {
      return(NULL)
    }

    points <- reference_curve_points_from_row(row, isTRUE(mc$higher_is_better))
    if (nrow(points) < 2) {
      return(NULL)
    }

    tibble::tibble(
      x = points$metric_value,
      y = points$index_score,
      stratum = row$stratum[1]
    )
  })

  if (nrow(all_curves) == 0) {
    return(NULL)
  }

  all_points <- purrr::map_dfr(seq_len(nrow(curve_rows)), function(i) {
    row <- curve_rows[i, , drop = FALSE]
    if (!identical(as.character(row$curve_status[1]), "complete")) {
      return(NULL)
    }

    points <- reference_curve_points_from_row(row, isTRUE(mc$higher_is_better))
    if (nrow(points) < 2) {
      return(NULL)
    }

    tibble::tibble(
      x = points$metric_value,
      y = points$index_score,
      stratum = row$stratum[1]
    )
  })

  x_range <- reference_curve_x_range(numeric(0), tibble::tibble(
    point_order = seq_len(nrow(all_curves)),
    metric_value = all_curves$x,
    index_score = all_curves$y
  ))

  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = x_range[1], xmax = x_range[2], ymin = 0.70, ymax = 1.00, fill = "#2ca25f", alpha = 0.08) +
    ggplot2::annotate("rect", xmin = x_range[1], xmax = x_range[2], ymin = 0.30, ymax = 0.70, fill = "#f0ad4e", alpha = 0.08) +
    ggplot2::annotate("rect", xmin = x_range[1], xmax = x_range[2], ymin = 0.00, ymax = 0.30, fill = "#d9534f", alpha = 0.08) +
    ggplot2::geom_hline(yintercept = 0.70, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 0.30, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    ggplot2::geom_line(data = all_curves, ggplot2::aes(x = x, y = y, color = stratum), linewidth = 1.2) +
    ggplot2::geom_point(data = all_points, ggplot2::aes(x = x, y = y, color = stratum), size = 2.8) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    ggplot2::coord_cartesian(xlim = x_range, ylim = c(0, 1.02), expand = FALSE) +
    ggplot2::labs(
      title = paste0(mc$display_name, ": Cross-Stratum Scoring Curves"),
      subtitle = paste0("Strata: ", paste(unique(all_curves$stratum), collapse = ", ")),
      x = paste0(mc$display_name, " (", mc$units, ")"),
      y = "Index Score",
      color = "Stratum"
    ) +
    streamcurves_minimal_plot_theme(profile = "large_analysis", legend_position = "bottom")
}

build_overlay_bar_chart <- function(data, metric_key, metric_config, strat_var, levels) {
  mc <- metric_config[[metric_key]]
  col_name <- mc$column_name

  plot_data <- data |>
    dplyr::filter(.data[[strat_var]] %in% levels) |>
    dplyr::select(value = dplyr::all_of(col_name), stratum = dplyr::all_of(strat_var)) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(stratum = as.character(stratum))

  if (nrow(plot_data) < 4) {
    return(NULL)
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(x = value, fill = stratum, color = stratum)) +
    ggplot2::geom_density(alpha = 0.30) +
    ggplot2::geom_rug(alpha = 0.55, show.legend = FALSE) +
    ggplot2::labs(
      title = paste0(mc$display_name, ": Cross-Stratum Distributions"),
      subtitle = paste0("Strata: ", paste(levels, collapse = ", ")),
      x = paste0(mc$display_name, " (", mc$units, ")"),
      y = "Density",
      fill = "Stratum",
      color = "Stratum"
    ) +
    streamcurves_minimal_plot_theme(profile = "large_analysis", legend_position = "bottom")
}

run_all_reference_curves <- function(data, metric_config,
                                     model_selections = NULL,
                                     diagnostic_summary = NULL,
                                     all_models = NULL) {
  cli::cli_alert_info("Building reference curves for all metrics...")

  eligible <- purrr::keep(names(metric_config), function(mk) {
    mc <- metric_config[[mk]]
    !(mc$metric_family %in% c("categorical")) && !is.null(mc$higher_is_better)
  })

  cli::cli_alert_info("Processing {length(eligible)} metrics...")

  map_fn <- if (requireNamespace("furrr", quietly = TRUE) &&
                !inherits(future::plan(), "sequential")) {
    function(...) furrr::future_map(..., .options = furrr::furrr_options(seed = TRUE))
  } else {
    purrr::map
  }

  results_list <- map_fn(eligible, function(metric_key) {
    build_reference_curve(data, metric_key, metric_config)
  })

  registry <- dplyr::bind_rows(purrr::map(results_list, "curve_row"))

  bar_chart_list <- purrr::map(results_list, "bar_chart_plot")
  non_null_bar <- !vapply(bar_chart_list, is.null, logical(1))
  bar_chart_plots <- purrr::set_names(bar_chart_list[non_null_bar], eligible[non_null_bar])

  curve_list <- purrr::map(results_list, "curve_plot")
  non_null_curve <- !vapply(curve_list, is.null, logical(1))
  curve_plots <- purrr::set_names(curve_list[non_null_curve], eligible[non_null_curve])

  cli::cli_alert_success(
    "Reference curves complete: {nrow(registry)} curves built, {length(bar_chart_plots)} bar charts, {length(curve_plots)} curve plots"
  )

  list(
    registry = registry,
    bar_chart_plots = bar_chart_plots,
    curve_plots = curve_plots
  )
}
