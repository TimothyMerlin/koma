#' Plot HDRs for koma_estimate Objects
#'
#' Plot HDR intervals for coefficients from a `koma_estimate_hdr` object.
#' Outliers beyond the outer HDR are shown if available in the object.
#' The plot requires HDR levels matching `box_levels` to be present in the
#' object.
#'
#' @param x A `koma_estimate_hdr` object.
#' @param y Ignored. Included for compatibility with the generic function.
#' @param ... Additional arguments controlling the plot. See Details.
#'
#' @details
#' Additional arguments supported in `...`:
#' \describe{
#'   \item{variables}{Optional character vector of endogenous variables to plot.}
#'   \item{params}{Optional character vector of parameter groups to plot
#'     (e.g., "beta", "gamma", "sigma").}
#'   \item{box_levels}{Optional numeric probabilities in (0, 1] (or \eqn{[0, 100]})
#'     defining the inner and outer HDRs. By default, uses the available
#'     HDR levels closest to 50% and the maximum probability.}
#'   \item{interactive}{Logical. If TRUE and plotly is available, return an
#'     interactive plot via \code{plotly::ggplotly}. Default is FALSE.}
#' }
#'
#' @return A ggplot object, or a plotly object when `interactive = TRUE` and
#'   plotly is available.
#'
#' @export
plot.koma_estimate_hdr <- function(x, y = NULL, ...) {
  stopifnot(inherits(x, "koma_estimate_hdr"))

  args <- list(...)
  variables <- args$variables
  params <- args$params
  box_levels <- args$box_levels
  interactive <- isTRUE(args$interactive)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(c(
      "!" = "The {.pkg ggplot2} package is required for plotting.",
      "i" = "Install it with: {.code install.packages('ggplot2')}"
    ))
  }

  interval_out <- x$intervals
  if (is.null(variables)) {
    variables <- names(interval_out)
  }
  if (is.null(variables) || !length(variables)) {
    cli::cli_abort("`variables` must contain at least one name.")
  }

  missing_vars <- setdiff(variables, names(interval_out))
  if (length(missing_vars) > 0L) {
    cli::cli_abort(c(
      "The following variables are not contained in HDR output:",
      ">" = missing_vars
    ))
  }

  probs <- x$probs
  single_level <- FALSE
  if (is.null(box_levels)) {
    if (length(probs) < 2L) {
      box_probs <- probs
      single_level <- TRUE
    } else {
      inner <- probs[which.min(abs(probs - 0.5))]
      outer <- max(probs)
      box_probs <- sort(c(inner, outer))
    }
  } else {
    box_probs <- normalize_quantile_probs(box_levels)
    if (is.null(box_probs) || !length(box_probs)) {
      cli::cli_abort("`box_levels` must contain numeric values in (0, 1] or [0, 100].")
    }
    if (length(box_probs) == 1L) {
      single_level <- TRUE
    }
    if (!single_level && length(box_probs) != 2L) {
      cli::cli_abort("`box_levels` must contain two distinct values in (0, 1] or [0, 100].")
    }
    box_probs <- sort(box_probs)
  }
  level_names <- paste0("level_", format(box_probs * 100, trim = TRUE))
  if (!all(level_names %in% paste0("level_", format(probs * 100, trim = TRUE)))) {
    cli::cli_abort("`box_levels` must match available HDR levels in the object.")
  }

  labels <- character(0)
  modes <- numeric(0)
  mode_labels <- character(0)
  shapes <- list()
  out_x <- numeric(0)
  out_y <- numeric(0)
  y_vals <- numeric(0)

  for (variable in variables) {
    param_groups <- interval_out[[variable]]
    if (!is.null(params)) {
      param_groups <- param_groups[names(param_groups) %in% params]
    }
    if (!length(param_groups)) {
      next
    }

    for (param_name in names(param_groups)) {
      coef_list <- param_groups[[param_name]]
      for (coef_name in names(coef_list)) {
        coef_levels <- coef_list[[coef_name]]
        label <- paste(variable, param_name, coef_name, sep = ":")
        labels <- c(labels, label)
        mode_val <- x$mode[[variable]][[param_name]][[coef_name]]
        modes <- c(modes, mode_val)
        mode_labels <- c(mode_labels, label)
      }
    }
  }

  if (!length(labels)) {
    cli::cli_abort("No coefficients available to plot after filtering.")
  }

  label_levels <- unique(labels)
  x_vals <- seq_along(label_levels)
  x_map <- stats::setNames(x_vals, label_levels)

  shade_99 <- grDevices::rgb(0, 0, 0, 0.15)
  shade_50 <- grDevices::rgb(0, 0, 0, 0.35)

  for (variable in variables) {
    param_groups <- interval_out[[variable]]
    if (!is.null(params)) {
      param_groups <- param_groups[names(param_groups) %in% params]
    }
    if (!length(param_groups)) {
      next
    }
    for (param_name in names(param_groups)) {
      coef_list <- param_groups[[param_name]]
      for (coef_name in names(coef_list)) {
        coef_levels <- coef_list[[coef_name]]
        label <- paste(variable, param_name, coef_name, sep = ":")
        x_center <- x_map[[label]]
        x0 <- x_center - 0.3
        x1 <- x_center + 0.3

        mat_outer <- coef_levels[[level_names[1]]]
        mat_inner <- if (!single_level) coef_levels[[level_names[2]]] else NULL

        if (nrow(mat_outer) > 0L) {
          for (i in seq_len(nrow(mat_outer))) {
            shapes[[length(shapes) + 1]] <- list(
              type = "rect",
              x0 = x0,
              x1 = x1,
              y0 = mat_outer[i, "lower"],
              y1 = mat_outer[i, "upper"],
              line = list(width = 0),
              fillcolor = shade_99
            )
            y_vals <- c(y_vals, mat_outer[i, "lower"], mat_outer[i, "upper"])
          }
        }
        if (!single_level && nrow(mat_inner) > 0L) {
          for (i in seq_len(nrow(mat_inner))) {
            shapes[[length(shapes) + 1]] <- list(
              type = "rect",
              x0 = x0,
              x1 = x1,
              y0 = mat_inner[i, "lower"],
              y1 = mat_inner[i, "upper"],
              line = list(width = 0),
              fillcolor = shade_50
            )
            y_vals <- c(y_vals, mat_inner[i, "lower"], mat_inner[i, "upper"])
          }
        }

        mode_val <- x$mode[[variable]][[param_name]][[coef_name]]
        shapes[[length(shapes) + 1]] <- list(
          type = "line",
          x0 = x0,
          x1 = x1,
          y0 = mode_val,
          y1 = mode_val,
          line = list(color = "black", width = 2)
        )
        y_vals <- c(y_vals, mode_val)

        if (!is.null(x$outliers)) {
          outs <- x$outliers[[variable]][[param_name]][[coef_name]]
          if (!is.null(outs) && length(outs)) {
            if (nrow(mat_outer) > 0L) {
              in_any <- rep(FALSE, length(outs))
              for (i in seq_len(nrow(mat_outer))) {
                in_any <- in_any | (outs >= mat_outer[i, "lower"] & outs <= mat_outer[i, "upper"])
              }
              outs <- outs[!in_any]
            }
            if (length(outs)) {
              out_x <- c(out_x, rep(x_map[[label]], length(outs)))
              out_y <- c(out_y, outs)
              y_vals <- c(y_vals, outs)
            }
          }
        }
      }
    }
  }

  rect_df <- do.call(
    rbind,
    lapply(shapes, function(s) {
      if (!identical(s$type, "rect")) {
        return(NULL)
      }
      data.frame(
        xmin = s$x0,
        xmax = s$x1,
        ymin = s$y0,
        ymax = s$y1,
        fill = s$fillcolor,
        stringsAsFactors = FALSE
      )
    })
  )
  if (is.null(rect_df)) {
    rect_df <- data.frame(
      xmin = numeric(0),
      xmax = numeric(0),
      ymin = numeric(0),
      ymax = numeric(0),
      fill = character(0),
      stringsAsFactors = FALSE
    )
  }
  if (nrow(rect_df)) {
    if (single_level) {
      rect_df$level <- paste0(format(box_probs[1] * 100, trim = TRUE), "% HDR")
    } else {
      rect_df$level <- ifelse(
        rect_df$fill == shade_50,
        paste0(format(box_probs[1] * 100, trim = TRUE), "% HDR"),
        paste0(format(box_probs[2] * 100, trim = TRUE), "% HDR")
      )
    }
  } else {
    rect_df$level <- character(0)
  }

  line_df <- do.call(
    rbind,
    lapply(shapes, function(s) {
      if (!identical(s$type, "line")) {
        return(NULL)
      }
      data.frame(
        x = s$x0,
        xend = s$x1,
        y = s$y0,
        yend = s$y1,
        stringsAsFactors = FALSE
      )
    })
  )
  if (is.null(line_df)) {
    line_df <- data.frame(
      x = numeric(0),
      xend = numeric(0),
      y = numeric(0),
      yend = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  out_df <- if (length(out_x)) {
    data.frame(x = out_x, y = out_y, label = "Outliers", stringsAsFactors = FALSE)
  } else {
    data.frame(x = numeric(0), y = numeric(0), label = character(0))
  }

  y_min <- min(y_vals, na.rm = TRUE)
  y_max <- max(y_vals, na.rm = TRUE)
  pad <- 0.2 * (y_max - y_min)
  if (!is.finite(pad) || pad == 0) {
    pad <- if (is.finite(y_max) && y_max != 0) abs(y_max) * 0.05 else 0.1
  }
  y_range <- c(y_min - pad, y_max + pad)

  .data <- ggplot2::.data

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = rect_df,
      ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$level
      ),
      color = NA
    ) +
    ggplot2::scale_fill_manual(
      name = "HDR",
      values = if (single_level) {
        stats::setNames(
          shade_99,
          paste0(format(box_probs[1] * 100, trim = TRUE), "% HDR")
        )
      } else {
        stats::setNames(
          c(shade_99, shade_50),
          c(
            paste0(format(box_probs[2] * 100, trim = TRUE), "% HDR"),
            paste0(format(box_probs[1] * 100, trim = TRUE), "% HDR")
          )
        )
      },
      guide = ggplot2::guide_legend(order = 1)
    ) +
    ggplot2::geom_segment(
      data = line_df,
      ggplot2::aes(
        x = .data$x,
        xend = .data$xend,
        y = .data$y,
        yend = .data$yend
      ),
      color = "black",
      linewidth = 0.6
    ) +
    ggplot2::scale_x_continuous(
      breaks = x_vals,
      labels = label_levels
    ) +
    ggplot2::coord_cartesian(ylim = y_range) +
    ggplot2::labs(x = NULL, y = "Coefficient") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 1)
    )

  if (nrow(out_df) > 0L) {
    p <- p +
      ggplot2::geom_point(
        data = out_df,
        ggplot2::aes(x = .data$x, y = .data$y, color = .data$label),
        size = 1.6
      ) +
      ggplot2::scale_color_manual(
        name = NULL,
        values = c(Outliers = "black"),
        guide = ggplot2::guide_legend(order = 2, override.aes = list(size = 1.6))
      )
  }

  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    return(plotly::ggplotly(p))
  }
  if (interactive && !requireNamespace("plotly", quietly = TRUE)) {
    cli::cli_warn("plotly is not installed; returning a static ggplot.")
  }
  p
}
