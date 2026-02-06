#' Summarize Interval Estimates for koma_estimate_* Objects
#'
#' @param object A koma_estimate_* object with interval output.
#' @param variables Optional character vector to filter variables.
#' @param params Optional character vector to filter parameters (e.g.,
#'   "beta", "gamma", "sigma").
#' @param digits Number of digits to round numeric values.
#' @param center_label Label for the center statistic (e.g., "Mode", "Median").
#' @param interval_label Label for the interval type (e.g., "HDR", "HDI").
#'
#' @return Invisibly returns `object`.
#' @keywords internal
summary_interval_table <- function(object,
                                   variables = NULL,
                                   params = NULL,
                                   digits = 3,
                                   center_label = "Mode",
                                   interval_label = "HDR") {
  interval_out <- object$intervals

  if (is.null(variables)) {
    variables <- names(interval_out)
  }

  center_text <- function(text, width) {
    pad <- max(0, width - nchar(text))
    left <- floor(pad / 2)
    right <- pad - left
    paste0(strrep(" ", left), text, strrep(" ", right))
  }

  format_num <- function(x) {
    format(round(x, digits), trim = TRUE)
  }

  format_intervals <- function(mat) {
    pieces <- apply(mat, 1, function(x) {
      sprintf("[%s; %s]", format_num(x[1]), format_num(x[2]))
    })
    paste(pieces, collapse = "; ")
  }

  order_params <- function(param_groups) {
    preferred <- c("beta", "gamma", "sigma")
    existing <- names(param_groups)
    c(preferred[preferred %in% existing], setdiff(existing, preferred))
  }

  blocks <- list()
  coef_orders <- list()

  missing_vars <- setdiff(variables, names(interval_out))
  if (length(missing_vars) > 0L) {
    cli::cli_abort(c(
      "Variable not found in endogenous variables:",
      ">" = missing_vars
    ))
  }

  for (variable in variables) {
    param_groups <- interval_out[[variable]]
    if (!is.null(params)) {
      param_groups <- param_groups[names(param_groups) %in% params]
    }
    if (!length(param_groups)) {
      next
    }

    coef_names <- unlist(lapply(order_params(param_groups), function(param_name) {
      names(param_groups[[param_name]])
    }), use.names = FALSE)

    blocks[[variable]] <- param_groups
    coef_orders[[variable]] <- coef_names
  }

  if (!length(blocks)) {
    return(invisible(object))
  }

  all_coefs <- character(0)
  for (variable in names(blocks)) {
    for (coef_name in coef_orders[[variable]]) {
      if (!coef_name %in% all_coefs) {
        all_coefs <- c(all_coefs, coef_name)
      }
    }
  }

  level_labels <- paste0(" ", format(object$probs * 100, trim = TRUE), "%")

  left_width <- max(
    nchar(all_coefs),
    nchar(level_labels),
    na.rm = TRUE
  )

  col_names <- names(blocks)
  col_widths <- stats::setNames(rep(0L, length(col_names)), col_names)

  coef_info <- list()
  for (variable in col_names) {
    param_groups <- blocks[[variable]]
    coef_info[[variable]] <- list()
    for (param_name in names(param_groups)) {
      coef_list <- param_groups[[param_name]]
      for (coef_name in names(coef_list)) {
        coef_info[[variable]][[coef_name]] <- list(
          mode = format_num(object$mode[[variable]][[param_name]][[coef_name]]),
          intervals = lapply(coef_list[[coef_name]], format_intervals)
        )
      }
    }

    col_strings <- c(variable)
    for (coef_name in all_coefs) {
      info <- coef_info[[variable]][[coef_name]]
      if (is.null(info)) {
        next
      }
      col_strings <- c(col_strings, info$mode, unlist(info$intervals))
    }
    col_widths[[variable]] <- max(nchar(col_strings))
  }

  make_row <- function(left, cols, align_right = TRUE) {
    left_fmt <- sprintf("%-*s", left_width, left)
    col_fmt <- vapply(seq_along(cols), function(i) {
      if (align_right) {
        sprintf("%*s", col_widths[[i]], cols[[i]])
      } else {
        sprintf("%-*s", col_widths[[i]], cols[[i]])
      }
    }, character(1))
    paste(c(left_fmt, col_fmt), collapse = "  ")
  }

  header <- make_row(
    "",
    vapply(col_names, function(name) {
      center_text(name, col_widths[[name]])
    }, character(1)),
    align_right = FALSE
  )

  sep_top <- strrep("=", nchar(header))
  sep_mid <- strrep("-", nchar(header))

  lines <- c(sep_top, header, sep_mid)

  for (coef_name in all_coefs) {
    mode_cols <- vapply(col_names, function(variable) {
      info <- coef_info[[variable]][[coef_name]]
      if (is.null(info)) "" else info$mode
    }, character(1))
    lines <- c(lines, make_row(coef_name, mode_cols))

    for (lx in seq_along(level_labels)) {
      label <- level_labels[lx]
      interval_cols <- vapply(col_names, function(variable) {
        info <- coef_info[[variable]][[coef_name]]
        if (is.null(info)) "" else info$intervals[[lx]]
      }, character(1))
      lines <- c(lines, make_row(label, interval_cols))
    }
  }

  lines <- c(lines, sep_top)

  cat(paste(lines, collapse = "\n"), "\n", sep = "")

  note <- sprintf("%s, [%s]", center_label, interval_label)
  if (!is.null(object$dates$estimation$start) &&
    !is.null(object$dates$estimation$end)) {
    start <- dates_to_str(object$dates$estimation$start, frequency = 4)
    end <- dates_to_str(object$dates$estimation$end, frequency = 4)
    note <- paste0(note, "\nEstimation period: ", start, " - ", end)
  }
  cat(note, "\n")

  invisible(object)
}

#' Forecast Summary Helpers
#'
#' Shared helpers for forecast summary tables.
#'
#' @keywords internal
summary_forecast_format_num <- function(x, digits) {
  format(round(x, digits), trim = TRUE)
}

#' @keywords internal
summary_forecast_resolve_horizon <- function(horizon, display_labels, raw_labels = NULL) {
  n_h <- length(display_labels)
  if (is.null(horizon)) {
    return(seq_len(n_h))
  }
  if (is.character(horizon)) {
    idx <- match(horizon, display_labels)
    if (anyNA(idx) && !is.null(raw_labels)) {
      idx2 <- match(horizon, raw_labels)
      idx[is.na(idx)] <- idx2[is.na(idx)]
    }
    if (anyNA(idx)) {
      cli::cli_abort(c(
        "Horizon labels not found:",
        ">" = horizon[is.na(idx)]
      ))
    }
    return(idx)
  }
  if (!is.numeric(horizon)) {
    cli::cli_abort("`horizon` must be numeric or character.")
  }
  if (length(horizon) == 1L) {
    if (is.na(horizon) || horizon < 1) {
      cli::cli_abort("`horizon` must be a positive integer.")
    }
    return(seq_len(min(n_h, floor(horizon))))
  }
  if (any(horizon < 1 | horizon > n_h | horizon %% 1 != 0)) {
    cli::cli_abort("`horizon` must be valid indices within forecast horizon.")
  }
  horizon
}

#' @keywords internal
summary_forecast_write_table <- function(col_names, row_mat) {
  col_widths <- pmax(nchar(col_names), apply(nchar(row_mat), 2, max))

  format_row <- function(row, header = FALSE) {
    pieces <- vapply(seq_along(row), function(i) {
      if (header || i == 1L) {
        sprintf("%-*s", col_widths[i], row[i])
      } else {
        sprintf("%*s", col_widths[i], row[i])
      }
    }, character(1))
    paste(pieces, collapse = "  ")
  }

  header <- format_row(col_names, header = TRUE)
  sep_top <- strrep("=", nchar(header))
  sep_mid <- strrep("-", nchar(header))
  cat(sep_top, "\n", header, "\n", sep_mid, "\n", sep = "")
  cat(paste(vapply(seq_len(nrow(row_mat)), function(i) {
    format_row(row_mat[i, ])
  }, character(1)), collapse = "\n"), "\n", sep = "")
  cat(sep_top, "\n\n", sep = "")
}
