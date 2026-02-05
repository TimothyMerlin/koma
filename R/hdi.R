#' Highest Density Intervals from Draws
#'
#' Computes highest density intervals (HDIs) for a numeric sample. The HDI is
#' defined as the shortest interval containing a target probability mass.
#'
#' @param x A numeric vector of draws.
#' @param ... Unused.
#'
#' @return A list with class \code{"koma_hdi"} containing:
#' \describe{
#'   \item{intervals}{Named list of matrices with columns \code{lower} and
#'   \code{upper}, one matrix per \code{probs} level.}
#'   \item{mode}{Sample median of the draws (used as a center reference).}
#'   \item{cutoff}{Named numeric vector of \code{NA} values (not defined for
#'    HDI).}
#'   \item{mass}{Named numeric vector of achieved mass for each level.}
#'   \item{probs}{Numeric vector of target masses in (0, 1].}
#' }
#' @export
hdi <- function(x, ...) {
  UseMethod("hdi")
}

#' @rdname hdi
#' @param probs Numeric vector of target mass levels. Values in \eqn{(0, 1]}
#'   or \eqn{[0, 100]} are accepted. Default is \code{c(0.5, 0.99)}.
#' @export
hdi.default <- function(x, probs = c(0.5, 0.99), ...) {
  extra_args <- list(...)
  if (length(extra_args) > 0L) {
    cli::cli_abort("`hdi()` does not accept additional arguments in `...`.")
  }
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  x <- x[is.finite(x)]
  if (length(x) < 2L) {
    cli::cli_abort("`x` must contain at least two finite numeric values.")
  }

  probs <- normalize_quantile_probs(probs)
  if (is.null(probs) || any(probs <= 0 | probs > 1)) {
    cli::cli_abort("`probs` must be numeric values in (0, 1] or [0, 100].")
  }
  probs <- sort(unique(probs))

  x_sorted <- sort(x)
  n <- length(x_sorted)

  compute_hdi <- function(target_mass) {
    m <- ceiling(target_mass * n)
    if (m < 1L) {
      cli::cli_abort("`probs` values must be in (0, 1].")
    }
    if (m >= n) {
      return(c(lower = x_sorted[1], upper = x_sorted[n], mass = 1))
    }
    n_windows <- n - m + 1L
    widths <- x_sorted[m:n] - x_sorted[1:n_windows]
    idx <- which.min(widths)
    c(
      lower = x_sorted[idx],
      upper = x_sorted[idx + m - 1L],
      mass = m / n
    )
  }

  res <- lapply(probs, compute_hdi)
  level_names <- paste0("level_", format(probs * 100, trim = TRUE))
  names(res) <- level_names

  intervals <- lapply(res, function(x) {
    matrix(c(x["lower"], x["upper"]), nrow = 1L, dimnames = list(NULL, c("lower", "upper")))
  })
  cutoffs <- stats::setNames(rep(NA_real_, length(level_names)), level_names)
  masses <- stats::setNames(vapply(res, function(x) x["mass"], numeric(1)), level_names)

  structure(
    list(
      intervals = intervals,
      mode = stats::median(x_sorted),
      cutoff = cutoffs,
      mass = masses,
      probs = probs
    ),
    class = "koma_hdi"
  )
}

#' Summary for koma_hdi Objects
#'
#' Prints HDI intervals for a single numeric sample.
#'
#' @param object A `koma_hdi` object.
#' @param ... Unused.
#' @param digits Number of digits to round numeric values. Default is 3.
#'
#' @return Invisibly returns `object`.
#' @export
summary.koma_hdi <- function(object, ..., digits = 3) {
  if (!inherits(object, "koma_hdi")) {
    cli::cli_abort("`object` must be a koma_hdi.")
  }

  format_num <- function(x) {
    format(round(x, digits), trim = TRUE)
  }

  format_intervals <- function(mat) {
    if (is.null(mat) || nrow(mat) == 0L) {
      return("")
    }
    pieces <- apply(mat, 1, function(x) {
      sprintf("[%s; %s]", format_num(x[1]), format_num(x[2]))
    })
    paste(pieces, collapse = "; ")
  }

  level_names <- names(object$intervals)
  if (is.null(level_names)) {
    level_names <- paste0("level_", format(object$probs * 100, trim = TRUE))
  }
  level_labels <- paste0(format(object$probs * 100, trim = TRUE), "%")
  if (length(level_labels) != length(level_names)) {
    level_labels <- level_names
  }

  header <- strrep("=", max(10L, max(nchar(level_names))))
  lines <- c(header)

  for (lvl in level_names) {
    lines <- c(lines, lvl, strrep("-", nchar(lvl)))
    lines <- c(
      lines,
      paste0("Median: ", format_num(object$mode)),
      paste0("Intervals: ", format_intervals(object$intervals[[lvl]])),
      ""
    )
  }

  lines <- c(lines, header)
  cat(paste(lines, collapse = "\n"), "\n", sep = "")

  note <- "Median, [HDI]"
  cat(note, "\n")

  invisible(object)
}

#' Print method for koma_hdi objects
#'
#' Delegates to `summary()` for console output.
#'
#' @param x A `koma_hdi` object.
#' @param ... Additional arguments forwarded to `summary.koma_hdi()`.
#'
#' @return Invisibly returns `x`.
#' @export
print.koma_hdi <- function(x, ...) {
  summary(x, ...)
  invisible(x)
}

#' Highest Density Intervals for koma_estimate Objects
#'
#' Computes highest density intervals (HDIs) for coefficient draws from a
#' `koma_estimate` object.
#'
#' @param x A `koma_estimate` object.
#' @param variables Optional character vector of endogenous variables to
#'   include. Defaults to all variables in `object$estimates`.
#' @inheritParams hdi.default
#' @param include_sigma Logical. If TRUE, compute HDIs for the error variance
#'   parameter (omega). Default is FALSE.
#'
#' @return A list with class \code{c("koma_estimate_hdi", "koma_hdi")}
#' containing HDI intervals per coefficient.
#' @export
hdi.koma_estimate <- function(x,
                              variables = NULL,
                              probs = c(0.5, 0.99),
                              include_sigma = FALSE,
                              ...) {
  if (!inherits(x, "koma_estimate")) {
    cli::cli_abort("`x` must be a koma_estimate.")
  }

  if (is.null(variables)) {
    variables <- names(x$estimates)
  }

  if (!length(variables)) {
    cli::cli_abort("`variables` must contain at least one variable name.")
  }

  missing_vars <- setdiff(variables, names(x$estimates))
  if (length(missing_vars) > 0L) {
    cli::cli_abort(c(
      "The following variables are not contained in estimates:",
      ">" = missing_vars
    ))
  }

  sys_eq <- x$sys_eq

  draws_to_matrix <- function(draws) {
    if (length(draws) == 0L) {
      return(NULL)
    }
    if (length(draws[[1]]) == 1) {
      mat <- t(as.matrix(simplify2array(draws)))
    } else {
      mat <- simplify2array(draws)
    }
    if (length(dim(mat)) == 3) {
      mat <- mat[, 1, ]
    }
    mat
  }

  compute_hdi <- function(draws, variable, param_name, coef_name) {
    draws <- draws[is.finite(draws)]
    if (length(draws) < 2L) {
      cli::cli_abort(c(
        "Not enough finite draws to compute HDI.",
        "i" = "variable: {variable}",
        "i" = "parameter: {param_name}",
        "i" = "coefficient: {coef_name}"
      ))
    }
    hdi.default(draws, probs = probs, ...)
  }

  interval_out <- list()
  mode_out <- list()
  cutoff_out <- list()
  mass_out <- list()

  for (variable in variables) {
    jx <- match(variable, sys_eq$endogenous_variables)
    if (is.na(jx)) {
      cli::cli_abort(c(
        "Variable not found in endogenous variables: ",
        ">" = variable
      ))
    }

    estimate_jx <- x$estimates[[variable]]
    idx_beta <- which(sys_eq$character_beta_matrix[, jx] != 0)
    idx_gamma <- which(
      sys_eq$character_gamma_matrix[, jx] != 0 &
        sys_eq$character_gamma_matrix[, jx] != 1
    )

    beta_names <- sys_eq$total_exogenous_variables[idx_beta]
    gamma_names <- sys_eq$endogenous_variables[idx_gamma]

    interval_out[[variable]] <- list()
    mode_out[[variable]] <- list()
    cutoff_out[[variable]] <- list()
    mass_out[[variable]] <- list()

    if (length(idx_beta)) {
      beta_mat <- draws_to_matrix(estimate_jx$beta_jw)
      if (!is.null(beta_mat)) {
        interval_out[[variable]]$beta <- list()
        mode_out[[variable]]$beta <- list()
        cutoff_out[[variable]]$beta <- list()
        mass_out[[variable]]$beta <- list()

        for (i in seq_along(beta_names)) {
          coef_name <- beta_names[i]
          coef_draws <- beta_mat[i, ]
          hdi_res <- compute_hdi(coef_draws, variable, "beta", coef_name)
          interval_out[[variable]]$beta[[coef_name]] <- hdi_res$intervals
          mode_out[[variable]]$beta[[coef_name]] <- hdi_res$mode
          cutoff_out[[variable]]$beta[[coef_name]] <- hdi_res$cutoff
          mass_out[[variable]]$beta[[coef_name]] <- hdi_res$mass
        }
      }
    }

    if (length(idx_gamma)) {
      gamma_mat <- draws_to_matrix(estimate_jx$gamma_jw)
      if (!is.null(gamma_mat)) {
        interval_out[[variable]]$gamma <- list()
        mode_out[[variable]]$gamma <- list()
        cutoff_out[[variable]]$gamma <- list()
        mass_out[[variable]]$gamma <- list()

        for (i in seq_along(gamma_names)) {
          coef_name <- gamma_names[i]
          coef_draws <- gamma_mat[i, ]
          hdi_res <- compute_hdi(coef_draws, variable, "gamma", coef_name)
          interval_out[[variable]]$gamma[[coef_name]] <- hdi_res$intervals
          mode_out[[variable]]$gamma[[coef_name]] <- hdi_res$mode
          cutoff_out[[variable]]$gamma[[coef_name]] <- hdi_res$cutoff
          mass_out[[variable]]$gamma[[coef_name]] <- hdi_res$mass
        }
      }
    }

    if (isTRUE(include_sigma)) {
      omega_mat <- draws_to_matrix(estimate_jx$omega_tilde_jw)
      if (!is.null(omega_mat)) {
        omega_draws <- omega_mat[1, ]
        hdi_res <- compute_hdi(omega_draws, variable, "sigma", "omega")
        interval_out[[variable]]$sigma <- list(omega = hdi_res$intervals)
        mode_out[[variable]]$sigma <- list(omega = hdi_res$mode)
        cutoff_out[[variable]]$sigma <- list(omega = hdi_res$cutoff)
        mass_out[[variable]]$sigma <- list(omega = hdi_res$mass)
      }
    }
  }

  structure(
    list(
      intervals = interval_out,
      mode = mode_out,
      cutoff = cutoff_out,
      mass = mass_out,
      probs = normalize_quantile_probs(probs),
      include_sigma = include_sigma,
      dates = x$dates,
      sys_eq = x$sys_eq
    ),
    class = c("koma_estimate_hdi", "koma_hdi")
  )
}

#' Summary for koma_estimate_hdi Objects
#'
#' Prints HDI intervals for coefficients in a table per equation.
#'
#' @param object A `koma_estimate_hdi` object.
#' @param ... Unused.
#' @param variables Optional character vector to filter variables.
#' @param params Optional character vector to filter parameters (e.g.,
#'   "beta", "gamma", "sigma").
#' @param digits Number of digits to round numeric values. Default is 3.
#'
#' @return Invisibly returns `object`.
#' @export
summary.koma_estimate_hdi <- function(object,
                                      ...,
                                      variables = NULL,
                                      params = NULL,
                                      digits = 3) {
  if (!inherits(object, "koma_estimate_hdi")) {
    cli::cli_abort("`object` must be a koma_estimate_hdi.")
  }
  summary_interval_table(
    object,
    variables = variables,
    params = params,
    digits = digits,
    center_label = "Median",
    interval_label = "HDI"
  )
}

#' Highest Density Intervals for koma_forecast Objects
#'
#' Computes highest density intervals (HDIs) for predictive draws in a
#' `koma_forecast` object.
#'
#' @param x A `koma_forecast` object.
#' @param variables Optional character vector of endogenous variables to
#'   include. Defaults to all variables in `x$forecasts`.
#' @inheritParams hdi.default
#'
#' @return A list with class \code{c("koma_forecast_hdi", "koma_hdi")} containing
#' HDI intervals per variable and horizon, along with per-horizon centers and
#' achieved masses.
#' @export
hdi.koma_forecast <- function(x,
                              variables = NULL,
                              probs = c(0.5, 0.99),
                              ...) {
  if (!inherits(x, "koma_forecast")) {
    cli::cli_abort("`x` must be a koma_forecast.")
  }
  if (is.null(x$forecasts) || !length(x$forecasts)) {
    cli::cli_abort(c(
      "HDIs require predictive draws in `x$forecasts`.",
      "i" = "Re-run forecast with `options$approximate = FALSE`."
    ))
  }

  forecasts <- x$forecasts
  first_draw <- forecasts[[1]]
  if (!inherits(first_draw, "ts")) {
    cli::cli_abort("`x$forecasts` must contain `ts` objects.")
  }

  all_vars <- colnames(first_draw)
  if (is.null(variables)) {
    variables <- all_vars
  }
  if (!length(variables)) {
    cli::cli_abort("`variables` must contain at least one variable name.")
  }
  missing_vars <- setdiff(variables, all_vars)
  if (length(missing_vars) > 0L) {
    cli::cli_abort(c(
      "The following variables are not contained in forecasts:",
      ">" = missing_vars
    ))
  }

  probs <- normalize_quantile_probs(probs)
  if (is.null(probs) || any(probs <= 0 | probs > 1)) {
    cli::cli_abort("`probs` must be numeric values in (0, 1] or [0, 100].")
  }
  probs <- sort(unique(probs))
  level_names <- paste0("level_", format(probs * 100, trim = TRUE))

  start_forecast <- stats::start(first_draw)
  freq <- stats::frequency(first_draw)
  horizon <- nrow(first_draw)

  intervals <- stats::setNames(vector("list", length(level_names)), level_names)
  masses <- stats::setNames(vector("list", length(level_names)), level_names)
  cutoffs <- stats::setNames(vector("list", length(level_names)), level_names)
  modes <- list()

  compute_hdi <- function(draws) {
    hdi.default(draws, probs = probs, ...)
  }

  for (var in variables) {
    mode_vec <- numeric(horizon)
    mass_by_level <- lapply(level_names, function(x) numeric(horizon))
    names(mass_by_level) <- level_names
    lower_by_level <- lapply(level_names, function(x) numeric(horizon))
    names(lower_by_level) <- level_names
    upper_by_level <- lapply(level_names, function(x) numeric(horizon))
    names(upper_by_level) <- level_names

    for (h in seq_len(horizon)) {
      draws <- vapply(forecasts, function(fx) fx[h, var], numeric(1))
      hdi_res <- compute_hdi(draws)
      mode_vec[h] <- hdi_res$mode

      for (lvl in level_names) {
        interval <- hdi_res$intervals[[lvl]]
        lower_by_level[[lvl]][h] <- interval[1, "lower"]
        upper_by_level[[lvl]][h] <- interval[1, "upper"]
        mass_by_level[[lvl]][h] <- hdi_res$mass[[lvl]]
      }
    }

    modes[[var]] <- stats::ts(mode_vec, start = start_forecast, frequency = freq)

    for (lvl in level_names) {
      intervals[[lvl]][[var]] <- list(
        lower = stats::ts(lower_by_level[[lvl]], start = start_forecast, frequency = freq),
        upper = stats::ts(upper_by_level[[lvl]], start = start_forecast, frequency = freq)
      )
      masses[[lvl]][[var]] <- stats::ts(
        mass_by_level[[lvl]],
        start = start_forecast,
        frequency = freq
      )
      cutoffs[[lvl]][[var]] <- stats::ts(
        rep(NA_real_, horizon),
        start = start_forecast,
        frequency = freq
      )
    }
  }

  structure(
    list(
      intervals = intervals,
      mode = modes,
      cutoff = cutoffs,
      mass = masses,
      probs = probs
    ),
    class = c("koma_forecast_hdi", "koma_hdi")
  )
}

#' Summary for koma_forecast_hdi Objects
#'
#' Prints HDI intervals for forecast horizons.
#'
#' @param object A `koma_forecast_hdi` object.
#' @param ... Unused.
#' @param variables Optional character vector to filter variables.
#' @param horizon Optional numeric or character vector selecting forecast horizon.
#' @param digits Number of digits to round numeric values. Default is 3.
#'
#' @return Invisibly returns `object`.
#' @export
summary.koma_forecast_hdi <- function(object,
                                      ...,
                                      variables = NULL,
                                      horizon = NULL,
                                      digits = 3) {
  if (!inherits(object, "koma_forecast_hdi")) {
    cli::cli_abort("`object` must be a koma_forecast_hdi.")
  }

  format_num <- function(x) {
    format(round(x, digits), trim = TRUE)
  }

  format_interval <- function(lower, upper) {
    sprintf("[%s; %s]", format_num(lower), format_num(upper))
  }

  resolve_horizon <- function(display_labels, raw_labels = NULL) {
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

  level_names <- names(object$intervals)
  if (is.null(level_names)) {
    level_names <- paste0("level_", format(object$probs * 100, trim = TRUE))
  }
  level_labels <- paste0(format(object$probs * 100, trim = TRUE), "%")
  if (length(level_labels) != length(level_names)) {
    level_labels <- level_names
  }

  if (is.null(variables)) {
    variables <- names(object$mode)
  }
  if (!length(variables)) {
    cli::cli_abort("`variables` must contain at least one variable name.")
  }
  missing_vars <- setdiff(variables, names(object$mode))
  if (length(missing_vars) > 0L) {
    cli::cli_abort(c(
      "Variable not found in forecasts:",
      ">" = missing_vars
    ))
  }

  for (i in seq_along(variables)) {
    variable <- variables[[i]]
    mode_ts <- object$mode[[variable]]
    raw_labels <- format(stats::time(mode_ts), trim = TRUE)
    display_labels <- format_ts_time(mode_ts)
    idx <- resolve_horizon(display_labels, raw_labels)

    rows <- lapply(idx, function(i) {
      intervals <- vapply(level_names, function(lvl) {
        level <- object$intervals[[lvl]][[variable]]
        format_interval(level$lower[i], level$upper[i])
      }, character(1))
      c(display_labels[i], format_num(mode_ts[i]), intervals)
    })
    if (!length(rows)) {
      next
    }
    row_mat <- do.call(rbind, rows)
    col_names <- c(variable, "Median", level_labels)
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

  note <- "Median, [HDI]"
  cat(note, "\n")

  invisible(object)
}
