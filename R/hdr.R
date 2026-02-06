#' Highest Density Regions from a Kernel Density Estimate
#'
#' Computes highest density regions (HDRs) for a numeric sample using a kernel
#' density estimate. For multimodal densities, the HDR can consist of multiple
#' disjoint intervals.
#'
#' The Monte Carlo integration option follows the approach described by
#' Hyndman (1996) for computing HDRs from an estimated density.
#'
#'
#'
#' @return A list with class \code{"koma_hdr"} containing:
#' \describe{
#'   \item{intervals}{Named list of matrices with columns \code{lower} and
#'   \code{upper}, one matrix per \code{probs} level.}
#'   \item{mode}{Location of the KDE mode.}
#'   \item{density}{The density object returned by \code{stats::density}.}
#'   \item{cutoff}{Named numeric vector of density cutoffs for each level.}
#'   \item{mass}{Named numeric vector of achieved mass for each level.
#'     For `integration = "grid"`, this is the area under the KDE above the
#'     cutoff (grid-based). For `integration = "monte_carlo"`, this is the
#'     Monte Carlo coverage, i.e. the fraction of sampled points with
#'     density above the cutoff.}
#'   \item{probs}{Numeric vector of target masses in (0, 1].}
#' }
#' @references
#' Hyndman, R. J. (1996). Computing and graphing highest density regions.
#' The American Statistician, 50(2), 120–126. \doi{10.2307/2684423}
#' @export
hdr <- function(x, ...) {
  UseMethod("hdr")
}

#' @rdname hdr
#' @param x A numeric vector of draws.
#' @param probs Numeric vector of target mass levels. Values in \eqn{(0, 1]}
#'   or \eqn{[0, 100]} are accepted. Default is \code{c(0.5, 0.99)}.
#' @param n_grid Number of grid points for the KDE. Default is 4096.
#' @param integration Integration approach for the HDR cutoff. Use `"grid"`
#'   for grid-based integration or `"monte_carlo"` for Monte Carlo integration.
#'   Default is `"monte_carlo"`.
#' @param mc_use_observed Logical. If TRUE, Monte Carlo integration uses the
#'   observed draws. This can be a reasonable approximation for large `x`.
#'   If FALSE, draws are sampled from the KDE mixture; only implemented for
#'   `kernel = "gaussian"`.
#' @param mc_draws Optional integer number of draws for Monte Carlo sampling
#'   when `mc_use_observed = FALSE`. Defaults to `max(length(x), 2000)`.
#' @param mc_quantile_type Quantile type passed to `stats::quantile` for the
#'   Monte Carlo cutoff. Type 1 matches the order-statistic construction in
#'   Hyndman (1996), while type 7 (the default) interpolates between adjacent
#'   order statistics for a smoother, lower-variance cutoff that is
#'   asymptotically equivalent. Default is 7.
#' @param bw Bandwidth for \code{\link[stats]{density}}. Default is "nrd0".
#' @param adjust Bandwidth adjustment factor for
#'   \code{\link[stats]{density}}. Default is 1.
#' @param kernel Kernel for \code{\link[stats]{density}}. Default is "gaussian".
#' @param ... Additional arguments forwarded to
#'   \code{\link[stats]{density}}. Do not pass \code{n}, \code{bw},
#'   \code{adjust}, or \code{kernel} here.
#' @export
hdr.default <- function(x,
                        probs = c(0.5, 0.99),
                        n_grid = 4096,
                        integration = c("monte_carlo", "grid"),
                        mc_use_observed = FALSE,
                        mc_draws = NULL,
                        mc_quantile_type = 7,
                        bw = "nrd0",
                        adjust = 1,
                        kernel = "gaussian",
                        ...) {
  extra_args <- list(...)
  invalid <- intersect(names(extra_args), c("n", "bw", "adjust", "kernel"))
  if (length(invalid) > 0L) {
    cli::cli_abort(c(
      "Arguments in `...` must not include {.val {invalid}}.",
      "i" = "Use the dedicated function arguments instead."
    ))
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

  integration <- match.arg(integration)

  d <- stats::density(
    x = x,
    n = n_grid,
    bw = bw,
    adjust = adjust,
    kernel = kernel,
    ...
  )

  grid_hdr <- function(target_mass) {
    dx <- d$x[2] - d$x[1]
    ord <- order(d$y, decreasing = TRUE)
    cum_mass <- cumsum(d$y[ord]) * dx
    k <- which(cum_mass >= target_mass)[1]
    if (is.na(k)) {
      k <- length(cum_mass)
    }
    cutoff <- d$y[ord][k]

    inside <- d$y >= cutoff
    idx <- which(diff(c(FALSE, inside, FALSE)) != 0)

    starts <- idx[seq(1, length(idx), by = 2)]
    ends <- idx[seq(2, length(idx), by = 2)] - 1

    intervals <- cbind(
      lower = d$x[starts],
      upper = d$x[ends]
    )

    achieved <- sum(d$y[inside]) * dx

    list(
      intervals = intervals,
      cutoff = cutoff,
      mass = achieved
    )
  }

  mc_hdr <- function(target_mass) {
    if (target_mass <= 0 || target_mass > 1) {
      cli::cli_abort("`probs` values must be in (0, 1].")
    }
    alpha <- 1 - target_mass

    fhat_at <- function(z) {
      stats::approx(d$x, d$y, xout = z, rule = 2)$y
    }

    if (isTRUE(mc_use_observed)) {
      x_star <- x
    } else {
      if (!identical(kernel, "gaussian")) {
        cli::cli_abort("Sampling from the KDE is only implemented for kernel = \"gaussian\".")
      }
      mc_draws_val <- if (is.null(mc_draws)) {
        max(length(x), 2000L)
      } else {
        as.integer(mc_draws)
      }
      if (!is.finite(mc_draws_val) || mc_draws_val < 2L) {
        cli::cli_abort("`mc_draws` must be an integer >= 2.")
      }
      h <- d$bw
      x_star <- sample(x, size = mc_draws_val, replace = TRUE) +
        stats::rnorm(mc_draws_val, mean = 0, sd = h)
    }

    y <- fhat_at(x_star)
    # MC cutoff quantile: type = 1 matches Hyndman (1996) order-statistic
    # construction (up to floor/ceiling convention). type = 7 is R's default
    # interpolation, smoother and lower-variance while asymptotically
    # equivalent. Use type = 1 to reproduce Hyndman's definition.
    cutoff <- as.numeric(
      stats::quantile(y, probs = alpha, type = mc_quantile_type, names = FALSE)
    )

    inside <- d$y >= cutoff
    idx <- which(diff(c(FALSE, inside, FALSE)) != 0)
    starts <- idx[seq(1, length(idx), by = 2)]
    ends <- idx[seq(2, length(idx), by = 2)] - 1

    intervals <- cbind(
      lower = d$x[starts],
      upper = d$x[ends]
    )

    coverage_mc <- mean(y >= cutoff)

    list(
      intervals = intervals,
      cutoff = cutoff,
      mass = coverage_mc
    )
  }

  res <- if (identical(integration, "grid")) {
    lapply(probs, grid_hdr)
  } else {
    lapply(probs, mc_hdr)
  }
  level_names <- paste0("level_", format(probs * 100, trim = TRUE))
  names(res) <- level_names

  intervals <- lapply(res, function(x) x$intervals)
  cutoffs <- stats::setNames(vapply(res, function(x) x$cutoff, numeric(1)), level_names)
  masses <- stats::setNames(vapply(res, function(x) x$mass, numeric(1)), level_names)

  mode_hat <- d$x[which.max(d$y)]

  structure(
    list(
      intervals = intervals,
      mode = mode_hat,
      density = d,
      cutoff = cutoffs,
      mass = masses,
      probs = probs
    ),
    class = "koma_hdr"
  )
}

#' Summary for koma_hdr Objects
#'
#' Prints HDR intervals for a single numeric sample.
#'
#' @param object A `koma_hdr` object.
#' @param ... Unused.
#' @param digits Number of digits to round numeric values. Default is 3.
#'
#' @return Invisibly returns `object`.
#' @export
summary.koma_hdr <- function(object, ..., digits = 3) {
  if (!inherits(object, "koma_hdr")) {
    cli::cli_abort("`object` must be a koma_hdr.")
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

  header <- strrep("=", max(10L, max(nchar(level_names))))
  lines <- c(header)

  for (lvl in level_names) {
    lines <- c(lines, lvl, strrep("-", nchar(lvl)))
    lines <- c(
      lines,
      paste0("Mode: ", format_num(object$mode)),
      paste0("Intervals: ", format_intervals(object$intervals[[lvl]])),
      ""
    )
  }

  lines <- c(lines, header)
  cat(paste(lines, collapse = "\n"), "\n", sep = "")

  invisible(object)
}

#' Print method for koma_hdr objects
#'
#' Delegates to `summary()` for console output.
#'
#' @param x A `koma_hdr` object.
#' @param ... Additional arguments forwarded to `summary.koma_hdr()`.
#'
#' @return Invisibly returns `x`.
#' @export
print.koma_hdr <- function(x, ...) {
  summary(x, ...)
  invisible(x)
}

.hdr_outliers <- function(draws, intervals, probs) {
  if (is.null(draws) || !length(draws)) {
    return(numeric(0))
  }
  if (is.null(intervals) || !length(intervals)) {
    return(draws)
  }
  max_prob <- max(normalize_quantile_probs(probs))
  level_name <- paste0("level_", format(max_prob * 100, trim = TRUE))
  mat <- intervals[[level_name]]
  if (is.null(mat) || nrow(mat) == 0L) {
    return(draws)
  }
  inside <- rep(FALSE, length(draws))
  for (i in seq_len(nrow(mat))) {
    inside <- inside | (draws >= mat[i, "lower"] & draws <= mat[i, "upper"])
  }
  draws[!inside]
}

#' Highest Density Regions for koma_estimate Objects
#'
#' Computes highest density regions (HDRs) for coefficient draws from a
#' `koma_estimate` object.
#'
#' @param x A `koma_estimate` object.
#' @param variables Optional character vector of endogenous variables to
#'   include. Defaults to all variables in `object$estimates`.
#' @inheritParams hdr.default
#' @param include_sigma Logical. If TRUE, compute HDRs for the error variance
#'   parameter (omega). Default is FALSE.
#' @param ... Additional arguments forwarded to \code{\link[stats]{density}}.
#'
#' @return A list with class \code{c("koma_estimate_hdr", "koma_hdr")}
#' containing HDR intervals per coefficient. The object also includes an
#' `outliers` list with draws outside the largest HDR level.
#' @export
hdr.koma_estimate <- function(x,
                              variables = NULL,
                              probs = c(0.5, 0.99),
                              n_grid = 4096,
                              integration = c("monte_carlo", "grid"),
                              mc_use_observed = FALSE,
                              mc_draws = NULL,
                              mc_quantile_type = 7,
                              bw = "nrd0",
                              adjust = 1,
                              kernel = "gaussian",
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

  compute_hdr <- function(draws, variable, param_name, coef_name) {
    draws <- draws[is.finite(draws)]
    if (length(draws) < 2L) {
      cli::cli_abort(c(
        "Not enough finite draws to compute HDR.",
        "i" = "variable: {variable}",
        "i" = "parameter: {param_name}",
        "i" = "coefficient: {coef_name}"
      ))
    }
    hdr.default(
      draws,
      probs = probs,
      n_grid = n_grid,
      integration = integration,
      mc_use_observed = mc_use_observed,
      mc_draws = mc_draws,
      mc_quantile_type = mc_quantile_type,
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      ...
    )
  }

  interval_out <- list()
  mode_out <- list()
  cutoff_out <- list()
  mass_out <- list()
  outliers_out <- list()

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
    outliers_out[[variable]] <- list()

    if (length(idx_beta)) {
      beta_mat <- draws_to_matrix(estimate_jx$beta_jw)
      if (!is.null(beta_mat)) {
        interval_out[[variable]]$beta <- list()
        mode_out[[variable]]$beta <- list()
        cutoff_out[[variable]]$beta <- list()
        mass_out[[variable]]$beta <- list()
        outliers_out[[variable]]$beta <- list()

        for (i in seq_along(beta_names)) {
          coef_name <- beta_names[i]
          coef_draws <- beta_mat[i, ]
          hdr_res <- compute_hdr(coef_draws, variable, "beta", coef_name)
          interval_out[[variable]]$beta[[coef_name]] <- hdr_res$intervals
          mode_out[[variable]]$beta[[coef_name]] <- hdr_res$mode
          cutoff_out[[variable]]$beta[[coef_name]] <- hdr_res$cutoff
          mass_out[[variable]]$beta[[coef_name]] <- hdr_res$mass
          outliers_out[[variable]]$beta[[coef_name]] <- .hdr_outliers(
            coef_draws,
            hdr_res$intervals,
            probs
          )
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
        outliers_out[[variable]]$gamma <- list()

        for (i in seq_along(gamma_names)) {
          coef_name <- gamma_names[i]
          coef_draws <- gamma_mat[i, ]
          hdr_res <- compute_hdr(coef_draws, variable, "gamma", coef_name)
          interval_out[[variable]]$gamma[[coef_name]] <- hdr_res$intervals
          mode_out[[variable]]$gamma[[coef_name]] <- hdr_res$mode
          cutoff_out[[variable]]$gamma[[coef_name]] <- hdr_res$cutoff
          mass_out[[variable]]$gamma[[coef_name]] <- hdr_res$mass
          outliers_out[[variable]]$gamma[[coef_name]] <- .hdr_outliers(
            coef_draws,
            hdr_res$intervals,
            probs
          )
        }
      }
    }

    if (isTRUE(include_sigma)) {
      omega_mat <- draws_to_matrix(estimate_jx$omega_tilde_jw)
      if (!is.null(omega_mat)) {
        omega_draws <- omega_mat[1, ]
        hdr_res <- compute_hdr(omega_draws, variable, "sigma", "omega")
        interval_out[[variable]]$sigma <- list(omega = hdr_res$intervals)
        mode_out[[variable]]$sigma <- list(omega = hdr_res$mode)
        cutoff_out[[variable]]$sigma <- list(omega = hdr_res$cutoff)
        mass_out[[variable]]$sigma <- list(omega = hdr_res$mass)
        outliers_out[[variable]]$sigma <- list(
          omega = .hdr_outliers(omega_draws, hdr_res$intervals, probs)
        )
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
      outliers = outliers_out,
      dates = x$dates,
      sys_eq = x$sys_eq
    ),
    class = c("koma_estimate_hdr", "koma_hdr")
  )
}

#' Highest Density Regions for koma_forecast Objects
#'
#' Computes highest density regions (HDRs) for predictive draws in a
#' `koma_forecast` object.
#'
#' @param x A `koma_forecast` object.
#' @param variables Optional character vector of endogenous variables to
#'   include. Defaults to all variables in `x$forecasts`.
#' @inheritParams hdr.default
#'
#' @return A list with class \code{c("koma_forecast_hdr", "koma_hdr")} containing
#' HDR intervals per variable and horizon. The object also includes per-horizon
#' modes, cutoff levels, and achieved masses.
#' @export
hdr.koma_forecast <- function(x,
                              variables = NULL,
                              probs = c(0.5, 0.99),
                              n_grid = 4096,
                              integration = c("monte_carlo", "grid"),
                              mc_use_observed = FALSE,
                              mc_draws = NULL,
                              mc_quantile_type = 7,
                              bw = "nrd0",
                              adjust = 1,
                              kernel = "gaussian",
                              ...) {
  if (!inherits(x, "koma_forecast")) {
    cli::cli_abort("`x` must be a koma_forecast.")
  }
  if (is.null(x$forecasts) || !length(x$forecasts)) {
    cli::cli_abort(c(
      "HDRs require predictive draws in `x$forecasts`.",
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
  time_labels <- format(stats::time(first_draw), trim = TRUE)

  intervals <- stats::setNames(vector("list", length(level_names)), level_names)
  cutoffs <- stats::setNames(vector("list", length(level_names)), level_names)
  masses <- stats::setNames(vector("list", length(level_names)), level_names)
  modes <- list()

  compute_hdr <- function(draws) {
    hdr.default(
      draws,
      probs = probs,
      n_grid = n_grid,
      integration = integration,
      mc_use_observed = mc_use_observed,
      mc_draws = mc_draws,
      mc_quantile_type = mc_quantile_type,
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      ...
    )
  }

  for (var in variables) {
    mode_vec <- numeric(horizon)
    cutoff_by_level <- lapply(level_names, function(x) numeric(horizon))
    names(cutoff_by_level) <- level_names
    mass_by_level <- lapply(level_names, function(x) numeric(horizon))
    names(mass_by_level) <- level_names

    interval_by_level <- lapply(level_names, function(x) vector("list", horizon))
    names(interval_by_level) <- level_names

    for (h in seq_len(horizon)) {
      draws <- vapply(forecasts, function(fx) fx[h, var], numeric(1))
      hdr_res <- compute_hdr(draws)
      mode_vec[h] <- hdr_res$mode

      for (lvl in level_names) {
        interval_by_level[[lvl]][[h]] <- hdr_res$intervals[[lvl]]
        cutoff_by_level[[lvl]][h] <- hdr_res$cutoff[[lvl]]
        mass_by_level[[lvl]][h] <- hdr_res$mass[[lvl]]
      }
    }

    modes[[var]] <- stats::ts(mode_vec, start = start_forecast, frequency = freq)

    for (lvl in level_names) {
      intervals[[lvl]][[var]] <- stats::setNames(
        interval_by_level[[lvl]],
        time_labels
      )
      cutoffs[[lvl]][[var]] <- stats::ts(
        cutoff_by_level[[lvl]],
        start = start_forecast,
        frequency = freq
      )
      masses[[lvl]][[var]] <- stats::ts(
        mass_by_level[[lvl]],
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
    class = c("koma_forecast_hdr", "koma_hdr")
  )
}

#' Summary for koma_forecast_hdr Objects
#'
#' Prints HDR intervals for forecast horizons.
#'
#' @param object A `koma_forecast_hdr` object.
#' @param ... Unused.
#' @param variables Optional character vector to filter variables.
#' @param horizon Optional numeric or character vector selecting forecast horizon.
#' @param digits Number of digits to round numeric values. Default is 3.
#'
#' @return Invisibly returns `object`.
#' @export
summary.koma_forecast_hdr <- function(object,
                                      ...,
                                      variables = NULL,
                                      horizon = NULL,
                                      digits = 3) {
  if (!inherits(object, "koma_forecast_hdr")) {
    cli::cli_abort("`object` must be a koma_forecast_hdr.")
  }

  format_intervals <- function(mat) {
    if (is.null(mat) || nrow(mat) == 0L) {
      return("")
    }
    pieces <- apply(mat, 1, function(x) {
      sprintf(
        "[%s; %s]",
        summary_forecast_format_num(x[1], digits),
        summary_forecast_format_num(x[2], digits)
      )
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
    raw_labels <- names(object$intervals[[level_names[1]]][[variable]])
    if (is.null(raw_labels)) {
      raw_labels <- format(stats::time(mode_ts), trim = TRUE)
    }
    display_labels <- format_ts_time(mode_ts)
    idx <- summary_forecast_resolve_horizon(horizon, display_labels, raw_labels)

    rows <- lapply(idx, function(i) {
      time_label <- display_labels[i]
      intervals <- vapply(level_names, function(lvl) {
        format_intervals(object$intervals[[lvl]][[variable]][[i]])
      }, character(1))
      c(time_label, summary_forecast_format_num(mode_ts[i], digits), intervals)
    })
    if (!length(rows)) {
      next
    }
    row_mat <- do.call(rbind, rows)
    col_names <- c(variable, "Mode", level_labels)
    summary_forecast_write_table(col_names, row_mat)
  }

  note <- "Mode, [HDR]"
  cat(note, "\n")

  invisible(object)
}

#' Summary for koma_estimate_hdr Objects
#'
#' Prints HDR intervals for coefficients in a table per equation.
#'
#' @param object A `koma_estimate_hdr` object.
#' @param ... Unused.
#' @param variables Optional character vector to filter variables.
#' @param params Optional character vector to filter parameters (e.g.,
#'   "beta", "gamma", "sigma").
#' @param digits Number of digits to round numeric values. Default is 3.
#'
#' @return Invisibly returns `object`.
#' @export
summary.koma_estimate_hdr <- function(object,
                                      ...,
                                      variables = NULL,
                                      params = NULL,
                                      digits = 3) {
  if (!inherits(object, "koma_estimate_hdr")) {
    cli::cli_abort("`object` must be a koma_estimate_hdr.")
  }
  summary_interval_table(
    object,
    variables = variables,
    params = params,
    digits = digits,
    center_label = "Mode",
    interval_label = "HDR"
  )
}
