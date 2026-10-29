#' Residuals and Fitted Values of an Estimated SEM
#'
#' Returns the in-sample residuals or fitted values of the stochastic equations
#' of a `koma_estimate` object over the estimation period. Identities have no
#' residual and are left out.
#'
#' The residual of an equation is its structural residual, e.g. for
#' \code{consumption ~ gdp + consumption.L(1)} it is consumption minus the
#' contribution of gdp, the lag and the constant. The fitted value is the data
#' minus the residual. Both are in rates, like the data the model is
#' estimated on.
#'
#' @param object A `koma_estimate` object (\code{\link{estimate}}).
#' @param ... Not used.
#' @param central_tendency \code{"mean"} (default) or \code{"median"}: the
#' coefficients at which the residuals are computed. Since the residuals are
#' linear in the coefficients, at the mean they equal the mean of the residuals
#' over the draws.
#' @param probs Optional numeric vector of probabilities. If given, quantiles of
#' the residuals over the draws are returned as well.
#'
#' @return A named list with one element per stochastic equation. Without
#' \code{probs}, each element is a \code{ts}. With \code{probs}, each element is
#' a multivariate \code{ts} with a column for the central tendency (\code{mean}
#' or \code{median}) and one per quantile (e.g. \code{q_5}, \code{q_95}).
#'
#' @examples
#' data("simulated_sem")
#' set.seed(11)
#' fit <- estimate(
#'   ts_data = simulated_sem$ts_data,
#'   sys_eq = simulated_sem$sys_eq,
#'   dates = simulated_sem$dates,
#'   options = list(gibbs = list(ndraws = 10))
#' )
#' res <- residuals(fit)
#' utils::tail(res$consumption)
#' fitted(fit, probs = c(0.05, 0.95))$consumption
#'
#' @export
residuals.koma_estimate <- function(object, ..., central_tendency = "mean",
                                    probs = NULL) {
  rlang::check_dots_empty()
  in_sample_fit(object, "residuals", central_tendency, probs)
}

#' @rdname residuals.koma_estimate
#' @export
fitted.koma_estimate <- function(object, ..., central_tendency = "mean",
                                 probs = NULL) {
  rlang::check_dots_empty()
  in_sample_fit(object, "fitted", central_tendency, probs)
}

in_sample_fit <- function(object, type, central_tendency, probs,
                          call = rlang::caller_env()) {
  central_tendency <- rlang::arg_match(
    central_tendency, c("mean", "median"),
    error_call = call
  )
  if (!is.null(probs)) {
    if (!is.numeric(probs) || length(probs) == 0L || anyNA(probs)) {
      cli::cli_abort("{.arg probs} must be a numeric vector with at least one value.", call = call)
    }
    if (any(probs < 0 | probs > 1)) {
      cli::cli_abort("{.arg probs} must be between 0 and 1.", call = call)
    }
  }

  sys_eq <- object$sys_eq
  equations <- sys_eq$stochastic_equations
  y_matrix <- object$y_matrix

  compute <- function(...) {
    u <- structural_residuals(sys_eq, object$estimates, y_matrix, object$x_matrix, ...)
    out <- if (type == "residuals") u else y_matrix[, colnames(u)] - u
    out[, equations, drop = FALSE]
  }

  central <- compute(central_tendency = central_tendency)
  as_series <- function(x) {
    stats::ts(x, start = stats::start(y_matrix), frequency = stats::frequency(y_matrix))
  }

  if (is.null(probs)) {
    return(stats::setNames(lapply(equations, function(eq) as_series(central[, eq])), equations))
  }

  ndraws <- length(object$estimates[[equations[1]]]$beta_jw)
  draws <- lapply(seq_len(ndraws), function(jx) compute(jx = jx))
  quantile_names <- quantile_names_from_probs(probs)

  stats::setNames(lapply(equations, function(eq) {
    by_draw <- vapply(draws, function(d) d[, eq], numeric(nrow(central)))
    quantiles <- t(apply(by_draw, 1, stats::quantile, probs = probs, names = FALSE))
    if (length(probs) == 1L) {
      quantiles <- t(quantiles)
    }
    out <- cbind(central[, eq], quantiles)
    colnames(out) <- c(central_tendency, quantile_names)
    as_series(out)
  }), equations)
}

# Structural form: y %*% gamma = x %*% beta + u, with one column per equation
# and the equation's own variable on the diagonal of gamma. `...` selects the
# coefficients, see extract_estimates_from_draws().
structural_residuals <- function(sys_eq, estimates, y_matrix, x_matrix, ...) {
  coefficients <- extract_estimates_from_draws(sys_eq, estimates, ...)
  y_matrix[, sys_eq$endogenous_variables, drop = FALSE] %*%
    coefficients$gamma_matrix -
    x_matrix[, rownames(coefficients$beta_matrix), drop = FALSE] %*%
      coefficients$beta_matrix
}

#' Start Adjustments at the Last Residual
#'
#' `check_last_residuals()` checks that the adjustments with `init = "last"`
#' can start at the residual of the period before the forecast: all
#' endogenous variables of the equation must be observed there, not filled.
#' `last_residuals()` then sets the `value` of these adjustments to that
#' residual, at the mean of the coefficients.
#'
#' @param adjustments Named list of adjustments, checked by
#' [validate_adjustments()].
#' @param estimates A `koma_estimate` object.
#' @param current The period before the forecast, as numeric date.
#' @param frequency Number of periods per year.
#' @param y_matrix,x_matrix The data up to `current`, the last row is
#' `current`.
#' @param call The environment from which the error is called.
#'
#' @return `check_last_residuals()` returns `NULL` invisibly or fails.
#' `last_residuals()` returns the adjustments with `value` instead of `init`.
#' @keywords internal
check_last_residuals <- function(adjustments, estimates, current, frequency,
                                 call = rlang::caller_env()) {
  gamma <- estimates$sys_eq$character_gamma_matrix
  as_date <- function(x) dates_to_str(num_to_dates(x, frequency), frequency)

  if (length(from_last_residual(adjustments)) && is.null(estimates$observed_end)) {
    cli::cli_abort(c(
      "x" = "{.code init = \"last\"} needs the last observed periods, which
      this estimate does not store.",
      "i" = "Re-estimate the model with {.fn estimate}."
    ), call = call)
  }

  unobserved <- character(0)
  for (eq in from_last_residual(adjustments)) {
    variables <- rownames(gamma)[gamma[, eq] != "0"]
    observed_end <- estimates$observed_end[variables]
    # half a period of tolerance for the numeric dates
    missing <- observed_end < current - 0.5 / frequency
    if (any(missing)) {
      unobserved <- c(unobserved, paste0(
        eq, ": ",
        paste0(variables[missing], " observed up to ", vapply(
          observed_end[missing], as_date, character(1)
        ), collapse = ", ")
      ))
    }
  }

  if (length(unobserved)) {
    cli::cli_abort(c(
      "x" = "{.code init = \"last\"} needs the residual of {as_date(current)},
      the period before the forecast, from observed data:",
      stats::setNames(unobserved, rep("*", length(unobserved))),
      "i" = "Set the start value with {.field value} instead, e.g. from
      {.fn residuals}."
    ), call = call)
  }
  invisible(NULL)
}

#' @rdname check_last_residuals
last_residuals <- function(adjustments, estimates, y_matrix, x_matrix) {
  from_last <- from_last_residual(adjustments)
  if (!length(from_last)) {
    return(adjustments)
  }

  last <- nrow(y_matrix)
  u <- structural_residuals(
    estimates$sys_eq, estimates$estimates,
    y_matrix[last, , drop = FALSE], x_matrix[last, , drop = FALSE],
    central_tendency = "mean"
  )
  for (eq in from_last) {
    adjustments[[eq]]$value <- u[1, eq]
    adjustments[[eq]]$init <- NULL
  }
  adjustments
}

from_last_residual <- function(adjustments) {
  names(adjustments)[vapply(
    adjustments, function(x) is.list(x) && identical(x[["init"]], "last"),
    logical(1)
  )]
}
