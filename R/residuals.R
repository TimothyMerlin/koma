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

  # Structural form: y %*% gamma = x %*% beta + u, with one column per equation
  # and the equation's own variable on the diagonal of gamma.
  compute <- function(...) {
    coefficients <- extract_estimates_from_draws(sys_eq, object$estimates, ...)
    u <- y_matrix[, sys_eq$endogenous_variables, drop = FALSE] %*%
      coefficients$gamma_matrix -
      object$x_matrix[, rownames(coefficients$beta_matrix), drop = FALSE] %*%
        coefficients$beta_matrix
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
