#' Generate Forecasts for a System of Equations
#'
#' This function performs forecasting based on the provided system of equations,
#' estimates, and other parameters. It supports both density and point
#' forecasting.
#'
#' @param estimates List of parameter estimates for the system.
#' @param y_matrix Matrix of the dependent variable time series data.
#' @param forecast_x_matrix A matrix with forecasting data for exogenous
#' variables.
#' @param horizon Forecasting horizon, specifying the number of periods.
#' @param freq Frequency of the time series data.
#' @param forecast_dates List containing 'start' and 'end' dates for forecast.
#' @inheritParams forecast
#' @inheritParams estimate
#'
#' @return A list containing the forecast values.
#' @keywords internal
forecast_sem <- function(sys_eq, estimates,
                         restrictions, y_matrix, forecast_x_matrix, horizon,
                         freq, forecast_dates, approximate, probs,
                         conditional_innov_method = "projection") {
  # The same for every draw, so shorten (and warn) once in the main process
  # instead of per draw, where multisession workers cannot share the state.
  horizon <- shorten_forecast_horizon(horizon, forecast_x_matrix, forecast_dates)

  # Validate against the possibly shortened horizon, so restrictions beyond it
  # fail once here instead of in every draw.
  restrictions <- validate_restrictions(
    restrictions, sys_eq$endogenous_variables, horizon
  )

  # Depends only on the system of equations, so find it once for all draws.
  phi_positions <- find_phi_positions(sys_eq)

  out <- list()

  set_progress_handler(operation = "forecasting")

  if (approximate) {
    out$mean <- forecast_draw(
      sys_eq, estimates, NULL,
      y_matrix, forecast_x_matrix, horizon, freq, forecast_dates, restrictions,
      phi_positions,
      conditional_innov_method = conditional_innov_method,
      central_tendency = "mean"
    )
    out$median <- forecast_draw(
      sys_eq, estimates, NULL,
      y_matrix, forecast_x_matrix, horizon, freq, forecast_dates, restrictions,
      phi_positions,
      conditional_innov_method = conditional_innov_method,
      central_tendency = "median"
    )
    out$quantiles <- NULL
    out$forecasts <- NULL
    cli::cli_alert_success("Forecasting completed.")
  } else {
    `%dofuture%` <- doFuture::`%dofuture%` # load dofuture

    # Added to circumvent Note:
    #  estimate: no visible binding for global variable draw_jx
    #  Undefined global functions or variables:
    # draw_jx
    draw_jx <- NULL
    nsave <- length(estimates[[1]]$beta_jw)

    # Evaluate lazy arguments before the closure is shipped to future workers.
    # An unevaluated promise may refer to the caller's global environment,
    # which is not exported to multisession workers.
    force(sys_eq)
    force(restrictions)
    force(y_matrix)
    force(forecast_x_matrix)
    force(horizon)
    force(freq)
    force(forecast_dates)
    force(conditional_innov_method)

    p <- progressr::progressor(steps = nsave)

    # Warnings and errors are collected per draw and reported once below,
    # instead of being signalled for every draw on the workers.
    safe_draw_forecasts <- purrr::safely(function(draw_jx) {
      warnings <- character(0)
      result <- withCallingHandlers(
        forecast_draw(
          sys_eq, estimates, draw_jx,
          y_matrix, forecast_x_matrix, horizon, freq, forecast_dates,
          restrictions, phi_positions,
          conditional_innov_method = conditional_innov_method
        ),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      list(result = result, warnings = warnings)
    })

    suppressPackageStartupMessages(
      # Run estimation in parallel
      forecasts <- foreach::foreach(
        draw_jx = seq_len(nsave),
        .options.future = list(
          packages = c("koma"),
          globals = c(
            "p", # Export the progressor function
            "safe_draw_forecasts" # Export the forecast_draw function
          ),
          seed = TRUE # Enable future seed
        )
      ) %dofuture% {
        p("") # Signal progress
        safe_draw_forecasts(draw_jx)
      }
    )

    # x$error is the error of a failed draw, x$result$warnings the warnings
    # and x$result$result the forecast.
    draw_warnings <- unlist(lapply(forecasts, function(x) x$result$warnings))
    draw_errors <- Filter(Negate(is.null), lapply(forecasts, `[[`, "error"))
    # The first error is chained as parent, so rlang::last_trace() shows
    # where the draw failed.
    first_error <- if (length(draw_errors) > 0) draw_errors[[1]]
    draw_errors <- vapply(draw_errors, conditionMessage, character(1))
    forecasts <- lapply(forecasts, function(x) x$result$result)

    if (length(draw_warnings) > 0) {
      cli::cli_warn(c(
        "!" = "Forecast draws raised warnings:",
        summarise_draw_conditions(draw_warnings, nsave)
      ))
    }

    valid_forecasts <- !vapply(forecasts, is.null, logical(1))
    if (!any(valid_forecasts)) {
      cli::cli_abort(c(
        "x" = "All forecast draws failed:",
        summarise_draw_conditions(draw_errors, nsave)
      ), parent = first_error)
    }
    if (!all(valid_forecasts)) {
      cli::cli_warn(c(
        "!" = "Some forecast draws failed and were dropped:",
        summarise_draw_conditions(draw_errors, nsave),
        ">" = "Proceeding with {sum(valid_forecasts)} of {length(forecasts)} draws."
      ), parent = first_error)
      forecasts <- forecasts[valid_forecasts]
    }

    probs_for_summary <- if (is.null(probs)) 0.5 else unique(c(probs, 0.5))
    summary <- quantiles_from_forecasts(
      forecasts,
      freq,
      probs = probs_for_summary,
      include_mean = TRUE
    )
    out$mean <- summary$q_mean
    out$median <- summary$q_50
    if (is.null(probs)) {
      out$quantiles <- NULL
    } else {
      out$quantiles <- summary
      out$quantiles$q_mean <- NULL
      if (!0.5 %in% probs) {
        out$quantiles$q_50 <- NULL
      }
    }
    out$forecasts <- forecasts
  }

  out
}

#' Shorten the Forecast Horizon to the Available Exogenous Data
#'
#' If the exogenous variables end before the forecast end date, the horizon is
#' shortened to the number of periods with complete exogenous data, and a
#' warning names the variables that end early.
#'
#' @inheritParams forecast_sem
#'
#' @return The (possibly shortened) forecast horizon.
#' @keywords internal
shorten_forecast_horizon <- function(horizon, forecast_x_matrix, forecast_dates) {
  if (is.null(forecast_x_matrix)) {
    return(horizon)
  }

  max_date <- max(stats::time(stats::na.omit(forecast_x_matrix)))
  if (forecast_dates$end <= max_date) {
    return(horizon)
  }

  horizon <- nrow(stats::na.omit(forecast_x_matrix))

  # Identify variables that contain NAs
  na_columns <- colnames(forecast_x_matrix)[apply(is.na(forecast_x_matrix), 2, any)]

  # If no columns with NAs, set all columns as ending before forecast end date
  if (length(na_columns) == 0) na_columns <- colnames(forecast_x_matrix)

  cli::cli_warn(c(
    "!" = "Forecast horizon shortened to {horizon}.",
    ">" = "The following variables end before forecast end date: {na_columns}"
  ))

  horizon
}

#' Summarise Conditions Raised by Forecast Draws
#'
#' Groups condition messages by their first line and counts how often each
#' occurred, so that a condition raised in many draws is reported once.
#'
#' @param messages Character vector of condition messages.
#' @param n_draws Total number of draws.
#'
#' @return A named character vector of cli bullets.
#' @keywords internal
summarise_draw_conditions <- function(messages, n_draws) {
  headers <- vapply(
    strsplit(messages, "\n", fixed = TRUE),
    function(x) if (length(x)) x[[1]] else "",
    character(1)
  )
  # drop the leading cli symbol of the first line; the summary adds its own
  symbols <- c(
    "!", cli::symbol$cross, cli::symbol$info, cli::symbol$tick,
    cli::symbol$arrow_right, cli::symbol$bullet
  )
  for (prefix in paste0(symbols, " ")) {
    has_prefix <- startsWith(headers, prefix)
    headers[has_prefix] <- substring(headers[has_prefix], nchar(prefix) + 1)
  }

  unique_headers <- unique(headers)
  counts <- vapply(unique_headers, function(h) sum(headers == h), integer(1))

  # escape braces so that cli does not interpolate the messages
  unique_headers <- gsub("{", "{{", unique_headers, fixed = TRUE)
  unique_headers <- gsub("}", "}}", unique_headers, fixed = TRUE)

  stats::setNames(
    sprintf("%d of %d draws: %s", counts, n_draws, unique_headers),
    rep("*", length(unique_headers))
  )
}

#' Generate a Forecast for a Single Draw
#'
#' This function computes a forecast for a single draw of the parameter
#' estimates, supporting both point forecasts (mean or median) and density
#' forecasts. It constructs the posterior distribution, companion matrix,
#' and reduced-form representation of the system before computing forecasts.
#'
#' @inheritParams forecast_sem
#' @inheritParams construct_phi
#' @keywords internal
forecast_draw <- function(sys_eq, estimates, jx,
                          y_matrix, forecast_x_matrix,
                          horizon, freq, forecast_dates,
                          restrictions, phi_positions,
                          conditional_innov_method = "projection",
                          central_tendency = NULL) {
  if (is.null(jx)) {
    # Case point forecast with option to extract mean or median estimates
    estimates <- extract_estimates_from_draws(
      sys_eq, estimates,
      central_tendency = central_tendency
    )
  } else {
    # Case density forecast
    # Construct posterior of draw jx
    estimates <- extract_estimates_from_draws(sys_eq, estimates, jx = jx)
  }

  posterior <- construct_posterior(sys_eq, estimates, phi_positions)
  companion_matrix <- construct_companion_matrix(posterior, sys_eq$exogenous_variables)
  reduced_form <- construct_reduced_form(companion_matrix)

  forecast_values(
    posterior, companion_matrix, reduced_form, y_matrix, forecast_x_matrix,
    horizon, freq, forecast_dates$start, sys_eq$endogenous_variables,
    restrictions, sys_eq$identities,
    conditional_innov_method = conditional_innov_method,
    stochastic = is.null(central_tendency)
  )
}

#' Compute Forecast Based on Companion Matrix and Reduced Form
#'
#' This function calculates the forecast for a given set of parameters and data.
#' It computes baseline forecasts and applies certain restrictions if specified.
#'
#' @param posterior A list of posterior system matrices from
#' `construct_posterior()` (e.g., `gamma_matrix`, `sigma_matrix`).
#' @param companion_matrix A list containing components of the companion matrix.
#' @param reduced_form A list containing the reduced form components.
#' @param start_forecast A vector of format `c(YEAR, QUARTER)` representing
#' the start date of the forecast.
#' @param endogenous_variables A character vector containing the names of
#' endogenous variables.
#' @param stochastic Logical; when `TRUE`, returns a stochastic forecast draw.
#' @inheritParams forecast_sem
#'
#' @return A matrix with the base forecast of Y.
#'
#' @keywords internal
forecast_values <- function(posterior, companion_matrix, reduced_form,
                            y_matrix, forecast_x_matrix, horizon, freq,
                            start_forecast, endogenous_variables,
                            restrictions, identities,
                            conditional_innov_method = "projection",
                            stochastic = FALSE) {
  companion_gamma_matrix <- companion_matrix$gamma_matrix
  companion_pi <- reduced_form$companion_pi
  companion_theta <- reduced_form$companion_theta
  companion_d <- reduced_form$companion_d
  n <- companion_matrix$n
  p <- companion_matrix$p

  number_of_observations <- dim(y_matrix)[1]
  if (!is.null(forecast_x_matrix)) {
    forecast_x_matrix <- as.matrix(forecast_x_matrix)
  } else {
    # Case where there are no exogenous variables: e.g. AR(p) model
    forecast_x_matrix <- matrix(0, horizon, n)
    companion_pi <- matrix(0, n, n * p)
  }

  if (!is.null(companion_theta)) {
    # selection matrix J
    j_matrix <- cbind(diag(n), matrix(0, n, n * (p - 1)))
    y0 <- t(do.call(rbind, lapply(0:(p - 1), function(x) {
      as.matrix(y_matrix[number_of_observations - x, ])
    })))
  } else {
    # Case where there are no lagged variables
    companion_theta <- matrix(0, n, n) # no persitence when p = 1
    j_matrix <- diag(n) # select current y_t from state
    y0 <- matrix(0, nrow = 1, ncol = n)
  }

  # structural shocks
  # Clamp tiny negative variances from numerical noise to zero.
  sd <- sqrt(pmax(diag(posterior$sigma_matrix), 0))
  z_matrix <- matrix(rnorm(horizon * n), nrow = n, ncol = horizon)
  u_matrix <- z_matrix * sd
  # reduced-form innovations
  v_matrix <- solve(t(posterior$gamma_matrix), u_matrix) # = t(inv_gamma) %*% u_matrix

  deterministic_uncond <- forecast_companion(
    horizon,
    y0,
    companion_d,
    forecast_x_matrix,
    companion_pi,
    companion_theta,
    j_matrix,
    vc = NULL
  )

  stochastic_uncond <- forecast_companion(
    horizon,
    y0,
    companion_d,
    forecast_x_matrix,
    companion_pi,
    companion_theta,
    j_matrix,
    vc = v_matrix
  )

  #### Compute restrictions matrix and draw from u conditional on restrictions
  if (length(names(restrictions)) == 0) {
    out <- if (stochastic) {
      stochastic_uncond$forecast
    } else {
      deterministic_uncond$forecast
    }
  } else {
    stopifnot(
      j_matrix %*% companion_gamma_matrix %*% t(j_matrix) ==
        unname(posterior$gamma_matrix)
    )

    psi_transpose <- vector("list", horizon)
    a_pow <- diag(n * p)

    for (j in seq_len(horizon)) {
      psi_transpose[[j]] <- j_matrix %*% t(a_pow) %*% t(j_matrix) # Psi_{j-1}^0
      a_pow <- a_pow %*% companion_theta # next power
    }
    stopifnot(all(dim(j_matrix) == c(n, n * p))) # q x n * p
    stopifnot(max(abs(psi_transpose[[1]] - diag(n))) < 1e-12) # Psi_0 = I

    # number of restrictions: q
    number_restrictions <- sum(
      vapply(restrictions, function(x) length(x[["horizon"]]), 1L)
    )
    R <- matrix(0, nrow = number_restrictions, ncol = n * horizon)
    r <- matrix(0, nrow = number_restrictions, ncol = 1)

    rr <- 1L
    for (ix in names(restrictions)) {
      hx_vec <- restrictions[[ix]][["horizon"]]
      val_vec <- restrictions[[ix]][["value"]]

      row_idx <- which(endogenous_variables == ix)
      for (nx in seq_along(hx_vec)) {
        hx <- hx_vec[nx]

        if (hx < 1 || hx > horizon) stop("hx must be in 1..horizon")

        blocks <- do.call(cbind, psi_transpose[hx:1]) # n x (n*hx)
        # selects the row corresponding to the restricted equation.
        core <- blocks[row_idx, , drop = FALSE] # 1 x (n*hx)

        R[rr, 1:(hx * n)] <- core
        # use deterministic base forecast (expectation)
        r[rr, 1] <- val_vec[nx] - deterministic_uncond$forecast[hx, row_idx]

        rr <- rr + 1L
      }
    }
    stopifnot(rr - 1L == number_restrictions)
    stopifnot(ncol(R) == n * horizon)

    # Omega = (Gamma^{-1})' Sigma Gamma^{-1}
    # omega_matrix <- t(inv_gamma) %*% posterior$sigma_matrix %*% inv_gamma
    # numerically more stable:
    # Compute A = (Gamma^{-1})' Sigma:
    a_matrix <- solve(t(posterior$gamma_matrix), posterior$sigma_matrix)
    omega_matrix <- t(solve(t(posterior$gamma_matrix), t(a_matrix)))
    # Enforce symmetry of the reduced-form covariance matrix
    # (theoretically symmetric; numerical operations may introduce asymmetry)
    omega_matrix <- 0.5 * (omega_matrix + t(omega_matrix))
    # isTRUE(all.equal(omega_matrix, t(omega_matrix)))
    omega_matrix_h <- kronecker(diag(horizon), omega_matrix)

    #    mvc <- sigma_v %*% t(R) %*% solve(R %*% sigma_v %*% t(R)) %*% r
    A <- R %*% omega_matrix_h %*% t(R)

    # Rank check with explicit tolerance
    tol <- max(1e-12, sqrt(.Machine$double.eps))
    rank_A <- qr(A, tol = tol)$rank
    if (rank_A < nrow(A)) {
      vars <- unique(names(restrictions))
      cli::cli_abort(c(
        "x" = "A = R %*% Omega %*% t(R) is singular (rank {rank_A} < {nrow(A)}).",
        ">" = "Cannot compute solve(A, r).",
        ">" = "Likely causes: redundant/incompatible restrictions.",
        ">" = "Variables: {paste(vars, collapse = ', ')}."
      ))
    }

    # Ill-conditioning check (warn, don't abort)
    cond_A <- kappa(A, exact = TRUE)
    if (!is.finite(cond_A) || cond_A > 1e12) {
      vars <- unique(names(restrictions))
      cli::cli_warn(c(
        "!" = "A is ill-conditioned.",
        ">" = "kappa = {signif(cond_A, 3)}; solve(A, r) may be numerically unstable.",
        ">" = "Variables: {paste(vars, collapse = ', ')}."
      ))
    }

    v_cond <- draw_conditional_innovations(
      v_uncond_vec = as.vector(v_matrix),
      omega_matrix_h = omega_matrix_h,
      R = R,
      r = r,
      A = A,
      method = conditional_innov_method
    )
    v_cond_mean_vec <- v_cond$mean_vec
    v_cond_mean <- matrix(v_cond_mean_vec, nrow = n, ncol = horizon)

    deterministic_cond <- forecast_companion(
      horizon,
      y0,
      companion_d,
      forecast_x_matrix,
      companion_pi,
      companion_theta,
      j_matrix,
      vc = v_cond_mean
    )

    v_cond_draw_vec <- v_cond$draw_vec
    v_cond_draw <- matrix(v_cond_draw_vec, nrow = n, ncol = horizon)

    stochastic_cond <- forecast_companion(
      horizon,
      y0,
      companion_d,
      forecast_x_matrix,
      companion_pi,
      companion_theta,
      j_matrix,
      vc = v_cond_draw
    )

    out <- if (stochastic) {
      stochastic_cond$forecast
    } else {
      deterministic_cond$forecast
    }
  }

  colnames(out) <- endogenous_variables
  ts_out <- stats::ts(out, start = start_forecast, frequency = freq)

  validate_identities(ts_out, identities, x_matrix = forecast_x_matrix)

  ts_out
}

draw_conditional_innovations <- function(v_uncond_vec,
                                         omega_matrix_h,
                                         R,
                                         r,
                                         A,
                                         method = c("projection", "eigen"),
                                         tol = 1e-10) {
  method <- match.arg(method)
  mean_vec <- omega_matrix_h %*% t(R) %*% solve(A, r)

  if (method == "projection") {
    draw_vec <- v_uncond_vec +
      omega_matrix_h %*% t(R) %*% solve(A, r - R %*% v_uncond_vec)
    return(list(mean_vec = as.vector(mean_vec), draw_vec = as.vector(draw_vec)))
  }

  Omega_c <- omega_matrix_h -
    omega_matrix_h %*% t(R) %*% solve(A) %*% R %*% omega_matrix_h
  Omega_c <- 0.5 * (Omega_c + t(Omega_c))
  ev <- eigen(Omega_c, symmetric = TRUE)
  idx <- ev$values > tol
  U <- ev$vectors[, idx, drop = FALSE]
  D <- ev$values[idx]
  z <- rnorm(length(D))
  draw_vec <- as.vector(mean_vec) + U %*% (sqrt(D) * z)
  list(
    mean_vec = as.vector(mean_vec),
    draw_vec = as.vector(draw_vec)
  )
}

forecast_companion <- function(horizon,
                               y0,
                               companion_d,
                               forecast_x_matrix,
                               companion_pi,
                               companion_theta,
                               j_matrix,
                               vc = NULL) {
  if (is.null(vc)) {
    vc <- matrix(0, nrow = nrow(j_matrix), ncol = horizon)
  }

  state <- matrix(NA_real_, nrow = horizon, ncol = ncol(y0))

  temp <-
    companion_d +
    forecast_x_matrix[1, , drop = FALSE] %*% companion_pi +
    y0 %*% companion_theta +
    t(vc[, 1, drop = FALSE]) %*% j_matrix

  state[1, ] <- temp

  if (horizon > 1) {
    for (h in 2:horizon) {
      temp <-
        companion_d +
        forecast_x_matrix[h, , drop = FALSE] %*% companion_pi +
        temp %*% companion_theta +
        t(vc[, h, drop = FALSE]) %*% j_matrix

      state[h, ] <- temp
    }
  }

  forecast <- state %*% t(j_matrix)
  list(state = state, forecast = forecast)
}

#' Check Identity Equations in Forecast Output
#'
#' Recomputes each identity from its component series and weights, then warns if
#' deviations exceed `tol`. Intended as a safeguard against identity drift.
#'
#' @param ts_out A forecast output time-series matrix.
#' @param identities A named list of identity definitions produced by
#'   [get_identities()] and updated by [get_seq_weights()].
#' @param tol Numeric tolerance for deviations between the identity and its
#'   reconstructed value.
#' @param x_matrix Optional matrix of exogenous variables to include when
#'   checking identities.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
validate_identities <- function(ts_out, identities, tol = 1e-8,
                                x_matrix = NULL) {
  if (is.null(identities) || !length(identities)) {
    return(invisible(NULL))
  }

  if (!is.null(x_matrix)) {
    x_matrix <- as.matrix(x_matrix)
    if (is.null(colnames(x_matrix))) {
      colnames(x_matrix) <- paste0("x", seq_len(ncol(x_matrix)))
    }
    ts_x <- stats::ts(x_matrix,
      start = stats::start(ts_out),
      frequency = stats::frequency(ts_out)
    )
    new_cols <- setdiff(colnames(ts_x), colnames(ts_out))
    if (length(new_cols)) {
      ts_out <- cbind(ts_out, ts_x[, new_cols, drop = FALSE])
    }
  }

  for (lhs in names(identities)) {
    iden <- identities[[lhs]]
    if (!lhs %in% colnames(ts_out)) next
    comps <- names(iden$components)
    if (!length(comps)) next

    weights <- vapply(comps, function(comp) {
      weight_name <- iden$components[[comp]]
      wt <- iden$weights[[weight_name]]
      suppressWarnings(as.numeric(wt))
    }, numeric(1))

    missing_components <- setdiff(comps, colnames(ts_out))
    missing_weights <- comps[is.na(weights)]
    if (length(missing_components) || length(missing_weights)) {
      warn <- c(
        "!" = "Identity {.val {lhs}} could not be checked."
      )
      if (length(missing_components)) {
        warn <- c(
          warn,
          ">" = "Missing components in forecast output: {.val {missing_components}}."
        )
      }
      if (length(missing_weights)) {
        warn <- c(
          warn,
          ">" = "Weights missing or non-numeric for components: {.val {missing_weights}}."
        )
      }
      cli::cli_warn(warn)
      next
    }


    X <- ts_out[, comps, drop = FALSE]
    rhs <- as.numeric(X %*% weights)

    lhs_series <- ts_out[, lhs]
    diff <- abs(lhs_series - rhs)
    if (any(diff > tol, na.rm = TRUE)) {
      where <- which(diff > tol)
      cli::cli_warn(c(
        "!" = "Identity {.val {lhs}} not satisfied at horizon {.val {paste(where, collapse = ', ')}}."
      ))
    }
  }

  invisible(NULL)
}
