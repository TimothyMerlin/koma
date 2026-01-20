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
                         freq, forecast_dates, point_forecast) {
  state <- new.env()
  state$warning_issued <- FALSE
  state$warning_issued_restrictions <- FALSE

  out <- list()
  # Take median or mean point forecast
  out$mean <- forecast_single_draw(
    sys_eq, estimates, NULL,
    y_matrix, forecast_x_matrix, horizon, freq, forecast_dates, restrictions,
    state,
    central_tendency = "mean"
  )
  out$median <- forecast_single_draw(
    sys_eq, estimates, NULL,
    y_matrix, forecast_x_matrix, horizon, freq, forecast_dates, restrictions,
    state,
    central_tendency = "median"
  )

  set_progress_handler(operation = "forecasting")

  if (!point_forecast$active) {
    `%dofuture%` <- doFuture::`%dofuture%` # load dofuture

    # Added to circumvent Note:
    #  estimate: no visible binding for global variable draw_jx
    #  Undefined global functions or variables:
    # draw_jx
    draw_jx <- NULL
    nsave <- length(estimates[[1]]$beta_jw)

    p <- progressr::progressor(steps = nsave)

    safe_draw_forecasts <- purrr::safely(function(draw_jx) {
      forecast_single_draw(
        sys_eq, estimates, draw_jx,
        y_matrix, forecast_x_matrix, horizon, freq, forecast_dates,
        restrictions, state
      )
    })

    suppressPackageStartupMessages(
      # Run estimation in parallel
      forecasts <- foreach::foreach(
        draw_jx = seq_len(nsave),
        .options.future = list(
          packages = c("koma"),
          globals = c(
            "p", # Export the progressor function
            "safe_draw_forecasts" # Export the forecast_single_draw function
          ),
          seed = TRUE # Enable future seed
        )
      ) %dofuture% {
        p("") # Signal progress
        safe_draw_forecasts(draw_jx)
      }
    )

    lapply(names(forecasts), function(x) {
      if (!is.null(forecasts[[x]]$error)) {
        cli::cli_warn(c(
          "i" = paste(
            "Error in forecast:", x
          ),
          call = forecasts[[x]]$error
        ))
      }
    })

    forecasts <- purrr::map(forecasts, "result")

    out$quantiles <- quantiles_from_forecasts(forecasts, freq)
    out$forecasts <- forecasts
  } else {
    cli::cli_alert_success("Forecasting completed.")
  }

  out
}

#' Generate a Forecast for a Single Draw
#'
#' This function computes a forecast for a single draw of the parameter
#' estimates, supporting both point forecasts (mean or median) and density
#' forecasts. It constructs the posterior distribution, companion matrix,
#' and reduced-form representation of the system before computing forecasts.
#'
#' @param state An environment used to share mutable state between function
#'   calls, particularly for issuing warnings only once during the forecasting
#'   process.
#'
#' @inheritParams forecast_sem
#' @keywords internal
forecast_single_draw <- function(sys_eq, estimates, jx,
                                 y_matrix, forecast_x_matrix,
                                 horizon, freq, forecast_dates,
                                 restrictions, state, central_tendency = NULL) {
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

  posterior <- construct_posterior(sys_eq, estimates)
  companion_matrix <- construct_companion_matrix(posterior, sys_eq$exogenous_variables)
  reduced_form <- construct_reduced_form(companion_matrix)

  if (!is.null(forecast_x_matrix)) {
    # Shorten horizon if forecast end date is after the latest available date
    max_date <- max(stats::time(stats::na.omit(forecast_x_matrix)))
    if (forecast_dates$end > max_date) {
      horizon <- nrow(stats::na.omit(forecast_x_matrix))

      if (!state$warning_issued) {
        # Identify variables that contain NAs
        na_columns <- colnames(forecast_x_matrix)[apply(is.na(forecast_x_matrix), 2, any)]

        # If no columns with NAs, set all columns as ending before forecast end date
        if (length(na_columns) == 0) na_columns <- colnames(forecast_x_matrix)

        cli::cli_warn(c(
          "!" = "Forecast horizon shortened to {horizon}.",
          ">" = "The following variables end before forecast end date: {na_columns}"
        ))
        state$warning_issued <- TRUE
      }
    }
  }
  endogenous_variables <- sys_eq$endogenous_variables
  if (!all(names(restrictions) %in% endogenous_variables)) {
    missing <-
      names(restrictions)[!names(restrictions) %in% endogenous_variables]
    if (!state$warning_issued_restrictions) {
      cli::cli_warn(c(
        "x" = "Restriction(s) for variable(s) {.val {missing}} ignored: not found among endogenous variables.",
        "i" = "Please ensure all restriction names match endogenous variable names exactly. See ?forecast for details."
      ))
      state$warning_issued_restrictions <- TRUE
    }
    restrictions <-
      restrictions[names(restrictions) %in% endogenous_variables]
  }

  compute_forecast_values(
    posterior, companion_matrix, reduced_form, y_matrix, forecast_x_matrix,
    horizon, freq, forecast_dates$start, sys_eq$endogenous_variables,
    restrictions, sys_eq$identities
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
#' @inheritParams forecast_sem
#'
#' @return A matrix with the base forecast of Y.
#'
#' @keywords internal
compute_forecast_values <- function(posterior, companion_matrix, reduced_form,
                                    y_matrix, forecast_x_matrix, horizon, freq,
                                    start_forecast, endogenous_variables,
                                    restrictions, identities) {
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

  #### Compute baseline forecasts
  base_forecast <- matrix(0, horizon, n)

  if (!is.null(companion_theta)) {
    # selection matrix J
    j_matrix <- cbind(diag(n), matrix(0, n, n * (p - 1)))
    y_t_lag <- t(do.call(rbind, lapply(0:(p - 1), function(x) {
      as.matrix(y_matrix[number_of_observations - x, ])
    })))
  } else {
    # Case where there are no lagged variables
    companion_theta <- matrix(0, n, n) # no persitence when p = 1
    j_matrix <- diag(n) # select current y_t from state
    y_t_lag <- matrix(0, nrow = 1, ncol = n)
  }
  temp <-
    companion_d +
    forecast_x_matrix[1, , drop = FALSE] %*% companion_pi +
    y_t_lag %*% companion_theta

  base_forecast[1, ] <- temp %*% t(j_matrix)

  if (horizon > 1) {
    for (h in seq(2, horizon)) {
      temp <-
        companion_d +
        forecast_x_matrix[h, , drop = FALSE] %*% companion_pi +
        temp %*% companion_theta
      base_forecast[h, ] <- temp %*% t(j_matrix)
    }
  }

  colnames(base_forecast) <- endogenous_variables

  #### Compute restrictions matrix and draw from u conditional on restrictions
  if (length(names(restrictions)) == 0) {
    out <- base_forecast
  } else {
    stopifnot(
      j_matrix %*% companion_gamma_matrix %*% t(j_matrix) ==
        unname(posterior$gamma_matrix)
    )

    psi <- vector("list", horizon)
    a_pow <- diag(n * p)

    for (j in seq_len(horizon)) {
      psi[[j]] <- j_matrix %*% t(a_pow) %*% t(j_matrix) # Psi_{j-1}^0
      a_pow <- a_pow %*% companion_theta # next power
    }
    stopifnot(all(dim(j_matrix) == c(n, n * p))) # q x n * p
    stopifnot(max(abs(psi[[1]] - diag(n))) < 1e-12) # Psi_0 = I

    # number of restrictions: q
    number_restrictions <- sum(vapply(restrictions, function(x) length(x[["horizon"]]), 1L))
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

        blocks <- do.call(cbind, psi[hx:1]) # n x (n*hx)
        # selects the row corresponding to the restricted equation.
        core <- blocks[row_idx, , drop = FALSE] # 1 x (n*hx)

        R[rr, 1:(hx * n)] <- core
        r[rr, 1] <- val_vec[nx] - base_forecast[hx, ix]

        rr <- rr + 1L
      }
    }
    stopifnot(rr - 1L == number_restrictions)
    stopifnot(ncol(R) == n * horizon)

    inv_gamma <- solve(posterior$gamma_matrix)
    omega_matrix <- t(inv_gamma) %*% posterior$sigma_matrix %*% inv_gamma
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
        ">" = "Likely causes: redundant/incompatible restrictions or singular Omega.",
        ">" = "Variables: {paste(vars, collapse = ', ')}."
      ))
    }

    # Ill-conditioning check (warn, don't abort)
    cond_A <- kappa(A, exact = TRUE)
    if (!is.finite(cond_A) || cond_A > 1e12) {
      vars <- unique(names(restrictions))
      cli::cli_warn(c(
        "!" = "A is ill-conditioned (kappa {signif(cond_A, 3)}).",
        ">" = "solve(A, r) may be numerically unstable.",
        ">" = "Variables: {paste(vars, collapse = ', ')}."
      ))
    }

    mvc <- omega_matrix_h %*% t(R) %*% solve(A, r)
    vc <- matrix(mvc, n, horizon)

    # Compute conditional forecasts
    conditional_forecast <- matrix(0, horizon, n)

    temp <-
      companion_d +
      forecast_x_matrix[1, , drop = FALSE] %*% companion_pi +
      y_t_lag %*% companion_theta +
      t(vc[, 1, drop = FALSE]) %*% j_matrix


    conditional_forecast[1, ] <- temp %*% t(j_matrix)

    if (horizon > 1) {
      for (h in seq(2, horizon)) {
        temp <-
          companion_d +
          forecast_x_matrix[h, , drop = FALSE] %*% companion_pi +
          temp %*% companion_theta +
          t(vc[, h, drop = FALSE]) %*% j_matrix
        conditional_forecast[h, ] <- temp %*% t(j_matrix)
      }
    }
    colnames(conditional_forecast) <- endogenous_variables

    out <- conditional_forecast
  }

  ts_out <- stats::ts(out, start = start_forecast, frequency = freq)

  check_identities_add_up(ts_out, identities)

  ts_out
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
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
check_identities_add_up <- function(ts_out, identities, tol = 1e-8) {
  if (is.null(identities) || !length(identities)) {
    return(invisible(NULL))
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
