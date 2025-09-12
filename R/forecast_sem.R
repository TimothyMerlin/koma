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
    #  estimate: no visible binding for global variable ‘draw_jx’
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
    companion_matrix, reduced_form, y_matrix, forecast_x_matrix,
    horizon, freq, forecast_dates$start, sys_eq$endogenous_variables,
    restrictions
  )
}

#' Compute Forecast Based on Companion Matrix and Reduced Form
#'
#' This function calculates the forecast for a given set of parameters and data.
#' It computes baseline forecasts and applies certain restrictions if specified.
#'
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
#' @importFrom expm %^%
#' @keywords internal
compute_forecast_values <- function(companion_matrix, reduced_form, y_matrix,
                                    forecast_x_matrix, horizon, freq,
                                    start_forecast, endogenous_variables,
                                    restrictions) {
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
    j_matrix <- t(cbind(diag(n), matrix(0, n, n * (p - 1))))
    y_t_lag <- t(do.call(rbind, lapply(0:(p - 1), function(x) {
      as.matrix(y_matrix[number_of_observations - x, ])
    })))
  } else {
    # Case where there is only one equation or no lagged variables
    j_matrix <- companion_theta <- diag(dim(companion_d)[2])
    y_t_lag <- matrix(0, nrow = 1, ncol = n)
  }
  temp <-
    companion_d +
    forecast_x_matrix[1, , drop = FALSE] %*% companion_pi +
    y_t_lag %*% companion_theta

  base_forecast[1, ] <- temp %*% j_matrix

  if (horizon > 1) {
    for (ix in seq(2, horizon)) {
      temp <-
        companion_d +
        forecast_x_matrix[ix, , drop = FALSE] %*% companion_pi +
        temp %*% companion_theta
      base_forecast[ix, ] <- temp %*% j_matrix
    }
  }

  colnames(base_forecast) <- endogenous_variables

  #### Compute restrictions matrix and draw from u conditional on restrictions
  if (length(names(restrictions)) == 0) {
    out <- base_forecast
  } else {
    psi_s <- lapply(0:(horizon - 1), function(x) {
      thetax <- companion_theta %^% x # nolint: object_usage_linter.
      temp <- t((solve(companion_gamma_matrix) %*% thetax))
      temp
    })

    R <- list()
    r <- list()
    for (ix in names(restrictions)) {
      for (nx in seq_along(restrictions[[ix]][["horizon"]])) {
        hx <- restrictions[[ix]][["horizon"]][nx]
        R[[ix]][[nx]] <- cbind(
          t(do.call(cbind, psi_s[hx:1])[which(endogenous_variables %in% ix), ]),
          matrix(0, 1, (horizon * n * p - hx * n * p))
        )
        r[[ix]][[nx]] <-
          restrictions[[ix]][["value"]][nx] - base_forecast[hx, ix]
      }
      R[[ix]] <- do.call(rbind, R[[ix]])
      r[[ix]] <- do.call(rbind, r[[ix]])
    }

    R <- do.call(rbind, R)
    r <- do.call(rbind, r)

    muc <- t(R) %*% solve(R %*% t(R)) %*% r

    # Just use conditional mean (without density forecasts)
    uc <- matrix(muc, n * p, horizon)

    # Compute conditional forecasts
    conditional_forecast <- matrix(0, horizon, n)
    temp <-
      companion_d +
      forecast_x_matrix[1, ] %*% companion_pi +
      y_t_lag %*% companion_theta +
      t(uc[, 1]) %*% solve(companion_gamma_matrix)

    conditional_forecast[1, ] <- temp %*% j_matrix

    if (horizon > 1) {
      for (ix in seq(2, horizon)) {
        temp <- companion_d +
          forecast_x_matrix[ix, ] %*% companion_pi +
          temp %*% companion_theta +
          t(uc[, ix]) %*% solve(companion_gamma_matrix)
        conditional_forecast[ix, ] <- temp %*% j_matrix
      }
    }
    colnames(conditional_forecast) <- endogenous_variables

    out <- conditional_forecast
  }

  stats::ts(out, start = start_forecast, frequency = freq)
}
