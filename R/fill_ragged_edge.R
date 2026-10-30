#' Fill Ragged Edges in Time Series Data
#'
#' This function fills in the ragged edges in a time series data set using a
#' system of equations model. It iteratively detects edges, estimates the
#' model, and fills the unobserved series using a one-step ahead conditional
#' forecast until the time series is balanced.
#'
#' @param fill_method Character string indicating which central tendency measure
#' ("mean" or "median") to use when filling ragged edges.
#' @inheritParams estimate
#' @inheritParams system_of_equations
#'
#' @return A list containing the updated time series data.
#' @keywords internal
fill_ragged_edge <- function(ts_data, sys_eq,
                             exogenous_variables, dates, fill_method) {
  fill_method <- match.arg(fill_method, c("mean", "median"))
  endogenous_variables <- sys_eq$endogenous_variables
  total_exogenous_variables <- sys_eq$total_exogenous_variables

  edge <- detect_edge(
    ts_data[endogenous_variables],
    dates$estimation$start,
    dates$estimation$end
  )

  if (edge$date >= dates$estimation$end) {
    return(ts_data)
  }

  while (edge$date < dates$estimation$end) {
    ##### Construct Y and X matrix
    balanced_data <- construct_balanced_data(
      ts_data, endogenous_variables, total_exogenous_variables,
      dates$estimation$start, dates$estimation$end
    )

    date <- dates_to_str(num_to_dates(edge$date, balanced_data$freq), balanced_data$freq)
    cli::cli_text("")
    cli::cli_text("Ragged edge detected at {.val {date}}")
    cli::cli_text("Missing observations for: {.val {edge$variable_names}}")
    cli::cli_text("This will require estimation and forecasting to fill the gap.")
    response <- readline(prompt = "Continue with ragged edge processing? (y/n):")
    if (tolower(response) != "y") {
      cli::cli_text("{.alert-warning Ragged edge filling aborted by user.}")
      return(ts_data)
    }

    prompt <- paste0(
      "Choose fill method (mean/median) [", fill_method, "]: "
    )
    response <- readline(prompt = prompt)
    response <- tolower(trimws(response))
    if (nzchar(response)) {
      if (response %in% c("mean", "median")) {
        fill_method <- response
      } else {
        cli::cli_warn(c(
          "x" = "Invalid fill method {.val {response}}.",
          "i" = "Using {.val {fill_method}}."
        ))
      }
    }

    y_matrix <- balanced_data$y_matrix
    x_matrix <- balanced_data$x_matrix
    edge <- balanced_data$edge

    ##### Estimate model
    estimates <- estimate_sem(sys_eq, y_matrix, x_matrix)

    # One step ahead forecast to fill unobserved series at edge
    horizon <- 1

    # Set variables with observation as restricted
    variables_to_restrict <- endogenous_variables[
      !endogenous_variables %in% edge$variable_names
    ]
    restrictions <- drop_determined_identities(
      set_restrictions(
        ts_data, variables_to_restrict,
        start = edge$date,
        end = edge$date
      ),
      sys_eq
    )

    forecast_dates <- list()
    forecast_dates$start <- iterate_n_periods(edge$date, 1, frequency = balanced_data$freq)
    # forecast end date needs to be different to start to get a matrix
    # because horizon is 1 the end date will be disregarded
    forecast_dates$end <- iterate_n_periods(edge$date, 2, frequency = balanced_data$freq)

    forecast_x_matrix <- stats::window(
      as_mets(ts_data[sys_eq$exogenous_variables]),
      start = forecast_dates$start,
      end = forecast_dates$end
    )

    ##### Produce forecasts
    forecasts <- forecast_sem(
      sys_eq, estimates, restrictions,
      y_matrix, forecast_x_matrix, horizon, balanced_data$freq, forecast_dates,
      approximate = TRUE, probs = NULL
    )

    # Take mean or median forecast
    forecasts <- forecasts[[fill_method]]

    ts_data <- extend_ts_with_forecast(ts_data, forecasts)

    # remove lagged variables
    ts_data <- ts_data[c(sys_eq$endogenous_variables, sys_eq$exogenous_variables)]
    # lag values
    ts_data <- create_lagged_variables(
      ts_data, sys_eq$endogenous_variables,
      exogenous_variables, sys_eq$predetermined_variables
    )

    edge <- detect_edge(
      ts_data[endogenous_variables],
      dates$estimation$start,
      dates$estimation$end
    )
  }

  cli::cli_rule()
  cli::cli_text("") # Empty line via cli

  ts_data
}

#' Fill the Ragged Edge in Each Forecast Draw
#'
#' Prepares a density forecast that fills the periods before the forecast in
#' each draw, with the coefficients and shocks of the draw, instead of once.
#' The forecast of each draw starts after the last period in which all
#' endogenous variables are observed, and the observed values of the periods
#' up to the forecast are restrictions. Periods filled by [estimate()] count
#' as unobserved, see `observed_end` in [estimate()].
#'
#' @param estimates A `koma_estimate` object (see \code{\link{estimate}}).
#' @param ts_data The data prepared for the forecast, in rates and with lags.
#' Up to the edge it is observed.
#' @param dates The dates of the forecast, with `current`, the period before
#' the forecast.
#' @param frequency Number of periods per year.
#'
#' @return `NULL` if all endogenous variables are observed up to `current`.
#' Otherwise a list with `fill_periods`, the number of filled periods, `start`,
#' the first filled period, `restrictions` for the observed values of the
#' filled periods, and `y_matrix` and `x_matrix`, the endogenous data up to
#' the edge and the exogenous data from `start` to the end of the forecast.
#' @keywords internal
fill_in_draws <- function(estimates, ts_data, dates, frequency) {
  sys_eq <- estimates$sys_eq
  endogenous_variables <- sys_eq$endogenous_variables
  observed_data <- rate(estimates$ts_data[endogenous_variables])
  observed_end <- estimates$observed_end
  if (is.null(observed_end)) {
    observed_end <- vapply(observed_data, function(x) {
      stats::tsp(stats::na.omit(x))[2]
    }, numeric(1))
  }
  observed_end <- observed_end[endogenous_variables]

  edge <- min(observed_end)
  fill_periods <- round((dates$current - edge) * frequency)
  if (fill_periods < 1) {
    return(NULL)
  }
  start <- iterate_n_periods(edge, 1, frequency)

  # Values after observed_end were filled by estimate(), so they are dropped.
  observed_data <- lapply(endogenous_variables, function(x) {
    stats::window(observed_data[[x]], end = observed_end[[x]])
  })
  names(observed_data) <- endogenous_variables
  restricted <- endogenous_variables[observed_end > edge + 0.5 / frequency]
  restrictions <- drop_determined_identities(
    set_restrictions(
      observed_data, restricted,
      start = start, end = dates$current
    ),
    sys_eq
  )

  y_matrix <- construct_balanced_data(
    ts_data, endogenous_variables, sys_eq$total_exogenous_variables,
    dates$estimation$start, edge
  )$y_matrix
  x_matrix <- NULL
  if (length(sys_eq$exogenous_variables) > 0) {
    x_matrix <- stats::window(
      as_mets(ts_data[sys_eq$exogenous_variables]),
      start = start, end = dates$forecast$end
    )
  }

  list(
    fill_periods = fill_periods,
    start = start,
    restrictions = restrictions,
    y_matrix = y_matrix,
    x_matrix = x_matrix
  )
}

#' Conditional Fill and Forecast for Time Series Data
#'
#' This function fills missing observations and extends the time series data
#' up to and including the current quarter. The function conditionally forecasts
#' based on the estimates and realized observations.
#'
#' @param fill_method Character string indicating which central tendency measure
#' ("mean" or "median") to use when filling ragged edges.
#' @param estimates A `koma_estimate` object (see \code{\link{estimate}})
#' containing the estimates of the simultaneous equations model.
#' @inheritParams estimate
#' @inheritParams system_of_equations
#'
#' @return An extended time series list containing the filled and forecasted
#' time series up to the current quarter.
#' @keywords internal
conditional_fill <- function(ts_data, sys_eq, dates,
                             estimates, fill_method) {
  fill_method <- match.arg(fill_method, c("mean", "median"))
  endogenous_variables <- sys_eq$endogenous_variables

  edge <- detect_edge(
    ts_data[endogenous_variables],
    dates$estimation$start,
    dates$current
  )

  ##### Construct Y and X matrix
  balanced_data <- construct_balanced_data(
    ts_data, endogenous_variables,
    sys_eq$total_exogenous_variables,
    dates$estimation$start, dates$current
  )

  y_matrix <- balanced_data$y_matrix
  edge <- balanced_data$edge

  # Forecast horizon
  frequency <- balanced_data$freq
  horizon <- length(seq(
    iterate_n_periods(edge$date, 1, frequency),
    dates$current,
    by = 1 / frequency
  ))

  # Set variables with observation as restricted
  variables_to_restrict <- endogenous_variables[
    !endogenous_variables %in% edge$variable_names
  ]
  restrictions <- drop_determined_identities(
    set_restrictions(
      ts_data, variables_to_restrict,
      start = iterate_n_periods(edge$date, 1, frequency = frequency),
      end = dates$current
    ),
    sys_eq
  )

  forecast_dates <- list()
  forecast_dates$start <- iterate_n_periods(edge$date, 1, frequency = frequency)
  # forecast end date needs to be different to start to get a matrix
  # because horizon is 1 the end date will be disregarded
  forecast_dates$end <- iterate_n_periods(edge$date, horizon, frequency = frequency)

  forecast_x_matrix <- stats::window(
    as_mets(ts_data[sys_eq$exogenous_variables]),
    start = forecast_dates$start,
    end = forecast_dates$end
  )

  ##### Produce forecasts
  forecasts <- forecast_sem(
    sys_eq, estimates, restrictions,
    y_matrix, forecast_x_matrix, horizon, balanced_data$freq, forecast_dates,
    approximate = TRUE, probs = NULL
  )

  # Take mean or median forecast
  forecasts <- forecasts[[fill_method]]

  # remove lagged variables
  ts_data <- ts_data[c(sys_eq$endogenous_variables, sys_eq$exogenous_variables)]

  ts_data <- extend_ts_with_forecast(
    ts_data,
    forecasts
  )

  # lag values
  ts_data <- create_lagged_variables(
    ts_data,
    sys_eq$endogenous_variables,
    sys_eq$exogenous_variables,
    sys_eq$predetermined_variables
  )

  ts_data
}
