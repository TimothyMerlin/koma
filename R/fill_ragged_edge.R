#' Fill Ragged Edges in Time Series Data
#'
#' This function fills in the ragged edges in a time series data set using a
#' system of equations model. It iteratively detects edges, estimates the
#' model, and fills the unobserved series using a one-step ahead conditional
#' forecast until the time series is balanced.
#'
#' @param point_forecast A list that contains the following elements:
#'   - `active`: Determines the type of forecast generated.
#' If TRUE, a point forecast is created. If FALSE, a density forecast is
#' returned. Default is TRUE.
#'   - `central_tendency`: A character string indicating which
#' central tendency measure ("mean" or "median") to use for summary statistics.
#' Default is "mean".
#' @inheritParams estimate
#' @inheritParams system_of_equations
#'
#' @return A list containing the updated time series data.
#' @keywords internal
fill_ragged_edge <- function(ts_data, sys_eq,
                             exogenous_variables, dates, point_forecast) {
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
      dates$estimation$start, dates$estimation$end,
      state = list(warning_issued = TRUE)
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
    restrictions <- set_restrictions(
      ts_data, variables_to_restrict,
      start = edge$date,
      end = edge$date
    )

    forecast_dates <- list()
    forecast_dates$start <- iterate_n_periods(edge$date, 1, frequency = 4)
    # forecast end date needs to be different to start to get a matrix
    # because horizon is 1 the end date will be disregarded
    forecast_dates$end <- iterate_n_periods(edge$date, 2, frequency = 4)

    forecast_x_matrix <- stats::window(
      as_mets(ts_data[sys_eq$exogenous_variables]),
      start = forecast_dates$start,
      end = forecast_dates$end
    )

    ##### Produce forecasts
    forecasts <- forecast_sem(
      sys_eq, estimates, restrictions,
      y_matrix, forecast_x_matrix, horizon, balanced_data$freq, forecast_dates,
      point_forecast
    )

    # Take mean or median forecast
    forecasts <- forecasts[[point_forecast$central_tendency]]

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

#' Conditional Fill and Forecast for Time Series Data
#'
#' This function fills missing observations and extends the time series data
#' up to and including the current quarter. The function conditionally forecasts
#' based on the estimates and realized observations.
#'
#' @param point_forecast A list of options used when ragged edge is filled
#' that contains:
#'   - active: Determines the type of forecast generated.
#' If TRUE, a density forecast is created. If FALSE, a point forecast is
#' returned. Default is TRUE.
#'   - central_tendency A character string indicating which central tendency
#' measure ("mean" or "median") to use when point_forecast$active is TRUE.
#' Default is "mean".
#' @inheritParams estimate
#' @inheritParams system_of_equations
#'
#' @return An extended time series list containing the filled and forecasted
#' time series up to the current quarter.
#' @keywords internal
conditional_fill <- function(ts_data, sys_eq, dates,
                             estimates, point_forecast) {
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
    dates$estimation$start, dates$current,
    state = list(warning_issued = TRUE)
  )

  y_matrix <- balanced_data$y_matrix
  edge <- balanced_data$edge

  # Forecast horizon
  frequency <- 4
  horizon <- length(seq(
    iterate_n_periods(edge$date, 1, frequency),
    dates$current,
    by = 1 / frequency
  ))

  # Set variables with observation as restricted
  variables_to_restrict <- endogenous_variables[
    !endogenous_variables %in% edge$variable_names
  ]
  restrictions <- set_restrictions(
    ts_data, variables_to_restrict,
    start = iterate_n_periods(edge$date, 1, frequency = 4),
    end = dates$current
  )

  forecast_dates <- list()
  forecast_dates$start <- iterate_n_periods(edge$date, 1, frequency = 4)
  # forecast end date needs to be different to start to get a matrix
  # because horizon is 1 the end date will be disregarded
  forecast_dates$end <- iterate_n_periods(edge$date, horizon, frequency = 4)

  forecast_x_matrix <- stats::window(
    as_mets(ts_data[sys_eq$exogenous_variables]),
    start = forecast_dates$start,
    end = forecast_dates$end
  )

  ##### Produce forecasts
  forecasts <- forecast_sem(
    sys_eq, estimates, restrictions,
    y_matrix, forecast_x_matrix, horizon, balanced_data$freq, forecast_dates,
    point_forecast
  )

  # Take mean or median forecast
  forecasts <- forecasts[[point_forecast$central_tendency]]

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
