#' Calculate Out-of-Sample RMSE for a Specified Horizon
#'
#' @description
#' Computes the Root Mean Square Error (RMSE) of a model for a given prediction
#' horizon, incrementing the in-sample data by a quarter for each calculation
#' until the specified horizon equals the end date in the forecast period.
#'
#' @param variables A character vector of name(s) of the stochastic
#' endogenous variable for which the forecast error(s) should be calculated. If
#' NULL it is calculated for all variables.
#' @param horizon The forecast horizon in quarters up to which the RMSE should
#' be calculated.
#' @param ts_data time series data set, must include data until end date of
#' forecasting period.
#' @param evaluate_on_levels Boolean, if TRUE RMSE is calculated on levels if
#' FALSE on growth rates.
#' @inheritParams estimate
#' @inheritParams forecast
#' @param options Optional settings for model evaluation. Use
#' \code{list(gibbs = list(), summary = "mean", approximate = FALSE)}. Elements:
#' \itemize{
#'   \item \code{gibbs}: Gibbs sampler settings (see
#'   \link[=get_default_gibbs_spec]{Gibbs Sampler Specifications}).
#'   \item \code{summary}: "mean" or "median" point forecast used for RMSE.
#'   \item \code{approximate}: Logical; if TRUE, use the fast approximate
#'   point forecast (mean/median of coefficient draws).
#' }
#'
#' @details
#' The function initiates the RMSE calculation from `dates$forecast$start` and
#' continues until `dates$forecast$start + horizon` equals `dates$forecast$end`.
#' In each iteration, a quarter is added to both the in-sample data and to
#' `dates$forecast$start`.
#'
#' @return DataFrame containing the RMSE of the selected Variables up to the
#' desired horizon.
#'
#' @examples
#' data("simulated_sem")
#'
#' dates <- list(
#'   estimation = list(start = c(1977, 1), end = c(2018, 4)),
#'   forecast = list(start = c(2023, 2), end = c(2023, 3))
#' )
#'
#' rmse <- model_evaluation(
#'   sys_eq = simulated_sem$sys_eq,
#'   variables = c("consumption", "investment"),
#'   horizon = 1,
#'   ts_data = simulated_sem$ts_data,
#'   dates = dates,
#'   evaluate_on_levels = TRUE,
#'   options = list(gibbs = list(ndraws = 10), summary = "mean")
#' )
#' head(rmse)
#'
#' @export
model_evaluation <- function(sys_eq, variables,
                             horizon, ts_data, dates, ...,
                             evaluate_on_levels = TRUE,
                             options = list(
                               gibbs = list(),
                               summary = "mean",
                               approximate = FALSE
                             ),
                             restrictions = NULL) {
  check_dots_used(...)
  variables <- validate_model_evaluation_input(
    sys_eq, variables, horizon, ts_data, dates, evaluate_on_levels
  )
  setup_global_progress_handler()

  equation_settings <- sys_eq$equation_settings[sys_eq$stochastic_equations]
  if (is.null(options)) {
    options <- list()
  }
  if (!is.list(options)) {
    cli::cli_abort("`options` must be a list.")
  }
  gibbs_options <- options$gibbs
  if (is.null(gibbs_options)) {
    gibbs_options <- list()
  }
  set_gibbs_settings(gibbs_options, equation_settings)

  summary <- options$summary
  if (is.null(summary)) {
    summary <- "mean"
  }
  summary <- match.arg(summary, c("mean", "median"))
  approximate <- options$approximate
  if (is.null(approximate)) {
    approximate <- FALSE
  }
  if (!is.logical(approximate) || length(approximate) != 1L || is.na(approximate)) {
    cli::cli_abort("`options$approximate` must be a single logical value.")
  }

  out <- new_model_evaluation(
    sys_eq, variables,
    horizon, ts_data, dates,
    evaluate_on_levels,
    summary,
    approximate,
    restrictions
  )

  out
}

new_model_evaluation <- function(sys_eq, variables,
                                 horizon, ts_data, dates,
                                 evaluate_on_levels,
                                 summary,
                                 approximate,
                                 restrictions) {
  frequency <- get_single_frequency(ts_data)
  # Initialize error accumulation
  errors <- matrix(0, ncol = length(variables), nrow = horizon)
  colnames(errors) <- variables

  # Collect all iteration parameters
  params <- list()
  n_iterations <- 0

  dates <- dates_to_num(dates, frequency = frequency)
  dates$in_sample$end <- iterate_n_periods(dates$forecast$start, -1, frequency = frequency)

  while (
    iterate_n_periods(dates$forecast$start, horizon - 1, frequency = frequency) <=
      dates$forecast$end
  ) {
    temp_dates <- dates

    # Prepare the in-sample data window
    ts_data_temp <- ts_data
    ts_data_temp[sys_eq$endogenous_variables] <-
      lapply(sys_eq$endogenous_variables, function(x) {
        stats::window(ts_data_temp[[x]], end = temp_dates$in_sample$end)
      })

    if (dates$in_sample$end <= dates$estimation$end) {
      # If the chosen end date for in_sample is less than or equal to the
      # estimation end date, use the last in_sample date as the end date for
      # estimation. Otherwise, skip the estimation unless it's the first
      # iteration.

      temp_dates$estimation$end <- dates$in_sample$end
    }

    temp_dates$forecast$end <-
      iterate_n_periods(temp_dates$forecast$start, horizon - 1, frequency = frequency)
    temp_dates$current <- temp_dates$in_sample$end

    # Store the parameters for this iteration
    params[[n_iterations + 1]] <- list(
      ts_data = ts_data_temp,
      dates = temp_dates
    )

    # advance one quarter
    dates$forecast$start <- iterate_n_periods(temp_dates$forecast$start, 1, frequency = frequency)
    dates$in_sample$end <- iterate_n_periods(dates$in_sample$end, 1, frequency = frequency)
    n_iterations <- n_iterations + 1
  }

  estimates <- NULL

  p <- progressr::progressor(steps = length(params))

  for (param in params) {
    p(amount = 0)

    out <- run_model_iteration(
      param, summary, approximate,
      variables, restrictions, sys_eq,
      evaluate_on_levels, ts_data, estimates
    )
    estimates <- out$estimates
    errors <- errors + out$error

    p(amount = 1)
  }

  # Compute RMSE
  errors <- sqrt(errors / n_iterations)
  errors <- as.data.frame(errors)

  errors
}

#' Validate Model Evaluation Input
#'
#' @inheritParams model_evaluation
#' @param call The environment from which the error is called.
#'
#' @return The variables to evaluate: `variables`, or all endogenous variables
#' if `variables` is `NULL`.
#' @keywords internal
validate_model_evaluation_input <- function(sys_eq, variables, horizon,
                                            ts_data, dates, evaluate_on_levels,
                                            call = rlang::caller_env()) {
  if (!inherits(sys_eq, "koma_seq")) {
    cli::cli_abort(
      "`sys_eq` must be of class 'koma_seq', not {.obj_type_friendly {sys_eq}}.",
      call = call
    )
  }
  if (!is.list(ts_data)) {
    cli::cli_abort(
      "`ts_data` must be a list, not {.obj_type_friendly {ts_data}}.",
      call = call
    )
  }

  if (is.null(variables)) {
    variables <- sys_eq$endogenous_variables
  } else {
    if (!is.character(variables) || length(variables) == 0L || anyNA(variables)) {
      cli::cli_abort(
        "`variables` must be NULL or a character vector of variable names.",
        call = call
      )
    }

    missing_vars <- variables[
      !(variables %in% names(ts_data))
    ]

    if (length(missing_vars) > 0) {
      cli::cli_abort(c(
        "x" = "The following variables were not found in ts_data:",
        ">" = paste(missing_vars, collapse = ", ")
      ), call = call)
    }

    not_endogenous <- setdiff(variables, sys_eq$endogenous_variables)
    if (length(not_endogenous) > 0) {
      cli::cli_abort(c(
        "x" = "Only endogenous variables can be evaluated:",
        ">" = paste(not_endogenous, collapse = ", ")
      ), call = call)
    }
  }

  if (!is.numeric(horizon) || length(horizon) != 1L || !is.finite(horizon) ||
    horizon < 1 || horizon != round(horizon)) {
    cli::cli_abort("`horizon` must be a single positive whole number.", call = call)
  }

  if (!is.logical(evaluate_on_levels) || length(evaluate_on_levels) != 1L ||
    is.na(evaluate_on_levels)) {
    cli::cli_abort("`evaluate_on_levels` must be TRUE or FALSE.", call = call)
  }

  frequency <- get_single_frequency(ts_data)
  validate_date_range(dates, "estimation", frequency = frequency, call = call)
  validate_date_range(dates, "forecast", frequency = frequency, call = call)

  # The rolling evaluation needs at least one full horizon inside the forecast
  # window; otherwise no iteration runs and the RMSE is NaN.
  forecast_start <- dates_to_num(dates$forecast$start, frequency = frequency)
  forecast_end <- dates_to_num(dates$forecast$end, frequency = frequency)
  if (iterate_n_periods(forecast_start, horizon - 1, frequency = frequency) > forecast_end) {
    cli::cli_abort(c(
      "x" = "`horizon` ({horizon}) does not fit into {.field dates$forecast}.",
      "i" = "The forecast window must span at least {horizon} period{?s}."
    ), call = call)
  }

  variables
}

run_model_iteration <- function(param, summary, approximate,
                                variables, restrictions, sys_eq,
                                evaluate_on_levels, realized, estimates) {
  # Perform estimation if necessary
  ragged <- param$dates$in_sample$end <= param$dates$estimation$end
  if (ragged || is.null(estimates)) {
    estimates <- estimate.list(
      param$ts_data, sys_eq, param$dates
    )
  }

  forecasts <- forecast.koma_estimate(
    estimates, param$dates,
    restrictions = restrictions,
    options = list(approximate = approximate)
  )

  if (evaluate_on_levels) {
    # Convert growth rates to level
    forecasts <- as_mets(
      level(forecasts[[summary]])
    )
    realized <- as_mets(level(realized))
  } else {
    forecasts <- as_mets(
      rate(forecasts[[summary]])
    )
    realized <- as_mets(rate(realized))
  }

  forecasts <- stats::window(forecasts,
    start = param$dates$forecast$start,
    end = param$dates$forecast$end
  )
  realized <- stats::window(realized,
    start = param$dates$forecast$start,
    end = param$dates$forecast$end
  )

  error <- calculate_error(forecasts, realized, variables)

  list(error = error, estimates = estimates)
}


#' Calculate the Forecast Error
#'
#' This function calculates the the desired forecast errors for the
#' desired variables given the true values and the forecasts
#'
#' @param forecasts A mets-object containing the forecasts.
#' @param realized A mets-object containing the true values.
#' @param variables A character vector of name(s) of the stochastic
#' endogenous variable for which the forecast error(s) should be calculated.
#'
#' @return List of errors
#' @keywords internal
calculate_error <- function(forecasts, realized, variables) {
  realized <- realized[, variables, drop = FALSE]
  forecasts <- forecasts[, variables, drop = FALSE]
  errors <- as.data.frame((realized - forecasts)^2)
  colnames(errors) <- variables

  errors
}
