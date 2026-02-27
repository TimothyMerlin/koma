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
  validate_model_evaluation_input(
    sys_eq, variables, horizon, ts_data, dates, ...
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
  # Initialize error accumulation
  errors <- matrix(0, ncol = length(variables), nrow = horizon)
  colnames(errors) <- variables

  # Collect all iteration parameters
  params <- list()
  n_iterations <- 0

  dates <- dates_to_num(dates, frequency = 4)
  dates$in_sample$end <- iterate_n_periods(dates$forecast$start, -1, frequency = 4)

  while (
    iterate_n_periods(dates$forecast$start, horizon - 1, frequency = 4) <=
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
      iterate_n_periods(temp_dates$forecast$start, horizon - 1, frequency = 4)
    temp_dates$current <- temp_dates$in_sample$end

    # Store the parameters for this iteration
    params[[n_iterations + 1]] <- list(
      ts_data = ts_data_temp,
      dates = temp_dates
    )

    # advance one quarter
    dates$forecast$start <- iterate_n_periods(temp_dates$forecast$start, 1, frequency = 4)
    dates$in_sample$end <- iterate_n_periods(dates$in_sample$end, 1, frequency = 4)
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

validate_model_evaluation_input <- function(
  sys_eq, variables, horizon, ts_data, dates, ...
) {
  if (is.null(variables)) {
    variables <- sys_eq$endogenous_variables
  } else {
    missing_vars <- variables[
      !(variables %in% names(ts_data))
    ]

    if (length(missing_vars) > 0) {
      cli::cli_abort(c(
        "x" = "The following variables were not found in ts_data:",
        ">" = paste(missing_vars, collapse = ", ")
      ), call = rlang::caller_env())
    }
  }
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
    approximate = approximate
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
  realized <- realized[, variables]
  forecasts <- forecasts[, variables]
  errors <- as.data.frame((realized - forecasts)^2)
  colnames(errors) <- variables

  errors
}
