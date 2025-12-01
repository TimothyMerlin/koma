#' Forecast the Simultaneous Equations Model (SEM)
#'
#' This function produces forecasts for the SEM.
#'
#' @param estimates A `koma_estimate` object (\code{\link{estimate}}) containing
#' the estimates for the simultaneous equations model, as well as a list of time
#' series and a `koma_seq` object (\code{\link{system_of_equations}}) that were
#' used in the estimation.
#' @inheritParams estimate
#' @param ... Additional parameters.
#' @param restrictions List of model constraints. Default is empty.
#' @inheritParams fill_ragged_edge
#'
#' @inheritSection estimate Parallel
#'
#' @return An object of class `koma_forecast`.
#'
#' An object of class `koma_forecast` is a list containing the following
#' elements:
#' \describe{
#'   \item{mean}{Mean point forecasts as a list of time series of class
#'   `koma_ts`.}
#'   \item{median}{Median point forecasts as a list of time series of class
#'   `koma_ts`.}
#'   \item{quantiles}{A list of quantiles, where each element is named
#'   according to the quantile (e.g., "5", "50", "95"), and contains the
#'   forecasts for that quantile. This element is NULL if `quantiles = FALSE`.}
#'   \item{ts_data}{Time-series data set used in forecasting.}
#'   \item{y_matrix}{The Y matrix constructed from the balanced data up to the
#'   current quarter, used for forecasting.}
#'   \item{x_matrix}{The X matrix used for forecasting.}
#' }
#'
#' @details
#' The `forecast` function for SEM uses the estimates from the `koma_estimate`
#' object to produce point forecasts or quantile forecasts based on the
#' `point_forecast` parameter. If `point_forecast$active` is `TRUE`, only point
#' forecasts are generated. If `FALSE`, quantile forecasts are generated and
#' included in the `quantiles` list.
#'
#' Use the \code{\link[=print.koma_forecast]{print}} method to print a
#' the forecast results, use the \code{\link[=plot.koma_forecast]{plot}} method,
#' to visualize the forecasts and prediction intervals.
#'
#' @seealso
#' - For a comprehensive example of using `forecast`, see
#' \code{vignette("koma")}.
#' - Related functions within the package that may be of interest:
#' \code{\link{estimate}}.
#' @export
forecast <- function(estimates, dates, ...,
                     restrictions = NULL, point_forecast = NULL) {
  check_dots_used(...)
  setup_global_progress_handler()

  cli::cli_h1("Forecast")
  UseMethod("forecast")
}

#' @export
forecast.koma_estimate <- function(estimates, dates, ...,
                                   restrictions = NULL, point_forecast = NULL) {
  stopifnot(inherits(estimates, "koma_estimate"))

  dates$current <- iterate_n_periods(dates$forecast$start, -1, 4)
  validate_forecast_input(estimates, dates)
  fo <- new_forecast(estimates, dates, restrictions, point_forecast)
  validate_forecast_output(fo)
}

new_forecast <- function(estimates, dates, restrictions, point_forecast) {
  ts_data <- estimates$ts_data
  stopifnot(inherits(ts_data, "list"))
  stopifnot(sapply(ts_data, function(x) inherits(x, "koma_ts")))
  stopifnot(is_system_of_equations(estimates$sys_eq))

  # Define default options
  default_point_forecast <- list(active = TRUE, central_tendency = "mean")
  # Merge user-provided options with default options
  if (is.null(point_forecast)) {
    point_forecast <- default_point_forecast
  } else {
    point_forecast <- utils::modifyList(default_point_forecast, point_forecast)
  }

  dates <- dates_to_num(dates, frequency = 4)

  if (is.null(dates$forecast$start) ||
    is.null(dates$forecast$start) ||
    is.null(dates$forecast$end) ||
    !is.numeric(dates$forecast$start) ||
    !is.numeric(dates$forecast$end)
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$forecast}:",
      "x" = "{.field start} and {.field end} must be numeric and provided"
    ))
  }

  horizon <- length(seq(dates$forecast$start, dates$forecast$end, by = 1 / 4))

  edge <- detect_edge(
    rate(ts_data[estimates$sys_eq$endogenous_variables]),
    dates$estimation$start,
    dates$current
  )

  ##### Conditional fill of ragged edge up to and including forecast start
  if (edge$date != dates$current) {
    ts_data <- conditional_fill(
      rate(ts_data), estimates$sys_eq, dates, estimates$estimates,
      point_forecast
    )
  }

  ##### Create Lagged Variables
  ts_data <- create_lagged_variables(
    rate(ts_data),
    estimates$sys_eq$endogenous_variables,
    estimates$sys_eq$exogenous_variables,
    estimates$sys_eq$predetermined_variables
  )

  ##### Construct Y and X matrix up to current quarter
  balanced_data <- construct_balanced_data(
    rate(ts_data),
    estimates$sys_eq$endogenous_variables,
    estimates$sys_eq$total_exogenous_variables,
    dates$estimation$start, dates$current,
    state = list(warning_issued = TRUE)
  )

  y_matrix <- balanced_data$y_matrix
  if (!is.null(estimates$sys_eq$exogenous_variables)) {
    x_matrix <- stats::window(
      as_mets(rate(ts_data[estimates$sys_eq$exogenous_variables])),
      start = dates$forecast$start,
      end = dates$forecast$end
    )
  } else {
    x_matrix <- NULL
  }

  ##### Forecast model
  forecasts <- forecast_sem(
    estimates$sys_eq, estimates$estimates, restrictions, y_matrix, x_matrix,
    horizon, balanced_data$freq, dates$forecast, point_forecast
  )

  exogenous_ts_data <- rate(estimates$ts_data[estimates$sys_eq$exogenous_variables])
  exogenous_ts_data <- lapply(exogenous_ts_data, function(x) {
    suppressWarnings(
      stats::window(x,
        start = dates$forecast$start, end = dates$forecast$end
      )
    )
  })

  out <- list()
  out$mean <- c(
    as_ets_list(forecasts$mean, rate(ts_data), central_tendency = "mean"),
    exogenous_ts_data
  )

  out$median <- c(
    as_ets_list(forecasts$median, rate(ts_data), central_tendency = "median"),
    exogenous_ts_data
  )
  if (!is.null(forecasts$quantiles)) {
    out$quantiles <- as_ets_list(forecasts$quantiles, rate(ts_data))
    out$quantiles <- lapply(out$quantiles, function(x) {
      x <- c(x, exogenous_ts_data)
    })
  } else {
    out$quantiles <- NULL
  }

  structure(
    list(
      mean = out$mean,
      median = out$median,
      forecasts = forecasts$forecasts,
      quantiles = out$quantiles,
      ts_data = ts_data,
      y_matrix = y_matrix,
      x_matrix = x_matrix
    ),
    class = "koma_forecast"
  )
}

validate_forecast_input <- function(estimates, dates, ...) {
  # Check if exogenous data extends to forecast start date
  # If exogenous data is shorter then forecast end date the forecast horizon
  # will later be automatically shortened.
  data_too_short <- list()
  for (x in estimates$sys_eq$exogenous_variables) {
    if (stats::tsp(estimates$ts_data[[x]])[2] < dates_to_num(dates$forecast$start, frequency = 4)) {
      data_too_short <- c(data_too_short, x)
    }
  }
  if (length(data_too_short) > 0) {
    cli::cli_abort(
      c(
        "x" = "The following exogenous series do not extend into the forecast period:",
        ">" = paste(data_too_short, collapse = ", "),
        "i" = "Please provide these time series for the entire the forecast period."
      ),
      call = rlang::caller_env()
    )
  }

  # Check if data longer than current quarter
  data_too_long <- list()
  for (x in estimates$sys_eq$endogenous_variables) {
    if (stats::tsp(estimates$ts_data[[x]])[2] > dates_to_num(dates$current, frequency = 4)) {
      data_too_long <- c(data_too_long, x)
    }
  }
  if (length(data_too_long) > 0) {
    cli::cli_abort(
      c(
        "x" = "The following series extend beyond the forecast start date:",
        ">" = paste(data_too_long, collapse = ", "),
        "i" = "Please adjust these time series to fit within the forecast
        period."
      ),
      call = rlang::caller_env()
    )
  }
}

validate_forecast_output <- function(x, ...) {
  x
}

#' Print Method for koma_forecast Objects
#'
#' This function prints the forecasts contained in a `koma_forecast` object as
#' a multivariate time series (mts). Users can specify which variables to print
#' and select either the mean forecast, the median forecast, or specific
#' quantiles of the forecast distribution.
#'
#' @param x A `koma_forecast` object.
#' @param ... Additional parameters.
#' @param variables Optional. A character vector of variable names to print.
#' Default is NULL, which prints all forecasted variables.
#' @param central_tendency Optional. A string specifying the type of forecast
#' to print. Can be "mean", "median", or a quantile name like "q_5", "q_50",
#' "q_95". Default is "mean" if available, otherwise "median", or a specified
#' quantile.
#'
#' @details
#' This function prints the forecasts contained in a `koma_forecast` object.
#' Users can choose to print either the mean forecast, the median forecast, or
#' specific quantiles of the forecast distribution.
#'
#' If `variables` is specified, only the forecasts for those variables are
#' printed.
#' If `central_tendency` is not specified, the function defaults to printing the
#' mean forecast if available, otherwise the median forecast, or a specified
#' quantile.
#'
#' @export
print.koma_forecast <- function(x, ..., variables = NULL,
                                central_tendency = NULL) {
  stopifnot(inherits(x, "koma_forecast"))

  if (is.null(central_tendency)) {
    out <- x[["mean"]]
  } else if (central_tendency %in% c("mean", "median")) {
    out <- x[[central_tendency]]
  } else if (central_tendency %in% names(x$quantiles)) {
    out <- x$quantiles[[central_tendency]]
  } else {
    stop("Please provide a valid `central_tendency`.")
  }

  if (!is.null(variables)) {
    stopifnot(any(is.character(variables), is.vector(variables)))
    out <- out[variables]
  }
  print(as_mets(out))
}

#' @export
format.koma_forecast <- function(x, ...) {
  NextMethod("print")
}

#' @keywords internal
as_ets_list <- function(x, ts_data, central_tendency = NULL) {
  ts_list <- list()

  if (!inherits(x, "list")) {
    x <- list(x)
    names(x) <- central_tendency
  }

  ts_list <- lapply(x, function(y) {
    for (k in seq_len(ncol(y))) {
      colname <- colnames(y)[k]
      new_ts <- y[, colname]
      attributes(new_ts) <- c(
        attributes(new_ts),
        class = list(c("koma_ts", class(new_ts))),
        get_custom_attributes(ts_data[[colname]])
      )
      # do not update non-existing anker (e.g. custom methods)
      if (!is.null(attr(ts_data[[colname]], "anker")) &&
        any(!is.na(attr(ts_data[[colname]], "anker")))) {
        new_ts <- update_anker(ts_data[[colname]], new_ts)
      }
      ts_list <- append(ts_list, list(new_ts))
    }
    names(ts_list) <- colnames(y)

    ts_list
  })

  if (length(ts_list) == 1) {
    ts_list[[1]]
  } else {
    ts_list
  }
}

#' Update anker
#'
#' This function updates the anker to the last observation of the realized
#' data.
#'
#' @param x Time series object of class `koma_ts` with realized / historical
#' data.
#' @param y Time series object of class `koma_ts` with forecast data.
#'
#' @return Return y of class `koma_ts` with updated anker.
#'
#' @keywords internal
update_anker <- function(x, y) {
  stopifnot(inherits(x, "ts"))
  stopifnot(inherits(y, "ts"))

  lvl_x <- level(x)
  attr(y, "anker") <- c(utils::tail(lvl_x, 1), stats::tsp(lvl_x)[2])
  lvl_y <- level(y)

  # start of anker series needs before start of forecast series
  stopifnot(stats::tsp(lvl_x)[1] < stats::tsp(lvl_y)[1])

  # end of anker series needs to be at or after start of forecast series
  stopifnot(stats::tsp(lvl_x)[2] >= stats::tsp(lvl_y)[1])

  lvl_x <- stats::window(lvl_x, end = stats::start(lvl_y))

  # end of anker series needs to align with start of forecast series
  stopifnot(stats::tsp(lvl_x)[2] == stats::tsp(lvl_y)[1])

  y
}
