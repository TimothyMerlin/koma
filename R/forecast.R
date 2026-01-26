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
#' @param options Optional settings for forecasting. Use
#' \code{list(approximate = FALSE, probs = NULL, fill = list(method = "mean"))}.
#' Elements:
#' \itemize{
#'   \item \code{approximate}: Logical. If FALSE (default), compute point
#'   forecasts from predictive draws. If TRUE, compute point forecasts from the
#'   mean/median of coefficient draws (fast approximation).
#'   \item \code{probs}: Numeric vector of quantile probabilities. If NULL, no
#'   quantiles are returned.
#'   \item \code{fill$method}: "mean" or "median" used for conditional fill
#'   before forecasting.
#' }
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
#'   according to the quantile (e.g., "q_5", "q_50", "q_95"), and contains the
#'   forecasts for that quantile. This element is NULL if `quantiles = FALSE`.}
#'   \item{ts_data}{Time-series data set used in forecasting.}
#'   \item{y_matrix}{The Y matrix constructed from the balanced data up to the
#'   current quarter, used for forecasting.}
#'   \item{x_matrix}{The X matrix used for forecasting.}
#' }
#'
#' @details
#' The `forecast` function for SEM uses the estimates from the `koma_estimate`
#' object to produce point forecasts and, optionally, quantile forecasts.
#' When \code{options$approximate} is FALSE (default), point forecasts are
#' computed from the predictive draws (with quantiles controlled by
#' \code{options$probs}). When TRUE, point forecasts are computed from the mean
#' and median of the coefficient draws for faster, approximate results.
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
                     restrictions = NULL,
                     options = list(
                       approximate = FALSE,
                       probs = NULL,
                       fill = list(method = "mean")
                     )) {
  check_dots_used(...)
  setup_global_progress_handler()

  cli::cli_h1("Forecast")
  UseMethod("forecast")
}

#' @export
forecast.koma_estimate <- function(estimates, dates, ...,
                                   restrictions = NULL,
                                   options = list(
                                     approximate = FALSE,
                                     probs = NULL,
                                     fill = list(method = "mean")
                                   )) {
  stopifnot(inherits(estimates, "koma_estimate"))

  validate_forecast_input(estimates, dates)
  dates$current <- iterate_n_periods(dates$forecast$start, -1, 4)
  fo <- new_forecast(estimates, dates, restrictions, options)
  validate_forecast_output(fo)
}

new_forecast <- function(estimates, dates, restrictions, options) {
  ts_data <- estimates$ts_data
  stopifnot(inherits(ts_data, "list"))
  stopifnot(sapply(ts_data, function(x) inherits(x, "koma_ts")))
  stopifnot(is_system_of_equations(estimates$sys_eq))

  if (is.null(options)) {
    options <- list()
  }
  if (!is.list(options)) {
    cli::cli_abort("`options` must be a list.")
  }
  approximate <- options$approximate
  if (is.null(approximate)) {
    approximate <- FALSE
  }
  if (!is.logical(approximate) || length(approximate) != 1L || is.na(approximate)) {
    cli::cli_abort("`options$approximate` must be a single logical value.")
  }
  probs <- options$probs
  if (!is.null(probs)) {
    if (!is.numeric(probs) || length(probs) == 0L || anyNA(probs)) {
      cli::cli_abort("`options$probs` must be a numeric vector with at least one value.")
    }
    if (any(probs < 0 | probs > 1)) {
      cli::cli_abort("`options$probs` must be between 0 and 1.")
    }
    if (approximate) {
      cli::cli_abort("`options$approximate = TRUE` cannot be used with `options$probs`.")
    }
  }
  fill_method <- options$fill$method
  if (is.null(fill_method)) {
    fill_method <- "mean"
  }
  fill_method <- match.arg(fill_method, c("mean", "median"))

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
    missing_flags <- vapply(
      estimates$sys_eq$endogenous_variables,
      function(var) stats::tsp(rate(ts_data[[var]]))[2] < dates$current,
      logical(1)
    )
    missing_vars <- estimates$sys_eq$endogenous_variables[missing_flags]
    if (!length(missing_vars)) {
      missing_vars <- edge$variable_names
    }

    edge_date <- dates_to_str(num_to_dates(edge$date, 4), 4)
    current_date <- dates_to_str(num_to_dates(dates$current, 4), 4)
    cli::cli_text("")
    cli::cli_text("Conditional fill detected after {.val {edge_date}}.")
    cli::cli_text("Missing observations for: {.val {missing_vars}}")
    cli::cli_text("Missing values will be conditionally filled up to {.val {current_date}} before forecasting.")

    should_prompt <- interactive() &&
      (!rlang::is_installed("testthat") || !testthat::is_testing())
    if (should_prompt) {
      response <- readline(prompt = "Continue with conditional fill? (y/n): ")
      if (tolower(response) != "y") {
        cli::cli_abort("Conditional fill aborted by user.")
      }
    }

    ts_data <- conditional_fill(
      rate(ts_data), estimates$sys_eq, dates, estimates$estimates,
      fill_method
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
    horizon, balanced_data$freq, dates$forecast, approximate, probs
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
  if (is.null(dates$forecast) ||
    is.null(dates$forecast$start) ||
    is.null(dates$forecast$end) ||
    length(dates$forecast$start) == 0L ||
    length(dates$forecast$end) == 0L
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$forecast}:",
      "x" = "{.field start} and {.field end} must be provided"
    ))
  }
  if (!is.numeric(dates$forecast$start) || !is.numeric(dates$forecast$end)) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$forecast}:",
      "x" = "{.field start} and {.field end} must be numeric"
    ))
  }
  if (!length(dates$forecast$start) %in% c(1L, 2L) ||
    !length(dates$forecast$end) %in% c(1L, 2L)
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$forecast}:",
      "x" = "{.field start} and {.field end} must be length 1 or 2"
    ))
  }
  if (anyNA(dates$forecast$start) ||
    anyNA(dates$forecast$end) ||
    any(!is.finite(dates$forecast$start), na.rm = TRUE) ||
    any(!is.finite(dates$forecast$end), na.rm = TRUE)
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$forecast}:",
      "x" = "{.field start} and {.field end} must be finite"
    ))
  }
  if (length(dates$forecast$start) == 2L &&
    (dates$forecast$start[2] < 1L || dates$forecast$start[2] > 4L)
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$forecast}:",
      "x" = "{.field start} period must be between 1 and 4"
    ))
  }
  if (length(dates$forecast$end) == 2L &&
    (dates$forecast$end[2] < 1L || dates$forecast$end[2] > 4L)
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$forecast}:",
      "x" = "{.field end} period must be between 1 and 4"
    ))
  }

  forecast_start <- dates_to_num(dates$forecast$start, frequency = 4)
  forecast_end <- dates_to_num(dates$forecast$end, frequency = 4)
  if (length(forecast_start) != 1L || length(forecast_end) != 1L) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$forecast}:",
      "x" = "{.field start} and {.field end} must be scalar dates"
    ))
  }
  if (forecast_start > forecast_end) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$forecast}:",
      "x" = "{.field start} must be before {.field end}"
    ))
  }

  current_date <- iterate_n_periods(dates$forecast$start, -1, 4)

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
    if (stats::tsp(estimates$ts_data[[x]])[2] > dates_to_num(current_date, frequency = 4)) {
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

  invisible(NULL)
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
  print(format(
    x,
    ...,
    variables = variables,
    central_tendency = central_tendency
  ))
  invisible(x)
}

#' @export
format.koma_forecast <- function(x, ...,
                                 variables = NULL,
                                 central_tendency = NULL) {
  stopifnot(inherits(x, "koma_forecast"))

  if (is.null(central_tendency)) {
    if (!is.null(x[["mean"]])) {
      out <- x[["mean"]]
    } else if (!is.null(x[["median"]])) {
      out <- x[["median"]]
    } else if (!is.null(x$quantiles) && length(x$quantiles) > 0) {
      out <- x$quantiles[[1]]
    } else {
      stop("No forecasts available to format.")
    }
  } else if (central_tendency %in% c("mean", "median")) {
    out <- x[[central_tendency]]
  } else if (!is.null(x$quantiles) && central_tendency %in% names(x$quantiles)) {
    out <- x$quantiles[[central_tendency]]
  } else {
    stop("Please provide a valid `central_tendency`.")
  }

  if (!is.null(variables)) {
    stopifnot(is.character(variables))
    out <- out[variables]
  }

  as_mets(out)
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
