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
#' @param restrictions Named list of restrictions for conditional forecasts,
#' one element per endogenous variable, in rates. Default is empty. Each
#' element is one of:
#' \itemize{
#'   \item \code{list(value = , horizon = )}: values for forecast horizons,
#'   e.g. \code{list(value = c(0.5, 0.4), horizon = 1:2)}.
#'   \item \code{list(value = , date = )}: values for forecast periods, given
#'   as \code{c(year, period)}, numeric dates or a list of \code{c(year,
#'   period)}, e.g. \code{list(value = 0.5, date = c(2025, 2))} or
#'   \code{list(value = c(0.5, 0.4), date = c(2025, 2025.25))}.
#'   \item a \code{ts} whose time index is the forecast period, e.g.
#'   \code{ts(c(0.5, 0.4), start = c(2025, 1), frequency = 4)}. Missing
#'   values leave that period unrestricted.
#' }
#' Dates use the frequency of the data: for quarterly data,
#' \code{c(2025, 2)} is the second quarter, for monthly data February. A
#' \code{ts} must have the frequency of the data.
#' @param adjustments Named list of residual adjustments (add-factors), one
#' element per stochastic equation, in rates. Default is empty. An adjustment
#' adds its value to the residual of the equation. The model solves the
#' equation together with all others, so through the contemporaneous
#' coefficients and the lags the adjustment moves other variables and later
#' periods too, and with contemporaneous feedback the adjusted variable
#' itself changes by more or less than the value. Identities cannot be adjusted, and adjustments cannot be combined with
#' \code{restrictions}. Each element is one of:
#' \itemize{
#'   \item \code{list(value = , horizon = )}, \code{list(value = , date = )}
#'   or a \code{ts}, as for \code{restrictions}: the shift in each forecast
#'   period, e.g. \code{list(value = c(0.3, 0.2), horizon = 1:2)}.
#'   \item \code{list(value = , decay = )}: a shift that starts at
#'   \code{value} and decays by the factor \code{decay} (between 0 and 1) per
#'   period until the end of the forecast, e.g.
#'   \code{list(value = 0.5, decay = 0.6)} gives 0.5, 0.3, 0.18, \dots
#'   It starts in the first forecast period, or at a single \code{horizon} or
#'   \code{date}. \code{decay = 0} shifts one period only, \code{decay = 1}
#'   all periods by the same value.
#'   \item \code{list(init = "last", decay = )}: as before, but starting at
#'   the residual of the equation in the period before the forecast, at the
#'   mean of the coefficients (see \code{\link[=residuals.koma_estimate]{residuals}}).
#'   The variables of the equation must be observed in that period; if one
#'   is filled because its series ends earlier, \code{forecast()} fails.
#' }
#' @param options Optional settings for forecasting. Use
#' \code{list(approximate = FALSE, probs = NULL, fill = list(method = "mean"),
#' conditional_innov_method = "projection")}.
#' Elements:
#' \itemize{
#'   \item \code{approximate}: Logical. If FALSE (default), compute point
#'   forecasts from predictive draws. If TRUE, compute point forecasts from the
#'   mean/median of coefficient draws (fast approximation).
#'   \item \code{probs}: Numeric vector of quantile probabilities. If NULL, no
#'   quantiles are returned. When \code{approximate = FALSE} and \code{probs} is
#'   NULL, defaults to \code{setdiff(get_quantiles(), 0.5)}.
#'   \item \code{fill$method}: "mean" or "median" used for conditional fill
#'   before forecasting.
#'   \item \code{conditional_innov_method}: Method for drawing conditional
#'   innovations. One of \code{"projection"} (default) or \code{"eigen"}.
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
#' With \code{adjustments}, the list also contains:
#' \describe{
#'   \item{unadjusted}{The forecast without adjustments (`mean`, `median`
#'   and `quantiles`), computed with the same shocks, so that the
#'   difference to `mean` is the effect of the adjustments.}
#'   \item{adjustments}{The applied adjustments as a named list of time series,
#'   one per adjusted variable, indexed by forecast period.}
#'   \item{adjustments_spec}{The \code{adjustments} as given.}
#' }
#'
#' @examples
#' data("simulated_sem")
#'
#' dates <- list(
#'   current = c(2024, 4),
#'   estimation = simulated_sem$dates$estimation,
#'   forecast = list(start = c(2025, 1), end = c(2025, 4))
#' )
#'
#' ts_data <- simulated_sem$ts_data
#' # Endogenous series must stop at the last observed quarter before forecasting.
#' ts_data[simulated_sem$sys_eq$endogenous_variables] <- lapply(
#'   simulated_sem$sys_eq$endogenous_variables,
#'   function(x) {
#'     stats::window(ts_data[[x]], end = dates$current)
#'   }
#' )
#'
#' set.seed(11)
#' fit <- estimate(
#'   ts_data = ts_data,
#'   sys_eq = simulated_sem$sys_eq,
#'   dates = dates,
#'   options = list(gibbs = list(ndraws = 10))
#' )
#' fc <- forecast(fit, dates = dates)
#' print(fc)
#'
#' @details
#' The `forecast` function for SEM uses the estimates from the `koma_estimate`
#' object to produce point forecasts and, optionally, quantile forecasts.
#' When \code{options$approximate} is FALSE (default), point forecasts are
#' computed from the predictive draws (with quantiles controlled by
#' \code{options$probs}). When TRUE, point forecasts are computed from the mean
#' and median of the coefficient draws for faster, approximate results.
#'
#' Each predictive draw uses the coefficients of one posterior draw and draws
#' new structural shocks for every forecast period, so the quantiles include
#' both parameter and shock uncertainty. The structural shocks are independent
#' across equations, with the variance of the draw. Exogenous variables are
#' taken as given, without uncertainty. Periods before the forecast that are
#' filled because a series ends earlier (ragged edge) are filled once, with
#' the mean or median coefficients, so their uncertainty is not included.
#'
#' The returned `koma_forecast` object keeps forecasts as named lists of
#' `koma_ts` (for `mean`, `median`, and `quantiles`) alongside the input data
#' and matrices used to produce them.
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
                     adjustments = NULL,
                     options = list(
                       approximate = FALSE,
                       probs = NULL,
                       fill = list(method = "mean"),
                       conditional_innov_method = "projection"
                     )) {
  check_dots_used(...)
  setup_global_progress_handler()

  cli::cli_h1("Forecast")
  UseMethod("forecast")
}

#' @rdname forecast
#' @export
forecast.koma_estimate <- function(estimates, dates, ...,
                                   restrictions = NULL,
                                   adjustments = NULL,
                                   options = list(
                                     approximate = FALSE,
                                     probs = NULL,
                                     fill = list(method = "mean"),
                                     conditional_innov_method = "projection"
                                   )) {
  stopifnot(inherits(estimates, "koma_estimate"))

  frequency <- get_single_frequency(estimates$ts_data)
  validate_forecast_input(estimates, dates, frequency = frequency)
  dates$current <- iterate_n_periods(dates$forecast$start, -1, frequency)
  fo <- new_forecast(estimates, dates, restrictions, adjustments, options)
  validate_forecast_output(fo)
}

new_forecast <- function(estimates, dates, restrictions, adjustments, options) {
  adjustments_spec <- adjustments
  ts_data <- estimates$ts_data
  frequency <- get_single_frequency(ts_data)
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
  if (is.null(probs) && !approximate) {
    probs <- setdiff(get_quantiles(), 0.5)
  }
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
  conditional_innov_method <- options$conditional_innov_method
  if (is.null(conditional_innov_method)) {
    conditional_innov_method <- "projection"
  }
  conditional_innov_method <- match.arg(conditional_innov_method, c("projection", "eigen"))

  dates <- dates_to_num(dates, frequency = frequency)

  if (is.null(dates$forecast$start) ||
    is.null(dates$forecast$end) ||
    !is.numeric(dates$forecast$start) ||
    !is.numeric(dates$forecast$end)) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$forecast}:",
      "x" = "{.field start} and {.field end} must be numeric and provided"
    ))
  }

  horizon <- length(seq(dates$forecast$start, dates$forecast$end, by = 1 / frequency))

  restrictions <- dates_to_horizon(
    restrictions, dates$forecast$start, horizon, frequency
  )
  restrictions <- validate_restrictions(
    restrictions,
    estimates$sys_eq$endogenous_variables,
    horizon
  )
  # Decaying adjustments are expanded in forecast_sem(), to the possibly
  # shortened horizon; here they are only checked, before the fill below.
  adjustments <- dates_to_horizon(
    adjustments, dates$forecast$start, horizon, frequency,
    what = "adjustment"
  )
  validate_adjustments(
    adjustments,
    estimates$sys_eq$endogenous_variables,
    names(estimates$sys_eq$identities),
    horizon
  )
  # validate_adjustments() has warned about them, so drop them here
  adjustments <- adjustments[
    names(adjustments) %in% estimates$sys_eq$endogenous_variables
  ]
  check_last_residuals(adjustments, estimates, dates$current, frequency)

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

    edge_date <- dates_to_str(num_to_dates(edge$date, frequency), frequency)
    current_date <- dates_to_str(num_to_dates(dates$current, frequency), frequency)
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
    dates$estimation$start, dates$current
  )

  y_matrix <- balanced_data$y_matrix
  adjustments <- last_residuals(
    adjustments, estimates, y_matrix, balanced_data$x_matrix
  )
  if (length(estimates$sys_eq$exogenous_variables) > 0) {
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
    horizon, balanced_data$freq, dates$forecast, approximate, probs,
    conditional_innov_method,
    adjustments = adjustments
  )

  exogenous_ts_data <- rate(estimates$ts_data[estimates$sys_eq$exogenous_variables])
  exogenous_ts_data <- lapply(exogenous_ts_data, function(x) {
    suppressWarnings(
      stats::window(x,
        start = dates$forecast$start, end = dates$forecast$end
      )
    )
  })

  as_output <- function(forecasts) {
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
    out
  }

  out <- as_output(forecasts)
  result <- list(
    mean = out$mean,
    median = out$median,
    forecasts = forecasts$forecasts,
    quantiles = out$quantiles,
    ts_data = ts_data,
    y_matrix = y_matrix,
    x_matrix = x_matrix
  )

  if (!is.null(forecasts$adjustment_matrix)) {
    result$unadjusted <- as_output(forecasts$unadjusted)
    # the applied adjustment of each adjusted variable by forecast quarter
    result$adjustments <- lapply(names(adjustments), function(variable) {
      stats::ts(
        unname(forecasts$adjustment_matrix[variable, ]),
        start = dates$forecast$start,
        frequency = frequency
      )
    })
    names(result$adjustments) <- names(adjustments)
    result$adjustments_spec <- adjustments_spec
  }

  structure(result, class = "koma_forecast")
}

validate_forecast_input <- function(estimates, dates, frequency = 4, ...) {
  validate_date_range(dates, "forecast", frequency = frequency)

  current_date <- iterate_n_periods(dates$forecast$start, -1, frequency)

  # Check if exogenous data extends to forecast start date
  # If exogenous data is shorter then forecast end date the forecast horizon
  # will later be automatically shortened.
  data_too_short <- list()
  for (x in estimates$sys_eq$exogenous_variables) {
    if (stats::tsp(estimates$ts_data[[x]])[2] < dates_to_num(dates$forecast$start, frequency = frequency)) {
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
    if (stats::tsp(estimates$ts_data[[x]])[2] > dates_to_num(current_date, frequency = frequency)) {
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
#' @param digits Optional. Integer number of decimal digits to round the
#' printed output. Default is 4.
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
#' Printing converts the selected forecast list to an `mts` for readability; the
#' underlying `koma_forecast` object remains a list of `koma_ts`.
#'
#' @return Invisibly returns `x`, the original `koma_forecast` object (a list
#'   of `koma_ts` objects, one per forecasted variable). Called primarily for
#'   its side effect of printing the selected forecast as a multivariate time
#'   series (`mts`) to the console.
#'
#' @export
print.koma_forecast <- function(x, ..., variables = NULL,
                                central_tendency = NULL,
                                digits = 4) {
  stopifnot(inherits(x, "koma_forecast"))
  print(format(
    x,
    ...,
    variables = variables,
    central_tendency = central_tendency,
    digits = digits
  ))
  invisible(x)
}

#' @export
format.koma_forecast <- function(x, ...,
                                 variables = NULL,
                                 central_tendency = NULL,
                                 digits = 4) {
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

  formatted <- as_mets(out)
  if (!is.null(digits)) {
    formatted <- round(formatted, digits = digits)
  }
  formatted
}

#' Summary for koma_forecast Objects
#'
#' Prints a summary table for forecast horizons, including available central
#' tendencies (mean/median) and quantiles.
#'
#' @param object A `koma_forecast` object.
#' @param ... Unused.
#' @param variables Optional character vector to filter variables.
#' @param horizon Optional numeric or character vector selecting forecast horizon.
#' @param digits Number of digits to round numeric values. Default is 3.
#'
#' @return Invisibly returns `object`.
#' @export
summary.koma_forecast <- function(object,
                                  ...,
                                  variables = NULL,
                                  horizon = NULL,
                                  digits = 3) {
  if (!inherits(object, "koma_forecast")) {
    cli::cli_abort("`object` must be a koma_forecast.")
  }

  mean_list <- object$mean
  median_list <- object$median
  quantiles_list <- object$quantiles

  normalize_quantiles <- function(quantiles) {
    if (is.null(quantiles) || !length(quantiles)) {
      return(list())
    }
    if (!is.null(names(quantiles))) {
      probs <- vapply(names(quantiles), parse_quantile_name, numeric(1))
      if (any(!is.na(probs))) {
        return(quantiles)
      }
    }
    is_ts_list <- all(vapply(quantiles, function(x) inherits(x, "ts"), logical(1)))
    if (is_ts_list) {
      return(list(quantile = quantiles))
    }
    quantiles
  }

  quantiles_list <- normalize_quantiles(quantiles_list)

  base_list <- if (!is.null(mean_list) && length(mean_list)) {
    mean_list
  } else if (!is.null(median_list) && length(median_list)) {
    median_list
  } else if (length(quantiles_list)) {
    quantiles_list[[1]]
  } else {
    NULL
  }

  if (is.null(base_list) || !length(base_list)) {
    cli::cli_abort("No forecasts available in `object`.")
  }

  if (is.null(variables)) {
    variables <- names(base_list)
  }
  if (!length(variables)) {
    cli::cli_abort("`variables` must contain at least one variable name.")
  }
  missing_vars <- setdiff(variables, names(base_list))
  if (length(missing_vars) > 0L) {
    cli::cli_abort(c(
      "Variable not found in forecasts:",
      ">" = missing_vars
    ))
  }

  has_mean <- !is.null(mean_list) && length(mean_list)
  has_median <- !is.null(median_list) && length(median_list)

  quantile_names <- names(quantiles_list)
  quantile_probs <- vapply(quantile_names, parse_quantile_name, numeric(1))
  quantile_order <- order(is.na(quantile_probs), quantile_probs, seq_along(quantile_names))
  quantile_names <- quantile_names[quantile_order]
  quantile_probs <- quantile_probs[quantile_order]
  quantile_labels <- vapply(seq_along(quantile_names), function(i) {
    if (!is.na(quantile_probs[i])) {
      paste0(format(quantile_probs[i] * 100, trim = TRUE), "%")
    } else {
      quantile_names[i]
    }
  }, character(1))

  value_at <- function(series, idx) {
    if (is.null(series) || !inherits(series, "ts")) {
      return(NA_real_)
    }
    if (idx > length(series)) {
      return(NA_real_)
    }
    series[idx]
  }

  for (i in seq_along(variables)) {
    variable <- variables[[i]]
    mean_ts <- if (has_mean) mean_list[[variable]] else NULL
    median_ts <- if (has_median) median_list[[variable]] else NULL
    quantile_ts <- if (length(quantiles_list)) {
      lapply(quantiles_list, function(q) q[[variable]])
    } else {
      list()
    }

    base_ts <- if (!is.null(mean_ts)) {
      mean_ts
    } else if (!is.null(median_ts)) {
      median_ts
    } else if (length(quantile_ts)) {
      quantile_ts[[1]]
    } else {
      NULL
    }

    if (is.null(base_ts)) {
      next
    }

    raw_labels <- format(stats::time(base_ts), trim = TRUE)
    display_labels <- format_ts_time(base_ts)
    idx <- summary_forecast_resolve_horizon(horizon, display_labels, raw_labels)

    rows <- lapply(idx, function(j) {
      row <- c(display_labels[j])
      if (has_mean) {
        row <- c(row, summary_forecast_format_num(value_at(mean_ts, j), digits))
      }
      if (has_median) {
        row <- c(row, summary_forecast_format_num(value_at(median_ts, j), digits))
      }
      if (length(quantile_ts)) {
        quant_vals <- vapply(quantile_names, function(qn) {
          summary_forecast_format_num(value_at(quantile_ts[[qn]], j), digits)
        }, character(1))
        row <- c(row, quant_vals)
      }
      row
    })

    if (!length(rows)) {
      next
    }
    row_mat <- do.call(rbind, rows)

    col_names <- c(variable)
    if (has_mean) col_names <- c(col_names, "Mean")
    if (has_median) col_names <- c(col_names, "Median")
    if (length(quantile_labels)) col_names <- c(col_names, quantile_labels)

    summary_forecast_write_table(col_names, row_mat)
  }

  note_parts <- character(0)
  if (has_mean) note_parts <- c(note_parts, "Mean")
  if (has_median) note_parts <- c(note_parts, "Median")
  if (length(quantile_labels)) note_parts <- c(note_parts, "Quantiles")
  if (length(note_parts)) {
    cat(paste(note_parts, collapse = ", "), "\n")
  }

  invisible(object)
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
