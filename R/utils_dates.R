#' Convert Date to Numeric Format
#'
#' This function converts a vector, list, or 2-element numeric vector of years
#' and quarters to a numeric representation. The format is
#' `year + (quarter - 1) / frequency`, where the `frequency` is the number of
#' periods in a year (e.g., 4 for quarters).
#'
#' @param x A numeric vector of length 2 (year, quarter), or a list of such
#' vectors. For a single year, pass just the year as a scalar.
#' @param frequency A numeric value indicating the number of periods in a year
#' (e.g., 4 for quarters, 12 for months).
#' @param ... Additional arguments passed to other methods.
#' @return A numeric representation of the date(s).
#' @keywords internal
dates_to_num <- function(x, frequency, ...) {
  UseMethod("dates_to_num")
}

#' @export
dates_to_num.double <- function(x, frequency, ...) {
  if (length(x) == 1L) {
    return(x)
  }
  x[1L] + (x[2L] - 1) / frequency
}

#' @export
dates_to_num.NULL <- function(x, frequency, ...) {
  x
}

#' @export
dates_to_num.list <- function(x, frequency, ...) {
  lapply(x, dates_to_num, frequency)
}

#' Convert Dates to String Format
#'
#' This function converts dates represented as a two-element numeric vector
#' (year, period), Date, POSIXct, or a list of these, to a character.
#' For quarterly data (frequency = 4), returns "YYYY Qn".
#' For monthly data (frequency = 12), returns "YYYY-MM".
#' Otherwise, returns "YYYY-MM-DD".
#'
#' @param x A numeric vector of length 2 (year, period), a Date,
#'   POSIXct, or a list of these.
#' @param frequency An integer number of periods per year
#'   (e.g., 4 = quarters, 12 = months).
#' @param ... Additional arguments passed to methods.
#' @return A character string or a list of character strings.
#' @keywords internal
dates_to_str <- function(x, frequency, ...) {
  UseMethod("dates_to_str")
}

#' @export
dates_to_str.NULL <- function(x, frequency, ...) {
  x
}

#' @export
dates_to_str.Date <- function(x, frequency, ...) {
  yr <- format(x, "%Y")
  mo <- as.integer(format(x, "%m"))
  if (frequency == 4L) {
    qtr <- (mo - 1L) %/% 3L + 1L
    paste0(yr, " Q", qtr)
  } else if (frequency == 12L) {
    paste0(yr, "-", sprintf("%02d", mo))
  } else {
    format(x, "%Y-%m-%d")
  }
}

#' @export
dates_to_str.POSIXct <- function(x, frequency, ...) {
  dates_to_str.Date(as.Date(x), frequency, ...)
}

#' @export
dates_to_str.double <- function(x, frequency, ...) {
  # if it's a singleton, just return it
  if (length(x) == 1L) {
    return(as.character(x))
  }
  year <- x[1L]
  period <- x[2L]
  if (frequency == 4L) {
    paste0(year, " Q", period)
  } else {
    paste0(year, "-", sprintf("%02d", period))
  }
}

#' @export
dates_to_str.list <- function(x, frequency, ...) {
  lapply(x, dates_to_str, frequency)
}

#' Convert Numeric to Date Format
#'
#' This function converts a numeric representation of a date back to a vector
#' or list of vectors with year and period (e.g., quarter or month).
#' The function supports input as a single numeric value, or a list of numeric
#' values. The numeric representation should match the format produced by
#' `dates_to_num`.
#'
#' @param x A numeric value or list of numeric values representing the date(s).
#' If `x` is a single numeric value, it should be in the format produced by
#' `dates_to_num`.
#' @param frequency A numeric value indicating the number of periods in a year
#' (e.g., 1 for years, 4 for quarters, 12 for months).
#' @param ... Additional arguments passed to other methods.
#' @return A numeric vector of length 2 (year, period) or a list of such
#' vectors.
#' @keywords internal
num_to_dates <- function(x, frequency, ...) {
  UseMethod("num_to_dates")
}

#' @export
num_to_dates.double <- function(x, frequency, ...) {
  if (length(x) == 2L) {
    return(x)
  }

  year <- floor(x)
  period <- round((x - year) * frequency + 1)
  c(year, period)
}

#' @export
num_to_dates.NULL <- function(x, frequency, ...) {
  x
}

#' @export
num_to_dates.list <- function(x, frequency, ...) {
  lapply(x, num_to_dates, frequency)
}

#' Add Periods to Numeric Date Representation
#'
#' This function iterates a numeric date representation by a given number of
#' periods (e.g., years, quarters or months). It accepts a date in either vector
#' format (year, period) or its numeric representation and adds the specified
#' number of periods to it.
#'
#' @param x A numeric value or a 2-element vector (year, period). The vector
#' should represent a year and a period (e.g., quarter or month).
#' @param n The number of periods to add. This can be positive or negative.
#' @param frequency A numeric value indicating the number of periods in a year
#' (e.g., 1, for years, 4 for quarters, 12 for months).
#' @return A numeric representation of the date after adding the periods.
#' @keywords internal
iterate_n_periods <- function(x, n, frequency) {
  if (length(x) == 2L) {
    x <- x[1L] + (x[2L] - 1) / frequency
  }

  n <- n / frequency
  x + n
}

#' Format ts Time Labels
#'
#' For quarterly data (frequency = 4), returns "YYYY Qn".
#' For monthly data (frequency = 12), returns "YYYY-MM".
#' Otherwise, falls back to the numeric time values.
#'
#' @param x A `ts` object.
#' @return A character vector of formatted time labels.
#' @keywords internal
format_ts_time <- function(x) {
  if (!inherits(x, "ts")) {
    cli::cli_abort("`x` must be a ts object.")
  }

  freq <- stats::frequency(x)
  times <- stats::time(x)

  if (freq == 4L || freq == 12L) {
    return(vapply(times, function(t) {
      dates_to_str(num_to_dates(t, freq), freq)
    }, character(1)))
  }

  format(times, trim = TRUE)
}
