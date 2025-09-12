#' Set Restrictions for Observed Variables in Time Series Data
#'
#' This function generates a list of restrictions for specified variables in a
#' time series data frame, based on their values between specified start and
#' end dates.
#'
#' @param ts_data A list containing the time series data.
#' @param variables_to_restrict A vector of strings specifying the names of the
#' variables in `ts_data` that should be restricted.
#' @param start The starting date for the restrictions.
#' @param end The ending date for the restrictions.
#'
#' @return A named list of restrictions, where the names are the names of the
#' variables in `variables_to_restrict`. Each element is a list containing
#' `horizon`, a sequence from 1 to the length of `value_at_date`, and `value`,
#' the truncated values of the variable between `start` and
#' `end`.
#' @keywords internal
set_restrictions <- function(ts_data, variables_to_restrict,
                             start, end) {
  restrictions <- lapply(
    variables_to_restrict,
    function(variable_name, ts_data) {
      value_at_date <- suppressWarnings(stats::window(ts_data[[variable_name]],
        start = start, end = end
      ))

      horizons <- seq_len(length(value_at_date))

      variable_restriction <- list(
        horizon = horizons,
        value = value_at_date[horizons]
      )
      return(variable_restriction)
    },
    ts_data
  )
  restrictions <- stats::setNames(restrictions, variables_to_restrict)

  return(restrictions)
}
