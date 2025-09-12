#' Extend Time Series List with Forecast Values at New Dates
#'
#' This function takes a named list of original time series and extends
#' each series with forecast values from a multi-time series (mts) only
#' at dates not already in the original time series.
#'
#' @param ts_list A named list of original time series objects.
#' @param forecast_mts A multi-time series object with forecast values.
#' The column names should match the names in `ts_list`.
#'
#' @details The function will extend the time series in `ts_list` by appending
#' forecast values from `forecast_mts`. The extension will occur only for dates
#' that do not already exist in the original time series.
#'
#' @return A named list of time series objects, each extended with forecast
#' values for new dates only. For time series whose names are not present in
#' `forecast_mts`, the original time series is returned unmodified.
#' @keywords internal
extend_ts_with_forecast <- function(ts_list, forecast_mts) {
  # Initialize an empty list to store extended time series
  extended_ts_list <- list()

  # Loop through each named time series in ts_list
  for (name in names(ts_list)) {
    # Retrieve the original time series
    original_ts <- ts_list[[name]]

    # Validate that the name exists in the forecast mts
    if (!name %in% colnames(forecast_mts)) {
      extended_ts_list[[name]] <- original_ts
      next
    }

    # Retrieve the forecast values from mts
    forecast_values <- forecast_mts[, name]

    # Generate time points for original and forecast series
    orig_time <- seq(stats::tsp(original_ts)[1],
      by = 1 / stats::frequency(original_ts),
      length.out = length(original_ts)
    )

    forecast_time <- seq(stats::tsp(forecast_mts)[1],
      by = 1 / stats::frequency(forecast_mts),
      length.out = length(forecast_values)
    )

    # Find forecast values that are not in original time series
    new_forecast_values <- forecast_values[!(forecast_time %in% orig_time)]
    new_forecast_time <- forecast_time[!(forecast_time %in% orig_time)]

    # Combine values and time points
    combined_values <- c(as.vector(original_ts), as.vector(new_forecast_values))
    combined_time <- c(orig_time, new_forecast_time)

    # Sort combined_time and rearrange combined_values accordingly
    sort_index <- order(combined_time)
    sorted_values <- combined_values[sort_index]

    # Create new extended time series
    extended_ts <-
      stats::ts(sorted_values,
        start = min(combined_time),
        frequency = stats::frequency(original_ts)
      )
    attributes(extended_ts) <- c(
      attributes(extended_ts),
      class = list(class(original_ts)),
      get_custom_attributes(original_ts)
    )

    # Add to the list
    extended_ts_list[[name]] <- extended_ts
  }

  return(extended_ts_list)
}
