#' Prepare Data for Plotting
#'
#' This function prepares time series data by converting the data from wide to
#' long format.
#'
#' @param mts_list A list with named multivariate time series objects: growth,
#' level and / or growth_annual.
#' @param start A numeric value representing the forecast start date.
#'
#' @return A single tibble in long format combining growth rates, annualized
#' growth rates, and level data.
#' @keywords internal
prepare_data_to_plot <- function(mts_list, start) {
  # Transform the metrics to long format and concatenate
  df_long <- do.call(rbind, lapply(names(mts_list), function(type) {
    df <- to_long(
      mts_list[[type]],
      start
    )
    df$data_type <- type
    return(df)
  }))

  # Find the earliest forecast date to create continuous lines in the plot.
  # Plotly requires this for lines with different colors (in-sample &
  # out-of-sample).
  first_out_of_sample <- min(subset(
    df_long,
    df_long$data_type == "level" & df_long$sample_status != "in_sample"
  )$dates)

  # Extract the first out-of-sample level data for the identified date.
  out_of_sample_level <- subset(
    df_long,
    df_long$data_type == "level" &
      df_long$sample_status != "in_sample" &
      df_long$dates == first_out_of_sample
  )

  # Create new data rows for 'base_forecast' or 'conditional_forecast' using
  # the last in-sample level data. This ensures plot continuity.
  forecast_levels <- transform(
    out_of_sample_level,
    sample_status = "in_sample"
  )

  # Append the new forecast data to the original dataframe.
  df_long <- rbind(df_long, forecast_levels)
  df_long <- df_long[order(df_long$dates), ]

  df_long$data_type <- factor(df_long$data_type)

  return(df_long)
}

#' Convert Time Series from Wide to Long Format
#'
#' This function returns the data in long format.
#'
#' @param mts A multivariate time series object.
#' @inheritParams prepare_data_to_plot
#'
#' @return A data frame in long format containing the original data along
#' with the sample status, dates, and frame identifiers.
#' @keywords internal
to_long <- function(mts, start) {
  sample_status <-
    ifelse(stats::time(mts) < start,
      "in_sample", "forecast"
    )

  sample_status <- factor(sample_status)

  start <- num_to_dates(start, frequency = 4)
  date_str <- paste0(start[1], "-Q", start[2])

  df <- as.data.frame(as.matrix(mts))
  df$dates <- stats::time(mts)
  df$sample_status <- sample_status
  df$frames <- rep(date_str, nrow(df))

  stacked <- utils::stack(df[setdiff(names(df), c("dates", "sample_status", "frames"))])
  out <- data.frame(
    dates = rep(df$dates, times = length(unique(stacked$ind))),
    sample_status = rep(df$sample_status, times = length(unique(stacked$ind))),
    frames = rep(df$frames, times = length(unique(stacked$ind))),
    variable = stacked$ind,
    value = stacked$values
  )

  out
}
