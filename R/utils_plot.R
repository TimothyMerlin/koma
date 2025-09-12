#' Attach Color Codes to Data Frame Based on Status
#'
#' This function maps the `status_column` values in the provided data frame to
#' color codes, based on a specified color mapping, and attaches these color
#' codes as a new column `color_code` in the data frame.
#'
#' @param df_long A data frame containing a column specified by `status_column`
#' which indicates the status of each sample.
#' @param marker_color A named list or other key-value mapping structure where
#' the names or keys correspond to statuses and the values correspond to color
#' codes.
#' @param status_column The name of the column in `df_long` that contains the
#' status information.
#'
#' @return A data frame identical to `df_long`, but with an additional column
#' `color_code` which contains the color codes mapped from the `status_column`
#' based on the `marker_color` mapping.
#' @keywords internal
attach_color_code <- function(df_long, marker_color, status_column) {
  # Check if the specified status_column exists in df_long
  if (!status_column %in% names(df_long)) {
    stop("The specified status_column does not exist in df_long")
  }

  # Map the status to color code using the marker_color mapping
  df_long$color_code <- sapply(
    as.character(df_long[[status_column]]),
    function(status) {
      if (status %in% names(marker_color)) {
        return(marker_color[[status]])
      } else {
        return(NA) # Return NA for unknown statuses
      }
    }
  )

  return(df_long)
}
