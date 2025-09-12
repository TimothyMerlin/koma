test_that("attach_color_code adds color codes correctly", {
  # Create a mock data frame
  df_long <- data.frame(
    sample_status =
      c("in_sample", "base_forecast", "conditional_forecast", "unknown_status"),
    stringsAsFactors = TRUE
  )
  # Create a mock color mapping
  marker_color <- list(
    in_sample = "red",
    base_forecast = "green",
    conditional_forecast = "blue"
  )
  status_column <- "sample_status"

  result <- attach_color_code(df_long, marker_color, status_column)

  # Check that the color_code column has been added
  expect_true("color_code" %in% names(result))

  # Check that the colors are correct
  expect_equal(result$color_code, c("red", "green", "blue", NA))

  # Check handling of a nonexistent status column
  expect_error(attach_color_code(df_long, marker_color,
    status_column = "nonexistent_column"
  ))
})
