test_that("extend_ts_with_forecast function works correctly", {
  # Create example data for testing
  ts1 <- stats::ts(1:5, start = 1, frequency = 1)
  ts2 <- stats::ts(6:10, start = 1, frequency = 1)
  ts_list <- lapply(list(a = ts1, b = ts2), function(x) {
    as_ets(x, series_type = "rate")
  })
  forecast_mts <- stats::ts(
    cbind(a = 11:15, b = 16:20),
    start = 6, frequency = 1
  )

  # Test: Extend the original time series with forecast
  result <- extend_ts_with_forecast(ts_list, forecast_mts)

  # Validate results
  expected_ts1 <- as_ets(
    stats::ts(c(1:5, 11:15), start = 1, frequency = 1),
    series_type = "rate"
  )
  expected_ts2 <- as_ets(
    stats::ts(c(6:10, 16:20), start = 1, frequency = 1),
    series_type = "rate"
  )

  expect_equal(result[[1]], expected_ts1)
  expect_equal(result[[2]], expected_ts2)

  # Test: Column name mismatch
  ts_list_2 <- list(c = as_ets(ts1, series_type = "rate"))
  result_2 <- extend_ts_with_forecast(ts_list_2, forecast_mts)

  expect_equal(result_2, ts_list_2)

  # Test: Empty ts_list
  ts_list_3 <- list()
  result_3 <- extend_ts_with_forecast(ts_list_3, forecast_mts)

  expect_equal(result_3, list())
})

test_that("extend_ts_with_forecast with ragged edge", {
  # Case: where ragged edge in tslist. Only add conditional forecasts to tslist
  # that are not already contained (realized observations) in tslist.

  # Create example data for testing
  ts1 <- stats::ts(1:8, start = c(2018, 1), frequency = 4)
  ts2 <- stats::ts(1:10, start = c(2018, 1), frequency = 4)
  ts_list <- lapply(list(var1 = ts1, var2 = ts2), function(x) {
    as_ets(x, series_type = "rate")
  })

  # Create example data for testing
  forecast_ts <- stats::ts(9:12, start = c(2020, 1), frequency = 4)

  # Combine them into a matrix
  mts_matrix <- matrix(NA,
    nrow = length(forecast_ts), ncol = 2
  )
  mts_matrix[, 1] <- forecast_ts
  mts_matrix[, 2] <- forecast_ts
  colnames(mts_matrix) <- c("var1", "var2")

  # Create a multivariate time series (MTS) object
  forecast_mts <- stats::ts(mts_matrix, start = c(2020, 1), frequency = 4)

  # Test: Extend the original time series with forecast
  result <- extend_ts_with_forecast(ts_list, forecast_mts)

  # Validate results
  expected_ts <- as_ets(stats::ts(1:12, start = c(2018, 1), frequency = 4),
    series_type = "rate"
  )
  expected_ts_list <- list(var1 = expected_ts, var2 = expected_ts)

  expect_equal(result$var1, expected_ts_list$var1)
  expect_equal(result$var2, expected_ts_list$var2)
})
