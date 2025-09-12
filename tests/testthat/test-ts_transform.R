test_that("prepare_data_to_plot", {
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2020, 4)),
    forecast = list(start = c(2022, 2), end = c(2025, 4)),
    index = list(start = c(2018, 1), end = c(2019, 4))
  )
  dates_current <- c(2022, 1)

  sys_eq <- simulated_data$sys_eq

  ts_data <- simulated_data$ts_data
  ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
      stats::window(ts_data[[x]], end = dates_current)
    })

  estimates <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates,
      options = list(ndraws = 200), # fewer draws for speed
      point_forecast = list(active = FALSE)
    )
  )

  forecasts <- forecast(
    estimates,
    dates,
    point_forecast = list(active = TRUE, central_tendency = "mean")
  )

  start <- stats::tsp(forecasts$mean[[1]])[1]

  tsl <- forecasts$ts_data[names(forecasts$mean)]
  suppressWarnings(
    tsl <- lapply(tsl, function(x) {
      stats::window(x, end = dates_current)
    })
  )
  mts_growth <- as_mets(concat(tsl, forecasts$mean))

  # Convert growth rates to level
  mts_level <- level(mts_growth)
  mts_level_annual <- tempdisagg::ta(mts_level,
    conversion = "sum", to = "annual"
  )

  mts_list <- list(
    growth = mts_growth,
    level = mts_level,
    growth_annual = rate(mts_level_annual)
  )
  result <- prepare_data_to_plot(mts_list, start)

  # Check columns
  expected_cols <-
    c("dates", "sample_status", "frames", "variable", "value", "data_type")
  expect_equal(names(result), expected_cols)

  # Check unique sample statuses
  expect_setequal(
    unique(result$sample_status),
    c("in_sample", "forecast")
  )

  # Check data_type column
  expect_setequal(
    unique(result$data_type),
    c("growth", "growth_annual", "level")
  )
})

test_that("to_long", {
  mts <- stats::ts(
    matrix(c(1:10, 11:20), ncol = 2),
    start = c(2000, 1),
    frequency = 4
  )
  forecast_start <- dates_to_num(c(2001, 1), frequency = 4)

  result <- to_long(mts, forecast_start)

  expected_result <- structure(list(
    dates = c(
      2000, 2000, 2000.25, 2000.25, 2000.5,
      2000.5, 2000.75, 2000.75, 2001, 2001, 2001.25, 2001.25, 2001.5,
      2001.5, 2001.75, 2001.75, 2002, 2002, 2002.25, 2002.25
    ), sample_status = structure(c(
      2L,
      2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
      1L, 1L, 1L
    ), levels = c("forecast", "in_sample"), class = "factor"),
    frames = c(
      "2001-Q1", "2001-Q1", "2001-Q1", "2001-Q1", "2001-Q1",
      "2001-Q1", "2001-Q1", "2001-Q1", "2001-Q1", "2001-Q1", "2001-Q1",
      "2001-Q1", "2001-Q1", "2001-Q1", "2001-Q1", "2001-Q1", "2001-Q1",
      "2001-Q1", "2001-Q1", "2001-Q1"
    ), variable = c(
      "Series 1",
      "Series 2", "Series 1", "Series 2", "Series 1", "Series 2",
      "Series 1", "Series 2", "Series 1", "Series 2", "Series 1",
      "Series 2", "Series 1", "Series 2", "Series 1", "Series 2",
      "Series 1", "Series 2", "Series 1", "Series 2"
    ), value = c(
      1L,
      11L, 2L, 12L, 3L, 13L, 4L, 14L, 5L, 15L, 6L, 16L, 7L, 17L,
      8L, 18L, 9L, 19L, 10L, 20L
    )
  ), row.names = c(NA, -20L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  ))

  expect_equal(result, expected_result)
})
