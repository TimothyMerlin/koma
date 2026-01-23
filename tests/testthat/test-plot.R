test_that("plot point forecasts", {
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2020, 4)),
    forecast = list(start = c(2023, 2), end = c(2025, 4)),
    index = list(start = c(2019, 1), end = c(2019, 4))
  )
  dates_current <- c(2023, 1)

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

  x <- forecast(
    estimates,
    dates,
    point_forecast = list(active = TRUE, central_tendency = "mean")
  )

  fig <- plot(x, variables = "gdp")
  # Test Type Checks
  expect_true(inherits(fig, "plotly"))

  ## Layout tests:
  fig_layout <- fig$x$layoutAttrs[[1]]

  # Test ticks
  x_axis_tickvals <- fig_layout$xaxis$tickvals

  expected_x_axis_tickvals <- 1977:2025
  expect_equal(x_axis_tickvals, expected_x_axis_tickvals)

  # plot another variable in the same figure
  fig <- plot(x, variables = "service", fig = fig)

  expect_silent(plot(x, variables = c("gdp", "service")))
  # Error on wrong variables
  expect_error(plot(x))
  expect_error(plot(x, variables = c("x", "gdp")))

  # Case plotting exogenous variables
  fig <- plot(x, variables = "world_gdp")
})

test_that("plot density forecasts", {
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2020, 4)),
    forecast = list(start = c(2023, 2), end = c(2025, 4)),
    index = list(start = c(2019, 1), end = c(2019, 4))
  )
  dates_current <- c(2023, 1)

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

  x <- forecast(
    estimates,
    dates,
    point_forecast = list(active = FALSE, central_tendency = "mean")
  )

  fig <- plot(x, variables = "gdp")
  # Test Type Checks
  expect_true(inherits(fig, "plotly"))

  ## Layout tests:
  fig_layout <- fig$x$layoutAttrs[[1]]

  # Test ticks
  x_axis_tickvals <- fig_layout$xaxis$tickvals

  expected_x_axis_tickvals <- 1977:2025
  expect_equal(x_axis_tickvals, expected_x_axis_tickvals)

  out <- plot(x, variables = "gdp", fan = TRUE)
  built <- plotly::plotly_build(out)
  fan_traces <- vapply(built$x$data, function(tr) {
    identical(tr$legendgroup, "fan")
  }, logical(1))
  expect_true(any(fan_traces))
})

test_that("plot.koma_forecast() errors cleanly when plotly is missing", {
  skip_if(
    requireNamespace("plotly", quietly = TRUE),
    "plotly is installed; skipping missing-package test."
  )

  fake_forecast <- list(
    mean = list(GDP = ts(1:8, start = c(2024, 1), freq = 4)),
    ts_data = list(GDP = ts(1:8, start = c(2022, 1), freq = 4)),
    quantiles = list()
  )
  class(fake_forecast) <- "koma_forecast"

  expect_error(
    plot.koma_forecast(fake_forecast, variables = "GDP"),
    regexp = "plotly.*required",
    class = "cli_error"
  )
})
