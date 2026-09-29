test_that("plot point forecasts", {
  skip_on_cran()
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
      options = list(gibbs = list(ndraws = 200)) # fewer draws for speed
    )
  )

  x <- forecast(
    estimates,
    dates,
    options = list(approximate = TRUE)
  )

  var <- "gdp"
  fig <- plot(x, variables = var)
  # Test Type Checks
  expect_true(inherits(fig, "plotly"))

  ## Layout tests:
  fig_layout <- fig$x$layoutAttrs[[1]]

  # Test ticks
  x_axis_tickvals <- fig_layout$xaxis$tickvals

  expected_x_axis_tickvals <- 1977:2025
  expect_equal(x_axis_tickvals, expected_x_axis_tickvals)

  expect_equal(fig_layout$title$text, var)

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
  skip_on_cran()
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
      options = list(gibbs = list(ndraws = 200)) # fewer draws for speed
    )
  )

  x <- forecast(
    estimates,
    dates,
    options = list(probs = get_quantiles())
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

fan_test_forecast <- function() {
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2020, 4)),
    forecast = list(start = c(2023, 2), end = c(2025, 4))
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
      options = list(gibbs = list(ndraws = 40))
    )
  )
  x <- withr::with_seed(7, forecast(estimates, dates))

  tsl <- lapply(x$ts_data[names(x$mean)], function(s) {
    suppressWarnings(stats::window(s, end = dates_current))
  })

  list(x = x, tsl = tsl, forecast_start = dates$forecast$start)
}

test_that("fan bands are quantiles of per-draw level paths", {
  skip_on_cran()
  setup <- fan_test_forecast()
  x <- setup$x
  tsl <- setup$tsl

  fan <- build_fan_data(
    x, tsl, setup$forecast_start, "gdp",
    fan_quantiles = c(0.05, 0.95)
  )

  expect_equal(unique(fan$band), "q_5-q_95")

  # gdp uses the "percentage" method: compound each draw from the last level
  last_level <- utils::tail(as.numeric(level(tsl$gdp)), 1)
  level_draws <- vapply(x$forecasts, function(draw) {
    last_level * cumprod(1 + draw[, "gdp"] / 100)
  }, numeric(nrow(x$forecasts[[1]])))
  expected <- apply(level_draws, 1, stats::quantile, probs = c(0.05, 0.95))

  expect_equal(fan$lower, unname(expected[1, ]))
  expect_equal(fan$upper, unname(expected[2, ]))
})

test_that("growth whiskers are per-horizon quantiles of growth draws", {
  skip_on_cran()
  setup <- fan_test_forecast()
  x <- setup$x

  growth_draws <- vapply(
    x$forecasts,
    function(draw) as.numeric(draw[, "gdp"]),
    numeric(nrow(x$forecasts[[1]]))
  )

  # q_5/q_95 are stored on the forecast; q_10/q_90 are computed from draws
  for (probs in list(c(0.05, 0.95), c(0.1, 0.9))) {
    whiskers <- build_whisker_data(
      x, setup$tsl, setup$forecast_start, "gdp",
      fan_quantiles = probs
    )
    expected <- apply(growth_draws, 1, stats::quantile, probs = probs)

    expect_equal(whiskers$lower, unname(expected[1, ]))
    expect_equal(whiskers$upper, unname(expected[2, ]))
  }
})

test_that("fan bands are rebased together with the level line", {
  skip_on_cran()
  setup <- fan_test_forecast()
  x <- setup$x
  tsl <- setup$tsl
  index <- list(start = c(2019, 1), end = c(2019, 4))

  fig <- plot(
    x,
    variables = "gdp",
    fan = TRUE,
    theme = init_koma_theme(index = index)
  )
  built <- plotly::plotly_build(fig)
  fan_traces <- Filter(
    function(tr) identical(tr$legendgroup, "fan"),
    built$x$data
  )

  # factor that rebase() applies to the level line in the forecast period
  level_line <- as_mets(concat(tsl, x$mean))
  factor <- as.numeric(stats::window(
    level(rebase(level_line, index$start, index$end))[, "gdp"],
    start = setup$forecast_start
  )) / as.numeric(stats::window(
    level(level_line)[, "gdp"],
    start = setup$forecast_start
  ))
  fan <- build_fan_data(x, tsl, setup$forecast_start, "gdp", NULL)

  expect_length(fan_traces, 2)
  expect_equal(as.numeric(fan_traces[[1]]$y), fan$lower * factor)
  expect_equal(as.numeric(fan_traces[[2]]$y), fan$upper * factor)
})

test_that("get_fan_pairs() does not duplicate complementary quantiles", {
  pairs <- get_fan_pairs(c("q_5", "q_95"), c(0.05, 0.95))
  expect_equal(pairs, list(list(lower = "q_5", upper = "q_95")))
})

test_that("plot.koma_forecast() validates variables", {
  fake_forecast <- structure(
    list(
      mean = list(GDP = ts(1:8, start = c(2024, 1), freq = 4)),
      ts_data = list(GDP = ts(1:8, start = c(2022, 1), freq = 4))
    ),
    class = "koma_forecast"
  )

  for (variables in list(NULL, 1, character(0), NA_character_, c("GDP", NA))) {
    expect_error(
      plot(fake_forecast, variables = variables),
      "non-empty character vector",
      class = "rlang_error"
    )
  }
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

test_that("plot point forecasts supports monthly single-frequency data", {
  y <- as_ets(
    stats::ts(
      cumsum(seq(0.5, 2.85, by = 0.05)),
      start = c(2019, 1),
      frequency = 12
    ),
    series_type = "rate",
    method = "none"
  )
  x <- as_ets(
    stats::ts(
      seq(1, 6, by = 0.1),
      start = c(2019, 1),
      frequency = 12
    ),
    series_type = "rate",
    method = "none"
  )
  ts_data <- list(
    y = stats::window(y, end = c(2022, 12)),
    x = x
  )
  sys_eq <- system_of_equations("y ~ y.L(1) + x", exogenous_variables = "x")
  dates <- list(
    estimation = list(start = c(2019, 2), end = c(2022, 12)),
    forecast = list(start = c(2023, 1), end = c(2023, 3))
  )

  estimates <- withr::with_seed(
    42,
    estimate(
      ts_data,
      sys_eq,
      dates,
      options = list(gibbs = list(ndraws = 40))
    )
  )
  forecast_obj <- forecast(
    estimates,
    dates,
    options = list(approximate = TRUE)
  )
  fig <- plot(forecast_obj, variables = "y")
  built <- plotly::plotly_build(fig)
  trace_names <- vapply(
    built$x$data[1:4],
    `[[`,
    character(1),
    "name"
  )
  bar_text <- unlist(lapply(built$x$data[1:2], `[[`, "text"))

  expect_true(inherits(fig, "plotly"))
  expect_true(all(grepl("2023-01", trace_names)))
  expect_true(any(grepl("Jan", bar_text)))
  expect_true(any(grepl("Feb", bar_text)))
  expect_false(any(grepl("Q[1-4]", bar_text)))
})

test_that("plot point forecasts supports yearly single-frequency data", {
  # Small perturbation avoids a perfect (zero-residual) fit of y on y.L(1)
  # and x, which otherwise starves the Gibbs sampler's variance draw of
  # degrees of freedom and makes it flaky across BLAS backends.
  y <- as_ets(
    stats::ts(
      cumsum(seq(1, 8, by = 1)) +
        c(0, 0.4, -0.3, 0.2, -0.4, 0.3, -0.2, 0.1),
      start = 2015,
      frequency = 1
    ),
    series_type = "rate",
    method = "none"
  )
  x <- as_ets(
    stats::ts(
      seq(2, 11, by = 1),
      start = 2015,
      frequency = 1
    ),
    series_type = "rate",
    method = "none"
  )
  ts_data <- list(
    y = stats::window(y, end = 2020),
    x = x
  )
  sys_eq <- system_of_equations("y ~ y.L(1) + x", exogenous_variables = "x")
  dates <- list(
    estimation = list(start = 2016, end = 2020),
    forecast = list(start = 2021, end = 2022)
  )

  estimates <- withr::with_seed(
    42,
    estimate(
      ts_data,
      sys_eq,
      dates,
      options = list(gibbs = list(ndraws = 40))
    )
  )
  forecast_obj <- forecast(
    estimates,
    dates,
    options = list(approximate = TRUE)
  )
  fig <- plot(forecast_obj, variables = "y")
  built <- plotly::plotly_build(fig)
  trace_names <- vapply(
    built$x$data[1:4],
    `[[`,
    character(1),
    "name"
  )
  hover_text <- unlist(lapply(built$x$data[1:4], `[[`, "text"))

  expect_true(inherits(fig, "plotly"))
  expect_true(all(grepl("2021", trace_names)))
  expect_true(any(grepl("^2021$", hover_text)))
  expect_true(any(grepl("^2022$", hover_text)))
  expect_false(any(grepl("-01", hover_text)))
  expect_true(any(grepl(" 2021", unlist(lapply(built$x$data[1:2], `[[`, "text")))))
})
