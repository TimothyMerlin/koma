test_that("fill_ragged_edge with one-step ahead conditional forecasts", {
  skip_on_cran()
  # mock readline function always return "y"
  responses <- c("y", "median")
  response_ix <- 0
  testthat::local_mocked_bindings(
    readline = function(...) {
      response_ix <<- response_ix + 1
      responses[[((response_ix - 1) %% length(responses)) + 1]]
    },
    .package = "base"
  )

  # Case: test shortens current account data, which sem will fill with
  # conditional forecast
  dates <- list(estimation = list(), forecast = list(), current = NULL)
  dates$estimation$start <- c(1996, 1)
  dates$estimation$end <- c(2019, 4)

  dates <- dates_to_num(dates, frequency = 4)

  equations <- "consumption ~ gdp + consumption.L(1) + interest_rate,
investment ~ gdp + investment.L(1) + interest_rate,
exports ~ world_gdp + exchange_rate + exports.L(1),
imports ~ gdp + exchange_rate + imports.L(1),
gdp == 0.64*consumption + 0.27*investment + 0.57*exports - 0.48*imports"

  exogenous_variables <- c("interest_rate", "world_gdp", "exchange_rate")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ## Fewer draws for speed:
  set_gibbs_settings(settings = list(ndraws = 200), sys_eq$equation_settings)

  series <- unique(c(sys_eq$endogenous_variables, sys_eq$exogenous_variables))
  ts_data <- small_open_economy[series]
  ts_data <- lapply(ts_data, function(x) {
    as_ets(x, series_type = "level", method = "diff_log")
  })
  ts_data$interest_rate <- as_ets(
    ts_data$interest_rate,
    series_type = "rate",
    method = "none"
  )

  # truncate current account data and add to ts_data
  ts_data_investment <- stats::window(ts_data$investment,
    end = c(2018, 4)
  )
  ts_data$investment <- ts_data_investment

  ts_data <- rate(ts_data)

  ts_data <- create_lagged_variables(
    ts_data, sys_eq$endogenous_variables,
    exogenous_variables, sys_eq$predetermined_variables
  )

  workers <- parallelly::availableCores(omit = 1)
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)

  if (Sys.info()[["sysname"]] == "Darwin" || .Platform$OS.type != "unix") {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::multicore, workers = workers)
  }

  result <- fill_ragged_edge(
    ts_data, sys_eq, exogenous_variables, dates,
    fill_method = "median"
  )

  expect_equal(length(result$investment), length(ts_data$investment) + 4)
})

test_that("fill_ragged_edge - no observations to fill", {
  dates <- list(estimation = list(), forecast = list(), current = NULL)
  dates$estimation$start <- c(1977, 1)
  dates$estimation$end <- c(2019, 4)

  dates <- dates_to_num(dates, frequency = 4)

  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")
  sys_eq <- system_of_equations(equations, exogenous_variables)

  ## Fewer draws for speed:
  set_gibbs_settings(settings = list(ndraws = 200), sys_eq$equation_settings)

  ts_data <- simulated_data$ts_data

  result <- fill_ragged_edge(
    ts_data, sys_eq, exogenous_variables, dates,
    fill_method = "median"
  )

  expect_length(result$current_account, 200)
})

test_that("fill_in_draws restricts the observed values of the ragged edge", {
  sys_eq <- system_of_equations(
    "manufacturing ~ world_gdp + manufacturing.L(1),
    service ~ population + gdp,
    gdp == 0.5*manufacturing + 0.5*service",
    c("world_gdp", "population")
  )
  endogenous <- sys_eq$endogenous_variables
  data <- rate(simulated_data$ts_data[c(endogenous, sys_eq$exogenous_variables)])
  data$gdp <- stats::window(data$gdp, end = c(2022, 3))
  data[c("manufacturing", "service")] <- lapply(
    data[c("manufacturing", "service")], stats::window,
    end = c(2023, 1)
  )
  # manufacturing in 2023 Q1 was filled by estimate()
  estimates <- list(
    sys_eq = sys_eq,
    ts_data = data,
    observed_end = c(manufacturing = 2022.75, service = 2023, gdp = 2022.5)
  )
  ts_data <- create_lagged_variables(
    data, endogenous, sys_eq$exogenous_variables, sys_eq$predetermined_variables
  )
  dates <- list(
    estimation = list(start = 1977, end = 2019.75),
    forecast = list(start = 2023.25, end = 2024),
    current = 2023
  )
  observed <- function(x, start, end) {
    as.numeric(stats::window(data[[x]], start = start, end = end))
  }

  fill <- fill_in_draws(estimates, ts_data, dates, frequency = 4)

  # gdp ends first, in 2022 Q3; 2022 Q4 and 2023 Q1 are filled
  expect_equal(fill$fill_periods, 2)
  expect_equal(fill$start, 2022.75)
  # only observed values are restrictions, not those filled by estimate()
  expect_equal(fill$restrictions, list(
    manufacturing = list(
      horizon = 1, value = observed("manufacturing", 2022.75, 2022.75)
    ),
    service = list(horizon = 1:2, value = observed("service", 2022.75, 2023))
  ))
  expect_equal(stats::tsp(fill$y_matrix)[2], 2022.5)
  expect_equal(stats::tsp(fill$x_matrix)[1:2], c(2022.75, 2024))
  expect_equal(colnames(fill$x_matrix), sys_eq$exogenous_variables)

  # without observed_end, the ends of the data count as observed
  estimates$observed_end <- NULL
  fill <- fill_in_draws(estimates, ts_data, dates, frequency = 4)
  expect_equal(fill$restrictions$manufacturing, list(
    horizon = 1:2, value = observed("manufacturing", 2022.75, 2023)
  ))

  # all observed up to the forecast: nothing to fill
  estimates$observed_end <- c(manufacturing = 2023, service = 2023, gdp = 2023)
  expect_null(fill_in_draws(estimates, ts_data, dates, frequency = 4))
})

test_that("conditional_fill fills up edge correctly", {
  skip_on_cran()
  # Case: test shortens current account data, which sem will fill with
  # conditional forecast
  dates <- list(estimation = list(), forecast = list(), current = NULL)
  dates$current <- c(2023, 2)
  dates$estimation$start <- c(1996, 1)
  dates$estimation$end <- c(2019, 4)
  # Begin of forecasts
  dates$forecast$start <- c(2023, 3)
  # Last quarter of forecast
  dates$forecast$end <- c(2024, 4)
  dates <- dates_to_num(dates, 4)

  equations <- "consumption ~ gdp + consumption.L(1) + interest_rate,
investment ~ gdp + investment.L(1) + interest_rate,
exports ~ world_gdp + exchange_rate + exports.L(1),
imports ~ gdp + exchange_rate + imports.L(1),
gdp == 0.64*consumption + 0.27*investment + 0.57*exports - 0.48*imports"

  exogenous_variables <- c("interest_rate", "world_gdp", "exchange_rate")
  sys_eq <- system_of_equations(equations, exogenous_variables)

  series <- unique(c(sys_eq$endogenous_variables, sys_eq$exogenous_variables))
  ts_data <- small_open_economy[series]
  ts_data <- lapply(ts_data, function(x) {
    as_ets(x, series_type = "level", method = "diff_log")
  })
  ts_data$interest_rate <- as_ets(
    ts_data$interest_rate,
    series_type = "rate",
    method = "none"
  )

  # truncate current account data and add to ts_data
  ts_data_investment <- stats::window(ts_data$investment,
    end = c(2022, 4)
  )
  ts_data$investment <- ts_data_investment

  ts_data <- rate(ts_data)


  estimates <- withr::with_seed(
    7,
    estimate(
      ts_data, sys_eq, dates,
      options = list(fill = list(method = "mean"))
    )
  )

  ts_data <- create_lagged_variables(
    estimates$ts_data, sys_eq$endogenous_variables,
    exogenous_variables, sys_eq$predetermined_variables
  )
  result <- conditional_fill(
    ts_data, sys_eq, dates, estimates$estimates,
    fill_method = "mean"
  )

  # variable that should not have changed
  expect_equal(result$consumption, estimates$ts_data$consumption)

  # variables that should be extended up to
  realized_investment <- ts_data$investment
  expect_equal(
    result$investment[seq_along(realized_investment)],
    as.vector(realized_investment)
  )
  expect_length(result$investment, length(realized_investment) + 2)
})

test_that("conditional_fill fills up edge correctly", {
  skip_on_cran()
  # Case: test shortens current account data, which sem will fill with
  # conditional forecast
  dates <- list(estimation = list(), forecast = list(), current = NULL)
  dates$current <- c(2023, 2)
  dates$estimation$start <- c(1996, 1)
  dates$estimation$end <- c(2019, 4)
  # Begin of forecasts
  dates$forecast$start <- c(2023, 3)
  # Last quarter of forecast
  dates$forecast$end <- c(2024, 4)
  dates <- dates_to_num(dates, 4)

  equations <- "consumption ~ gdp + consumption.L(1) + interest_rate,
investment ~ gdp + investment.L(1) + interest_rate,
exports ~ world_gdp + exchange_rate + exports.L(1),
imports ~ gdp + exchange_rate + imports.L(1),
gdp == 0.64*consumption + 0.27*investment + 0.57*exports - 0.48*imports"

  exogenous_variables <- c("interest_rate", "world_gdp", "exchange_rate")
  sys_eq <- system_of_equations(equations, exogenous_variables)

  series <- unique(c(sys_eq$endogenous_variables, sys_eq$exogenous_variables))
  ts_data <- small_open_economy[series]
  ts_data <- lapply(ts_data, function(x) {
    as_ets(x, series_type = "level", method = "diff_log")
  })
  ts_data$interest_rate <- as_ets(
    ts_data$interest_rate,
    series_type = "rate",
    method = "none"
  )

  # truncate data and add to ts_data
  ts_data_investment <- stats::window(ts_data$investment,
    end = c(2022, 4)
  )
  ts_data$investment <- ts_data_investment
  ts_data_gdp <- stats::window(ts_data$gdp,
    end = c(2022, 2)
  )
  ts_data$gdp <- ts_data_gdp

  ts_data <- rate(ts_data)

  estimates <- withr::with_seed(
    7,
    estimate(
      ts_data, sys_eq, dates,
      options = list(fill = list(method = "mean"))
    )
  )

  ts_data <- create_lagged_variables(
    estimates$ts_data, sys_eq$endogenous_variables,
    exogenous_variables, sys_eq$predetermined_variables
  )
  result <- conditional_fill(
    ts_data, sys_eq, dates, estimates$estimates,
    fill_method = "mean"
  )

  # variable that should not have changed
  expect_equal(result$consumption, estimates$ts_data$consumption)

  # variables that should be extended up to
  realized_investment <- ts_data$investment
  expect_equal(
    result$investment[seq_along(realized_investment)],
    as.vector(realized_investment)
  )
  expect_length(result$investment, length(realized_investment) + 2)
})
