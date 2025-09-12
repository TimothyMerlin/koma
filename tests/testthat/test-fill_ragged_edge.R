test_that("fill_ragged_edge with one-step ahead conditional forecasts", {
  # mock readline function always return "y"
  testthat::local_mocked_bindings(readline = function(...) "y", .package = "base")

  # Case: test shortens current account data, which sem will fill with
  # conditional forecast
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

  # truncate current account data and add to ts_data
  ts_data_current_account <- stats::window(ts_data$current_account,
    end = c(2018, 4)
  )
  ts_data$current_account <- ts_data_current_account

  ts_data <- create_lagged_variables(
    ts_data, sys_eq$endogenous_variables,
    exogenous_variables, sys_eq$predetermined_variables
  )

  # use future::plan for parallel execution
  workers <- parallelly::availableCores(omit = 1)

  if (.Platform$OS.type == "unix") {
    # Unix: Will fork the current R session for each parallel worker.
    # This is memory-efficient because child processes can share memory
    # with the parent process.
    future::plan(future::multicore, workers = workers)
  } else {
    # Windows: Since Windows does not support forking, multisession
    # will spawn new R sessions for each parallel worker.
    future::plan(future::multisession, workers = workers)
  }


  result <- fill_ragged_edge(
    ts_data, sys_eq, exogenous_variables, dates,
    point_forecast = list(active = TRUE, central_tendency = "median")
  )

  expect_length(result$current_account, 176)
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
    point_forecast = list(active = TRUE, central_tendency = "median")
  )

  expect_length(result$current_account, 200)
})

test_that("conditional_fill fills up edge correctly", {
  # Case: test shortens current account data, which sem will fill with
  # conditional forecast
  dates <- list(estimation = list(), forecast = list(), current = NULL)
  dates$current <- c(2023, 2)
  dates$estimation$start <- c(1977, 1)
  dates$estimation$end <- c(2019, 4)
  # Begin of forecasts
  dates$forecast$start <- c(2023, 3)
  # Last quarter of forecast
  dates$forecast$end <- c(2025, 4)
  dates <- dates_to_num(dates, 4)

  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")
  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  # truncate current account data and add to ts_data
  ts_data_current_account <- stats::window(ts_data$current_account,
    end = c(2022, 4)
  )
  ts_data$current_account <- ts_data_current_account

  ts_data <- create_lagged_variables(
    ts_data, sys_eq$endogenous_variables,
    exogenous_variables, sys_eq$predetermined_variables
  )
  estimates <- simulated_data$estimates

  result <- conditional_fill(
    ts_data, sys_eq, dates,
    estimates,
    point_forecast = list(active = TRUE, central_tendency = "mean")
  )

  # variable that should not have changed
  expect_equal(result$consumption, ts_data$consumption)

  # variables that should be extended up to
  realized_current_account <- ts_data$current_account
  expect_equal(
    result$current_account[seq_along(realized_current_account)],
    as.vector(realized_current_account)
  )
  expect_length(result$current_account, length(realized_current_account) + 2)
})

test_that("conditional_fill fills up edge for multiple missing", {
  # Case: test shortens current account and investment data, which sem will
  # fill with conditional forecast. The missing data is not of equal length.
  dates <- list(estimation = list(), forecast = list(), current = NULL)
  dates$current <- c(2023, 2)
  dates$estimation$start <- c(1977, 1)
  dates$estimation$end <- c(2019, 4)
  # Begin of forecasts
  dates$forecast$start <- c(2023, 3)
  # Last quarter of forecast
  dates$forecast$end <- c(2025, 4)

  dates <- dates_to_num(dates, 4)

  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")
  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  # truncate current account data and add to ts_data
  ts_data_current_account <- stats::window(ts_data$current_account,
    end = c(2022, 4)
  )
  ts_data$current_account <- ts_data_current_account

  # truncate current account data and add to ts_data
  ts_data_investment <- stats::window(ts_data$investment,
    end = c(2023, 1)
  )
  ts_data$investment <- ts_data_investment

  ts_data <- create_lagged_variables(
    ts_data, sys_eq$endogenous_variables,
    exogenous_variables, sys_eq$predetermined_variables
  )

  result <- conditional_fill(
    ts_data, sys_eq, dates,
    simulated_data$estimates,
    point_forecast = list(active = TRUE, central_tendency = "mean")
  )

  # variable that should not have changed
  expect_equal(result$consumption, ts_data$consumption)

  # variables that should be extended up to
  realized_current_account <- ts_data$current_account
  expect_equal(
    result$current_account[seq_along(realized_current_account)],
    as.vector(realized_current_account)
  )
  expect_length(result$current_account, length(realized_current_account) + 2)

  realized_investment <- ts_data$investment
  expect_equal(
    result$investment[seq_along(realized_investment)],
    as.vector(realized_investment)
  )
  # for investments we have one more realized value than for current account
  expect_length(result$current_account, length(realized_investment) + 1)

  expect_equal(
    result$`investment.L(1)`[seq_along(ts_data$`investment.L(1)`)],
    as.vector(ts_data$`investment.L(1)`)
  )
  # for investments we have one more realized value than for current account
  expect_length(
    result$`investment.L(1)`,
    length(ts_data$`investment.L(1)`) + 1
  )
})
