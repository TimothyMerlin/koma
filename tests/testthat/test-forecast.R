test_that("forecast works correctly for density forecasts", {
  dates <- list(
    estimation = list(
      start = c(1977, 1),
      end = c(2018, 4)
    ),
    forecast = list(
      start = c(2023, 2),
      end = c(2025, 4)
    ),
    dynamic_weights = list(
      start = c(1992, 1),
      end = c(2022, 4)
    )
  )

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

  dates_current <- c(2023, 1)

  # shorten endogenous data to end before forecast start
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
  result <- withr::with_seed(
    7,
    forecast(
      estimates, dates,
      point_forecast = list(active = FALSE)
    )
  )

  print(result)
  print(result, central_tendency = "median")
  print(result, central_tendency = "q_5")
  expect_error(print(result, central_tendency = "q"))
  print(result, variables = "gdp")
  print(result, variables = c("gdp", "service"))

  expect_identical(
    names(result),
    c(
      "mean", "median", "forecasts", "quantiles", "ts_data",
      "y_matrix", "x_matrix"
    )
  )
  expect_identical(
    names(result$quantiles),
    c("q_5", "q_50", "q_95")
  )

  expected_time <- structure(c(
    2023.25, 2023.5, 2023.75, 2024, 2024.25, 2024.5,
    2024.75, 2025, 2025.25, 2025.5, 2025.75
  ), tsp = c(
    2023.25, 2025.75,
    4
  ), class = "ts")
  expect_identical(
    stats::time(result$quantiles$q_50[[1]]),
    expected_time
  )

  # check that forecast preserves ts attributes
  expected_attr_cons <- list(
    tsp = c(2023.25, 2025.75, 4),
    class = c("koma_ts", "ts"),
    series_type = "rate",
    method = "percentage",
    value_type = "real",
    anker = c(797084.23, 2023),
    ets_attributes = c("series_type", "method", "value_type", "anker")
  )
  expect_equal(
    attributes(result$quantiles$q_50$consumption),
    expected_attr_cons
  )

  # arguments in ... must be used
  expect_warning(
    forecast(estimates, dates, unused = TRUE)
  )
})

test_that("forecast conditionally fills ragged edge", {
  dates <- list(
    estimation = list(
      start = c(1977, 1),
      end = c(2018, 4)
    ),
    forecast = list(
      start = c(2023, 2),
      end = c(2025, 4)
    ),
    dynamic_weights = list(
      start = c(1992, 1),
      end = c(2022, 4)
    )
  )
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

  # truncate current account data at 2021Q4 and add to ts_data
  ts_data_current_account <- stats::window(ts_data$current_account,
    end = c(2021, 4)
  )
  ts_data$current_account <- ts_data_current_account

  # truncate investment data at 2022Q1 and add to ts_data
  ts_data_investment <- stats::window(ts_data$investment,
    end = c(2022, 1)
  )
  ts_data$investment <- ts_data_investment

  dates_current <- c(2023, 1)

  ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
      suppressWarnings(stats::window(ts_data[[x]], end = dates_current))
    })

  est <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates,
      options = list(ndraws = 200), # fewer draws for speed
      point_forecast = list(active = FALSE, central_tendency = "mean")
    )
  )

  # We expect the current account and investment series to equal what we
  # inputed in the estimate_sem function. This is because the ragged edge is
  # between the estimation$end and forecast$start dates.
  exp_current_acc <- ts_data$current_account
  attr(exp_current_acc, "anker") <- c(100, 1975.75)
  attr(exp_current_acc, "ets_attributes") <- c(
    attr(exp_current_acc, "ets_attributes"), "anker"
  )
  expect_equal(
    est$ts_data$current_account,
    exp_current_acc
  )
  exp_invest <- ts_data$investment
  attr(exp_invest, "anker") <- c(100, 1975.75)
  attr(exp_invest, "ets_attributes") <- c(
    attr(exp_invest, "ets_attributes"), "anker"
  )
  expect_equal(
    est$ts_data$investment,
    exp_invest
  )

  # point forecast
  result <- withr::with_seed(7, forecast(est, dates))

  # y matrix should range from estimation start date to one quarter before
  # forecast start date
  expected_time <- stats::ts(seq(from = 1977.00, to = 2023.00, by = 0.25),
    start = 1977.00, frequency = 4
  )
  expect_identical(stats::time(result$y_matrix), expected_time)

  # investment ts should be uncut at start and range up to one quarter before
  # forecast start date
  expected_time <- stats::ts(seq(from = 1976.00, to = 2023.00, by = 0.25),
    start = 1976.00, frequency = 4
  )
  expect_identical(stats::time(result$ts_data$investment), expected_time)

  expect_identical(
    names(result),
    c(
      "mean", "median", "forecasts", "quantiles", "ts_data", "y_matrix",
      "x_matrix"
    )
  )

  expected_time <- structure(c(
    2023.25, 2023.5, 2023.75, 2024, 2024.25, 2024.5,
    2024.75, 2025, 2025.25, 2025.5, 2025.75
  ), tsp = c(
    2023.25, 2025.75,
    4
  ), class = "ts")
  expect_identical(stats::time(result$mean[[1]]), expected_time)
})

test_that("forecast stops when endogenous series longer than forecast start", {
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2018, 4)),
    forecast = list(start = c(2023, 2), end = c(2025, 4))
  )

  sys_eq <- system_of_equations(
    simulated_data$equations, simulated_data$exogenous_variables
  )

  ts_data <- simulated_data$ts_data

  est <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 20))
  )

  # series extend beyond the forecast start date
  expect_error(
    forecast(est, dates), "consumption, investment"
  )
})

test_that("forecast stops when exogenous series don't extend to forecast end", {
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2018, 4)),
    forecast = list(start = c(2023, 2), end = c(2025, 4))
  )

  sys_eq <- system_of_equations(
    simulated_data$equations, simulated_data$exogenous_variables
  )

  ts_data <- simulated_data$ts_data
  dates_current <- c(2023, 1)
  # shorten endogenous data to end before forecast start
  ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
      stats::window(ts_data[[x]], end = dates_current)
    })
  # shorten exogenous series world_gdp
  ts_data$world_gdp <- window(ts_data$world_gdp, end = dates_current)

  estimates <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 20))
  )

  # series extend beyond the forecast start date
  expect_error(
    forecast(estimates, dates), "world_gdp"
  )

  # Check for when one exogenous variable is missing data at edge
  estimates$ts_data$world_gdp <-
    stats::window(simulated_data$ts_data$world_gdp, end = c(2024, 4))
  dates$forecast$end <- c(2025, 4)

  # Expect a warning that indicates that world_gdp is too short
  expect_warning(
    withr::with_seed(
      7,
      forecast(
        estimates, dates,
        point_forecast = list(active = FALSE)
      )
    ),
    "Forecast horizon shortened to 7."
  )

  suppressWarnings(
    result <- withr::with_seed(
      7,
      forecast(
        estimates, dates,
        point_forecast = list(active = TRUE)
      )
    )
  )

  # Forecasts should only up to 2024 Q4
  expected_time <- structure(c(
    2023.25, 2023.5, 2023.75, 2024, 2024.25, 2024.5, 2024.75
  ), tsp = c(
    2023.25, 2024.75,
    4
  ), class = "ts")
  expect_identical(
    stats::time(result$mean[[1]]),
    expected_time
  )

  print(result)
  print(result, variables = "consumption")
  print(result, variables = c("gdp", "consumption"))
})

test_that("update_anker", {
  x <- rate(ets(
    c(100, 101, 99, 103),
    frequency = 4, start = c(2019, 1),
    series_type = "level", method = "percentage"
  ))

  y <- rate(ets(
    c(100, 101, 99, 103),
    frequency = 4, start = c(2019, 4),
    series_type = "level", method = "percentage"
  ))

  # set a wrong anker
  attr(y, "anker") <- c(100, 2019)

  result <- update_anker(x, y)

  expect_equal(attr(result, "anker")[1], level(x)[4])
  expect_equal(attr(result, "anker")[2], stats::tsp(x)[2])
})

test_that("forecast with one equation", {
  dates <- list(
    estimation = list(
      start = c(1977, 1),
      end = c(2019, 4)
    ),
    forecast = list(
      start = c(2023, 2),
      end = c(2025, 4)
    ),
    dynamic_weights = list(
      start = c(1992, 1),
      end = c(2022, 4)
    )
  )

  equations <- "manufacturing ~ world_gdp"
  exogenous_variables <- c("world_gdp")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data
  dates_current <- c(2023, 1)
  # shorten endogenous data to end before forecast start
  ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
      stats::window(ts_data[[x]], end = dates_current)
    })

  est <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )

  out <- forecast(est, dates)

  expect_equal(names(out$mean), c("manufacturing", "world_gdp"))
})

test_that("forecast without lags and with restrictions", {
  dates <- list(
    estimation = list(
      start = c(1977, 1),
      end = c(2019, 4)
    ),
    forecast = list(
      start = c(2023, 2),
      end = c(2025, 4)
    ),
    dynamic_weights = list(
      start = c(1992, 1),
      end = c(2022, 4)
    )
  )

  equations <- "manufacturing ~ world_gdp,
    service ~ population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"
  exogenous_variables <- c("world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data
  dates_current <- c(2023, 1)
  # shorten endogenous data to end before forecast start
  ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
      stats::window(ts_data[[x]], end = dates_current)
    })

  est <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )

  restrictions <- list(manufacturing = list(value = 0.5, horizon = 1))
  # When forecasting without lags Phi matrix will be empty
  out <- forecast(est, dates, restrictions = restrictions)

  # first horizon of manufacturing should equal restriction
  expect_equal(out$mean$manufacturing[1], 0.5)
})

test_that("forecast with restrictions for variables that are not in SEM", {
  dates <- list(
    estimation = list(
      start = c(1975, 1),
      end = c(2019, 4)
    ),
    forecast = list(
      start = c(2023, 2),
      end = c(2025, 4)
    ),
    dynamic_weights = list(
      start = c(1992, 1),
      end = c(2022, 4)
    )
  )

  equations <- "manufacturing ~ world_gdp,
    service ~ population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"
  exogenous_variables <- c("world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data
  dates_current <- c(2023, 1)
  # shorten endogenous data to end before forecast start
  ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
      stats::window(ts_data[[x]], end = dates_current)
    })

  est <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )

  restrictions <- list(
    manufacturing = list(value = 0.5, horizon = 1),
    consumption = list(value = 0.5, horizon = 1)
  )

  # When forecasting without lags Phi matrix will be empty
  out <- forecast(est, dates, restrictions = restrictions)

  # first horizon of manufacturing should equal restriction
  expect_equal(out$mean$manufacturing[1], 0.5)
})



test_that("estimate an AR(1) model", {
  # Case: AR(1) model
  equations <- "manufacturing ~ manufacturing.L(1) -1,
                service ~ service.L(1) -1"
  exogenous_variables <- c()

  sys_eq <- system_of_equations(equations, exogenous_variables)

  n <- 200 # Number of observations
  # Generate AR(1) process for manufacturing
  phi_manufacturing <- 0.8 # AR(1) coefficient for manufacturing
  sigma <- 1 # Standard deviation of white noise
  epsilon_manufacturing <- withr::with_seed(7, rnorm(n, mean = 0, sd = sigma))
  y_manufacturing <- numeric(n)

  y_manufacturing[1] <- epsilon_manufacturing[1]
  for (t in 2:n) {
    y_manufacturing[t] <- phi_manufacturing * y_manufacturing[t - 1] + epsilon_manufacturing[t]
  }

  # Generate AR(1) process for service
  phi_service <- 0.6 # AR(1) coefficient for service
  epsilon_service <- withr::with_seed(8, rnorm(n, mean = 0, sd = sigma))
  y_service <- numeric(n)

  y_service[1] <- epsilon_service[1]
  for (t in 2:n) {
    y_service[t] <- phi_service * y_service[t - 1] + epsilon_service[t]
  }

  ts_data <- list(
    manufacturing = ets(y_manufacturing,
      start = c(1970, 1), frequency = 4,
      series_type = "rate", method = "percentage"
    ),
    service = ets(y_service,
      start = c(1970, 1), frequency = 4,
      series_type = "rate", method = "percentage"
    )
  )

  dates <- list(
    estimation = list(
      start = c(1971, 1),
      end = c(2019, 4)
    ),
    forecast = list(
      start = c(2020, 1),
      end = c(2025, 4)
    )
  )

  est <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates)
  )

  result <- extract_estimates_from_draws(sys_eq, est$estimates)
  expect_equal(
    result$beta_matrix["manufacturing.L(1)", "manufacturing"],
    phi_manufacturing,
    tolerance = 2e-1
  )
  expect_equal(
    result$beta_matrix["service.L(1)", "service"],
    phi_service,
    tolerance = 2e-1
  )

  restrictions <- list(manufacturing = list(value = 0.5, horizon = 2))
  out <- forecast(est, dates, restrictions = restrictions)

  expect_equal(out$mean$manufacturing[2], 0.5)
})

test_that("forecast dates incomplete", {
  dates <- list(
    estimation = list(
      start = c(1977, 1),
      end = c(2018, 4)
    ),
    forecast = list(
      start = c(2023, 2)
    )
  )

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

  dates_current <- c(2023, 1)

  # shorten endogenous data to end before forecast start
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
  expect_error(
    forecast(
      estimates, dates,
      point_forecast = list(active = FALSE)
    ), "Invalid"
  )
})
