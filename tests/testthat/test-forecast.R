test_that("forecast works correctly for density forecasts", {
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2018, 4)),
    forecast = list(start = c(2023, 2), end = c(2025, 4))
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
      options = list(gibbs = list(ndraws = 200)) # fewer draws for speed
    )
  )
  result <- withr::with_seed(
    7,
    forecast(
      estimates, dates,
      options = list(probs = get_quantiles())
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

  expected_vars <- names(result$quantiles$q_50)
  lapply(result$quantiles, function(q) {
    expect_identical(names(q), expected_vars)
    expect_true(all(vapply(q, is_ets, logical(1))))
  })

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

  tol <- 1e-8
  for (var in expected_vars) {
    expect_true(all(result$quantiles$q_5[[var]] <= result$quantiles$q_50[[var]] + tol))
    expect_true(all(result$quantiles$q_50[[var]] <= result$quantiles$q_95[[var]] + tol))
  }

  # arguments in ... must be used
  expect_warning(
    forecast(estimates, dates, unused = TRUE)
  )

  formatted0 <- format(result, digits = 0)
  expected0 <- round(as_mets(result$mean), digits = 0)
  expect_equal(formatted0, expected0)
})

test_that("forecast conditional innovation methods", {
  # yield approximatively equal distributions
  dates <- list(estimation = list(), forecast = list())
  dates$current <- c(2023, 2)
  dates$estimation$start <- c(1996, 1)
  dates$estimation$end <- c(2019, 4)
  dates$forecast$start <- c(2023, 3)
  dates$forecast$end <- c(2023, 4)

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

  ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
      suppressWarnings(stats::window(ts_data[[x]], end = dates$current))
    })

  estimates <- withr::with_seed(
    11,
    estimate(ts_data, sys_eq, dates,
      options = list(gibbs = list(ndraws = 200))
    )
  )

  restrictions <- list(
    gdp = list(horizon = 1L, value = 0)
  )

  res_projection <- withr::with_seed(
    11,
    forecast(
      estimates, dates,
      restrictions = restrictions,
      options = list(conditional_innov_method = "projection")
    )
  )
  res_eigen <- withr::with_seed(
    11,
    forecast(
      estimates, dates,
      restrictions = restrictions,
      options = list(conditional_innov_method = "eigen")
    )
  )

  draws_proj <- vapply(
    res_projection$forecasts,
    function(x) x[1, "gdp"],
    numeric(1)
  )
  draws_eig <- vapply(
    res_eigen$forecasts,
    function(x) x[1, "gdp"],
    numeric(1)
  )

  mean_proj <- mean(draws_proj)
  mean_eig <- mean(draws_eig)
  sd_proj <- stats::sd(draws_proj)
  sd_eig <- stats::sd(draws_eig)
  n_draws <- length(draws_proj)

  # Compare sample means/SDs with MC-error-based tolerances.
  # The 4*SE band is a loose (~4-sigma) envelope so two draws from the same
  # conditional distribution should pass with high probability. The SD check
  # uses a 10% relative tolerance, but when the conditional variance is tiny
  # (due to tight restrictions), that relative tolerance can be ~0. The small
  # absolute floor prevents failures from floating-point noise in that case.
  se_mean <- sqrt(sd_proj^2 / n_draws + sd_eig^2 / n_draws)
  tol_mean <- max(4 * se_mean, 1e-12)
  tol_sd <- max(0.1 * max(sd_proj, sd_eig), 1e-12)
  expect_lte(abs(mean_proj - mean_eig), tol_mean)
  expect_lte(abs(sd_proj - sd_eig), tol_sd)
})

test_that("forecast conditionally fills ragged edge", {
  dates <- list(estimation = list(), forecast = list())
  dates$current <- c(2023, 2)
  dates$estimation$start <- c(1996, 1)
  dates$estimation$end <- c(2019, 4)
  # Begin of forecasts
  dates$forecast$start <- c(2023, 3)
  # Last quarter of forecast
  dates$forecast$end <- c(2024, 4)

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

  dates_current <- c(2023, 2)

  ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
      suppressWarnings(stats::window(ts_data[[x]], end = dates_current))
    })

  est <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates,
      options = list(
        gibbs = list(ndraws = 200), # fewer draws for speed
        fill = list(method = "mean")
      )
    )
  )

  # We expect the investment series to equal what we
  # inputed in the estimate_sem function. This is because the ragged edge is
  # between the estimation$end and forecast$start dates.
  exp_invest <- ts_data$investment
  expect_equal(
    level(est$ts_data$investment),
    exp_invest
  )

  # point forecast
  result <- withr::with_seed(7, forecast(est, dates))

  # y matrix should range from estimation start date to one quarter before
  # forecast start date
  expected_time <- stats::ts(seq(from = 1996.00, to = 2023.25, by = 0.25),
    start = 1996.00, frequency = 4
  )
  expect_identical(stats::time(result$y_matrix), expected_time)

  # investment ts should be uncut at start and range up to one quarter before
  # forecast start date
  expected_time <- stats::ts(seq(from = 1980.25, to = 2023.25, by = 0.25),
    start = 1980.25, frequency = 4
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
    2023.5, 2023.75, 2024, 2024.25, 2024.5,
    2024.75
  ), tsp = c(
    2023.5, 2024.75,
    4
  ), class = "ts")
  expect_identical(stats::time(result$mean[[1]]), expected_time)
})

test_that("validate_forecast_input stops when forecast dates are missing", {
  estimates <- list(
    sys_eq = list(exogenous_variables = NULL, endogenous_variables = NULL),
    ts_data = list()
  )
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2019, 4))
  )

  expect_error(
    koma:::validate_forecast_input(estimates, dates),
    "dates\\$forecast"
  )
})

test_that("validate_forecast_input stops when forecast start is after end", {
  estimates <- list(
    sys_eq = list(exogenous_variables = NULL, endogenous_variables = NULL),
    ts_data = list()
  )
  dates <- list(
    forecast = list(start = c(2020, 1), end = c(2019, 2))
  )

  expect_error(
    koma:::validate_forecast_input(estimates, dates),
    "start.*before.*end"
  )
})

test_that("validate_forecast_input stops on invalid forecast date format", {
  estimates <- list(
    sys_eq = list(exogenous_variables = NULL, endogenous_variables = NULL),
    ts_data = list()
  )
  dates <- list(
    forecast = list(start = c(2020, 5), end = c(2021, 1))
  )

  expect_error(
    koma:::validate_forecast_input(estimates, dates),
    "period must be between 1 and 4"
  )
})

test_that("validate_forecast_input stops on non-numeric forecast dates", {
  estimates <- list(
    sys_eq = list(exogenous_variables = NULL, endogenous_variables = NULL),
    ts_data = list()
  )
  dates <- list(
    forecast = list(start = "2020 Q1", end = "2021 Q1")
  )

  expect_error(
    koma:::validate_forecast_input(estimates, dates),
    "must be numeric"
  )
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
    estimate(ts_data, sys_eq, dates, options = list(gibbs = list(ndraws = 20)))
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
    estimate(ts_data, sys_eq, dates, options = list(gibbs = list(ndraws = 20)))
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
        options = list(probs = get_quantiles())
      )
    ),
    "Forecast horizon shortened to 7."
  )

  suppressWarnings(
    result <- withr::with_seed(
      7,
      forecast(
        estimates, dates,
        options = list(approximate = TRUE)
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
    estimation = list(start = c(1977, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 2), end = c(2025, 4))
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
    estimate(ts_data, sys_eq, dates, options = list(gibbs = list(ndraws = 200)))
  )

  out <- forecast(est, dates)

  expect_equal(names(out$mean), c("manufacturing", "world_gdp"))
})

test_that("forecast with one exogenous", {
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 2), end = c(2025, 4))
  )

  equations <- "manufacturing ~ world_gdp + manufacturing.L(1),
                service ~ service.L(1) + gdp,
                gdp == 0.5*manufacturing + 0.5*service"
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
    estimate(ts_data, sys_eq, dates, options = list(gibbs = list(ndraws = 200)))
  )

  out <- forecast(est, dates, options = list(approximate = FALSE))

  expect_equal(names(out$mean), c("manufacturing", "service", "gdp", "world_gdp"))
})

test_that("forecast with one AR equation", {
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 2), end = c(2025, 4))
  )

  equations <- "manufacturing ~ 0 + manufacturing.L(1)"
  exogenous_variables <- c()

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
    estimate(ts_data, sys_eq, dates, options = list(gibbs = list(ndraws = 200)))
  )

  out <- forecast(est, dates, options = list(approximate = TRUE))

  # Extract mean AR(1) coefficient (beta_jw has only the lag coef here)
  coef_mean <- quantiles_from_estimates(
    est$estimates$manufacturing$beta_jw,
    include_mean = TRUE
  )$q_mean

  phi_hat <- unname(coef_mean)

  y_end <- c(utils::tail(ts_data$manufacturing, 1))

  expect_equal(names(out$mean), c("manufacturing"))
  # First forecast should equal phi * last observed value
  expect_equal(out$mean$manufacturing[1], phi_hat * y_end, tolerance = 1e-8)
  # Second forecast propagates once more: phi^2 * last observed value
  expect_equal(out$mean$manufacturing[2], (phi_hat^2) * y_end, tolerance = 1e-8)
})

test_that("forecast without lags and with restrictions", {
  # no-lag restrictions do not propagate to later horizons #
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 2), end = c(2025, 4))
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
    estimate(ts_data, sys_eq, dates, options = list(gibbs = list(ndraws = 200)))
  )

  restrictions <- list(manufacturing = list(value = 0.5, horizon = 1))
  # Without restrictions
  base <- withr::with_seed(7, forecast(est, dates))

  # When forecasting without lags Phi matrix will be empty
  out <- withr::with_seed(7, forecast(est, dates, restrictions = restrictions))

  # first horizon of manufacturing should equal restriction
  # Restriction hit at h=1
  expect_equal(out$mean$manufacturing[1], 0.5)

  # No propagation to h=2 if there are no lags
  expect_equal(
    out$mean$manufacturing[2],
    base$mean$manufacturing[2],
    tolerance = 1e-12
  )
})

test_that("restricting aggregate alone keeps identity intact", {
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 2), end = c(2025, 4))
  )

  equations <- "manufacturing ~ world_gdp + manufacturing.L(1) + manufacturing.L(2),
      service ~ population + gdp,
      gdp == 0.5*manufacturing + 0.5*service"
  exogenous_variables <- c("world_gdp", "population")
  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data
  dates_current <- c(2023, 1)
  ts_data[sys_eq$endogenous_variables] <- lapply(
    sys_eq$endogenous_variables,
    function(x) stats::window(ts_data[[x]], end = dates_current)
  )

  est <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(gibbs = list(ndraws = 200)))
  )

  base <- withr::with_seed(7, forecast(est, dates))
  target <- base$mean$gdp[1] + 1 # feasible: adjust component shocks

  restrictions <- list(
    gdp = list(value = c(target, 0.5), horizon = c(1, 3)),
    manufacturing = list(value = base$mean$manufacturing[2], horizon = 2)
  )
  out <- withr::with_seed(7, forecast(est, dates, restrictions = restrictions))

  # Restriction is met
  expect_equal(out$mean$gdp[1], target)

  # Identity should still hold
  expect_equal(
    out$mean$gdp[1],
    0.5 * out$mean$manufacturing[1] + 0.5 * out$mean$service[1],
    tolerance = 1e-12
  )

  expect_equal(out$mean$gdp[3], 0.5)

  # manufacturing restriction should be met
  expect_equal(
    out$mean$manufacturing[2],
    base$mean$manufacturing[2]
  )
})

test_that("conflicting restrictions on identity error", {
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 2), end = c(2025, 4))
  )

  equations <- "manufacturing ~ world_gdp,
    service ~ population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"
  exogenous_variables <- c("world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data
  dates_current <- c(2023, 1)
  ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
      stats::window(ts_data[[x]], end = dates_current)
    })

  est <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(gibbs = list(ndraws = 200)))
  )

  base <- withr::with_seed(7, forecast(est, dates))

  restrictions <- list(
    manufacturing = list(value = base$mean$manufacturing[1], horizon = 1),
    service = list(value = base$mean$service[1], horizon = 1),
    gdp = list(value = base$mean$gdp[1] + 1, horizon = 1)
  )

  expect_error(
    withr::with_seed(7, forecast(est, dates,
      restrictions = restrictions,
      options = list(approximate = TRUE)
    )),
    "singular"
  )
  expect_error(
    withr::with_seed(7, forecast(est, dates, restrictions = restrictions)),
    "All forecast draws failed"
  )
})

test_that("forecast with restrictions for variables that are not in SEM", {
  dates <- list(
    estimation = list(start = c(1976, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 2), end = c(2025, 4))
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
    estimate(ts_data, sys_eq, dates, options = list(gibbs = list(ndraws = 200)))
  )

  restrictions <- list(
    manufacturing = list(value = 0.5, horizon = 1),
    consumption = list(value = 0.5, horizon = 1)
  )

  expect_warning(
    forecast(est, dates,
      restrictions = restrictions, options = list(approximate = TRUE)
    ),
    "Restriction\\(s\\)"
  )
  # When forecasting without lags Phi matrix will be empty
  out <- suppressWarnings(forecast(est, dates, restrictions = restrictions))

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
    estimation = list(start = c(1971, 1), end = c(2019, 4)),
    forecast = list(start = c(2020, 1), end = c(2025, 4))
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
  out <- forecast(est, dates,
    restrictions = restrictions, options = list(approximate = TRUE)
  )

  expect_equal(out$mean$manufacturing[2], 0.5)
})

test_that("forecast dates incomplete", {
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2018, 4)),
    forecast = list(start = c(2023, 2))
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
      options = list(gibbs = list(ndraws = 200)) # fewer draws for speed
    )
  )
  expect_error(forecast(estimates, dates), "Invalid")
})
