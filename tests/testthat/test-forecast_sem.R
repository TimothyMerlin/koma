test_that("forecast_sem", {
  sys_eq <- simulated_data$sys_eq
  exogenous_variables <- simulated_data$exogenous_variables
  estimates <- simulated_data$estimates
  restrictions <- NULL
  ts_data <- simulated_data$ts_data
  dates <- list(estimation = list(), forecast = list(), current = NULL)
  dates$estimation$start <- c(1977, 1)
  dates$estimation$end <- c(2018, 4) # use pre-COVID-19 data
  # Current quarter (published data)
  dates$current <- c(2018, 4)
  # Begin of forecasts
  dates$forecast$start <- c(2019, 1)
  # Last quarter of forecast
  dates$forecast$end <- c(2019, 2)
  # Dates for Dynamic Weights calculation
  dates$dynamic_weights$start <- c(1992, 1)
  dates$dynamic_weights$end <- c(2018, 4)
  dates <- dates_to_num(dates, frequency = 4)

  # Define identity weights
  sys_eq$identities$gdp$weights$theta6_4 <- 0.5
  sys_eq$identities$gdp$weights$theta6_5 <- 0.5

  # Forecast horizon
  horizon <- length(seq(dates$forecast$start, dates$forecast$end, by = 1 / 4))

  ##### Create Lagged Variables
  ts_data <- create_lagged_variables(
    ts_data, sys_eq$endogenous_variables,
    exogenous_variables, sys_eq$predetermined_variables
  )
  ##### Construct Y and X matrix up to estimation_end
  balanced_data <- construct_balanced_data(
    ts_data, sys_eq$endogenous_variables,
    sys_eq$total_exogenous_variables,
    dates$estimation$start, dates$estimation$end
  )
  y_matrix <- balanced_data$y_matrix
  forecast_x_matrix <- stats::window(
    as_mets(ts_data[sys_eq$exogenous_variables]),
    start = dates$forecast$start,
    end = dates$forecast$end
  )

  freq <- 4
  # Case 1: Point estimates
  result <- forecast_sem(
    sys_eq, estimates, restrictions,
    y_matrix, forecast_x_matrix, horizon, freq,
    forecast_dates = dates$forecast,
    point_forecast = list(active = TRUE, central_tendency = "median")
  )

  expect_equal(names(result), c("mean", "median"))

  expected_result <- structure(
    c(
      5.04104607100856, 4.7708005173572, 5.01602359998076,
      4.06003947968627, 0.541094781344398, 1.13145534077617, 0.450303373639633,
      0.540847907446731, 0.116717828312384, -0.119322631598908, 0.283510600976009,
      0.210762637923912
    ),
    dim = c(2L, 6L),
    dimnames = list(
      NULL,
      c("consumption", "investment", "current_account", "manufacturing", "service", "gdp")
    ), tsp = c(2019, 2019.25, 4),
    class = c("mts", "ts", "matrix", "array")
  )

  expect_equal(result$median, expected_result)

  # Case 2: Density forecasts
  result <- withr::with_seed(
    7,
    forecast_sem(
      sys_eq, estimates, restrictions,
      y_matrix, forecast_x_matrix, horizon, freq, dates$forecast,
      point_forecast = list(active = FALSE)
    )
  )
  expect_equal(names(result$quantiles), c("q_5", "q_50", "q_95"))
  expected_cols <- c(
    "consumption", "investment", "current_account",
    "manufacturing", "service", "gdp"
  )
  expected_tsp <- c(2019, 2019.25, 4)

  lapply(result$quantiles, function(q) {
    expect_true(inherits(q, "ts"))
    expect_equal(dim(q), c(2L, 6L))
    expect_equal(colnames(q), expected_cols)
    expect_equal(stats::tsp(q), expected_tsp)
  })

  tol <- 1e-8
  q_5 <- result$quantiles$q_5
  q_50 <- result$quantiles$q_50
  q_95 <- result$quantiles$q_95
  expect_true(all(q_5 <= q_50 + tol))
  expect_true(all(q_50 <= q_95 + tol))
})

test_that("validate_identities warns when components are missing", {
  mat <- matrix(
    c(
      1, 1,
      2, 2
    ),
    ncol = 2,
    byrow = TRUE
  )
  colnames(mat) <- c("agg", "c1")
  ts_out <- stats::ts(mat, start = c(2023, 2), frequency = 4)

  expect_warning(
    validate_identities(ts_out, identities = list(
      agg = list(
        components = list(c1 = "w1", c2 = "w2"),
        weights = list(w1 = 0.5, w2 = 0.5)
      )
    )),
    "could not be checked"
  )
})

test_that("validate_identities warns when weights are non-numeric", {
  mat <- matrix(
    c(
      1, 1, 1,
      2, 2, 2
    ),
    ncol = 3,
    byrow = TRUE
  )
  colnames(mat) <- c("agg", "c1", "c2")
  ts_out <- stats::ts(mat, start = c(2023, 2), frequency = 4)

  expect_warning(
    validate_identities(ts_out, identities = list(
      agg = list(
        components = list(c1 = "w1", c2 = "w2"),
        weights = list(w1 = 0.5, w2 = "theta")
      )
    )),
    "could not be checked"
  )
})


test_that("validate_identities warns on deviations", {
  # Construct a simple ts_out with one identity variable and two components
  mat <- matrix(
    c(
      1, 2, 2, # identity too low
      2, 2, 2 # identity matches
    ),
    ncol = 3,
    byrow = TRUE
  )
  colnames(mat) <- c("agg", "c1", "c2")
  ts_out <- stats::ts(mat, start = c(2023, 2), frequency = 4)

  expect_warning(
    validate_identities(ts_out,
      identities = list(
        agg = list(
          components = list(c1 = "w1", c2 = "w2"),
          weights = list(w1 = 0.5, w2 = 0.5)
        )
      )
    ),
    "Identity"
  )
})

test_that("validate_identities is quiet when identities match", {
  mat <- matrix(
    c(
      1, 1, 1,
      2, 2, 2
    ),
    ncol = 3,
    byrow = TRUE
  )
  colnames(mat) <- c("agg", "c1", "c2")
  ts_out <- stats::ts(mat, start = c(2023, 2), frequency = 4)

  expect_silent(
    validate_identities(ts_out, identities = list(
      agg = list(
        components = list(c1 = "w1", c2 = "w2"),
        weights = list(w1 = 0.5, w2 = 0.5)
      )
    ))
  )
})
