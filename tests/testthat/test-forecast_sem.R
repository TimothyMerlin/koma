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
  result <- forecast_sem(
    sys_eq, estimates, restrictions,
    y_matrix, forecast_x_matrix, horizon, freq, dates$forecast,
    point_forecast = list(active = FALSE)
  )

  expected_result <- list(q_5 = structure(c(
    4.82109556789144, 4.45690748680653, 4.69440248787175,
    3.69915350979583, 0.495309268912805, 1.09223507673112, 0.340604402361223,
    0.458155682564863, -0.0945899767006442, -0.276137308345971, 0.173976039089989,
    0.131057178557279
  ), dim = c(2L, 6L), dimnames = list(NULL, c(
    "consumption",
    "investment", "current_account", "manufacturing", "service",
    "gdp"
  )), tsp = c(2019, 2019.25, 4), class = c(
    "mts", "ts", "matrix",
    "array"
  )), q_50 = structure(c(
    5.05832496333041, 4.80065339358621,
    5.04117872496396, 4.0749759248586, 0.542239285623722, 1.13220681376785,
    0.451883808436417, 0.544678037809438, 0.116324325070415, -0.114688250842263,
    0.285346399263274, 0.212590703116717
  ), dim = c(2L, 6L), dimnames = list(
    NULL, c(
      "consumption", "investment", "current_account", "manufacturing",
      "service", "gdp"
    )
  ), tsp = c(2019, 2019.25, 4), class = c(
    "mts",
    "ts", "matrix", "array"
  )), q_95 = structure(c(
    5.32692712901112,
    5.1776618816249, 5.41784726334038, 4.5176449902336, 0.588127922973782,
    1.16954018238363, 0.564936580079121, 0.630732940266279, 0.363333409635949,
    0.0794230808871807, 0.413541744733043, 0.306758740571166
  ), dim = c(
    2L,
    6L
  ), dimnames = list(NULL, c(
    "consumption", "investment", "current_account",
    "manufacturing", "service", "gdp"
  )), tsp = c(2019, 2019.25, 4), class = c("mts", "ts", "matrix", "array")))

  expect_equal(result$quantiles, expected_result)
})

test_that("check_identities_add_up warns when components are missing", {
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
    check_identities_add_up(ts_out, identities = list(
      agg = list(
        components = list(c1 = "w1", c2 = "w2"),
        weights = list(w1 = 0.5, w2 = 0.5)
      )
    )),
    "could not be checked"
  )
})

test_that("check_identities_add_up warns when weights are non-numeric", {
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
    check_identities_add_up(ts_out, identities = list(
      agg = list(
        components = list(c1 = "w1", c2 = "w2"),
        weights = list(w1 = 0.5, w2 = "theta")
      )
    )),
    "could not be checked"
  )
})


test_that("check_identities_add_up warns on deviations", {
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
    check_identities_add_up(ts_out,
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

test_that("check_identities_add_up is quiet when identities match", {
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
    check_identities_add_up(ts_out, identities = list(
      agg = list(
        components = list(c1 = "w1", c2 = "w2"),
        weights = list(w1 = 0.5, w2 = 0.5)
      )
    ))
  )
})
