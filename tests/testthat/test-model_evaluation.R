test_that("model_evaluation", {
  horizon <- 4
  variables <- c("consumption", "investment")
  options <- list(ndraws = 200)

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

  result <- withr::with_seed(7, model_evaluation(
    sys_eq, variables, horizon, ts_data, dates,
    evaluate_on_levels = TRUE,
    options = list(ndraws = 20),
    point_forecast = NULL,
    restrictions = NULL
  ))

  expect_equal(nrow(result), horizon)
  expect_equal(colnames(result), variables)

  result <- withr::with_seed(7, model_evaluation(
    sys_eq, variables, horizon, ts_data, dates,
    evaluate_on_levels = FALSE,
    options = list(ndraws = 20),
    point_forecast = NULL,
    restrictions = NULL
  ))

  expect_equal(nrow(result), horizon)
  expect_equal(colnames(result), variables)

  # arguments in ... must be used
  expect_warning(
    model_evaluation(
      sys_eq, variables, horizon, ts_data, dates,
      unused = TRUE,
      options = list(ndraws = 20),
      point_forecast = NULL,
      restrictions = NULL
    )
  )

  variables <- c("gdpp", "investment")

  expect_error(
    model_evaluation(
      sys_eq, variables, horizon, ts_data, dates,
      evaluate_on_levels = TRUE,
      options = list(ndraws = 20),
      point_forecast = NULL,
      restrictions = NULL
    ), "gdpp"
  )
})

test_that("run_model_iteration", {
  horizon <- 4
  # global variables
  point_forecast <- list(active = TRUE, central_tendency = "mean")
  variables <- c("consumption", "investment")
  restrictions <- NULL
  sys_eq <- simulated_data$sys_eq
  evaluate_on_levels <- TRUE
  realized <- simulated_data$ts_data

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

  dates <- dates_to_num(dates, frequency = 4)

  # modify dates for i_params
  dates$in_sample$end <- iterate_n_periods(dates$forecast$start, -1, frequency = 4)

  ts_data <- simulated_data$ts_data
  ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
      stats::window(ts_data[[x]], end = dates$in_sample$end)
    })
  dates$estimation$end <- dates$in_sample$end
  dates$forecast$end <-
    iterate_n_periods(dates$forecast$start, horizon - 1, frequency = 4)
  i_params <- list(ts_data = ts_data, dates = dates)

  equation_settings <- sys_eq$equation_settings[sys_eq$stochastic_equations]
  set_gibbs_settings(settings = list(ndraws = 200), simulated_data$sys_eq$equation_settings)

  result <- withr::with_seed(
    7,
    run_model_iteration(
      i_params, point_forecast, variables, restrictions, sys_eq,
      evaluate_on_levels, realized,
      estimates = NULL
    )
  )
  expect_equal(names(result), c("error", "estimates"))
})

test_that("calculate_error", {
  time_index <- c(1, 2, 3, 4)

  realized <- ts(
    data = matrix(c(100, 200, 300, 400, 500, 600, 700, 800), ncol = 2),
    start = 1, frequency = 4
  )

  forecasts <- ts(
    data = matrix(c(90, 180, 290, 390, 480, 580, 680, 780), ncol = 2),
    start = 1, frequency = 4
  )

  # Expected result
  expected_errors <- as.data.frame(ts(
    data = matrix(c(100, 400, 100, 100, 400, 400, 400, 400), ncol = 2),
    start = 1, frequency = 4
  ))

  variables <- c("Series 1", "Series 2")
  result <- calculate_error(forecasts, realized, variables)

  expect_equal(result, expected_errors)
})
