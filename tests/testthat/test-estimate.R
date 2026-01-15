test_that("new_prepare_estimation", {
  ts_data <- simulated_data$ts_data
  sys_eq <- simulated_data$sys_eq
  point_forecast <- list(active = TRUE, central_tendency = "median")
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  out <- new_prepare_estimation(ts_data, sys_eq, dates,
    point_forecast = point_forecast
  )

  expect_length(out, 4)
})

test_that("validate_estimation_dates stops when estimation dates are missing", {
  dates <- list()

  expect_error(
    koma:::validate_estimation_dates(dates),
    "dates\\$estimation"
  )
})

test_that("validate_estimation_dates stops when start is after end", {
  dates <- list(
    estimation = list(start = c(2020, 2), end = c(2019, 4))
  )

  expect_error(
    koma:::validate_estimation_dates(dates),
    "start.*before.*end"
  )
})

test_that("validate_estimation_dates stops on invalid estimation date format", {
  dates <- list(
    estimation = list(start = c(2020, 5), end = c(2021, 1))
  )

  expect_error(
    koma:::validate_estimation_dates(dates),
    "period must be between 1 and 4"
  )
})

test_that("estimate correctly estimates model", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  equations <-
    "consumption ~ gdp + consumption.L(1:2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  # Test 1: Case estimates the SEM (with only 200 draws per equation)
  out <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )

  expect_length(out, 6)
  expect_identical(
    names(out$estimates),
    c(
      "consumption", "investment", "current_account", "manufacturing",
      "service"
    )
  )
  # We expect estimates to contain 100 draws because of the 200 set, the first
  # half is discarded.
  expect_length(out$estimates$consumption[["beta_jw"]], 100)
  expect_true(inherits(out, "koma_estimate"))

  format(out)
  print(out)

  ## Default - Returns summary statistics.
  result_summary <- summary(out, variables = "consumption", use_texreg = FALSE)
  expected_summary <- koma:::summary_statistics(
    "consumption", out$estimates, out$sys_eq
  )
  expect_s3_class(result_summary, "koma_summary")
  expect_equal(result_summary$stats, expected_summary)
  expect_identical(result_summary$variables, "consumption")

  if (requireNamespace("texreg", quietly = TRUE)) {
    result_summary <- summary(
      out,
      variables = "consumption",
      use_texreg = TRUE
    )

    expected_texreg <- koma:::summary_statistics(
      "consumption", out$estimates, out$sys_eq
    )$consumption

    tol <- if (Sys.getenv("CI") == "true") 1e-1 else 1e-9
    expect_s3_class(result_summary, "koma_texreg")
    expect_equal(attr(result_summary, "koma_digits"), 2)
    expect_match(attr(result_summary, "koma_custom_note"), "Posterior mean")
    expect_equal(
      result_summary$consumption@coef.names,
      expected_texreg$coef.names
    )
    expect_equal(
      result_summary$consumption@coef,
      expected_texreg$coef,
      tolerance = tol
    )
    expect_equal(
      result_summary$consumption@ci.low,
      expected_texreg$ci.low,
      tolerance = tol
    )
    expect_equal(
      result_summary$consumption@ci.up,
      expected_texreg$ci.up,
      tolerance = tol
    )
    expect_equal(result_summary$consumption@model.name, "KOMA")
    expect_equal(
      length(result_summary$consumption@pvalues),
      length(expected_texreg$coef)
    )
  }
})

test_that("estimate correctly returns when parallel", {
  skip_if_not_installed(c("parallelly", "future"))

  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

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

  out <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )

  expect_length(out, 6)
  expect_identical(
    names(out$estimates),
    c(
      "consumption", "investment", "current_account", "manufacturing",
      "service"
    )
  )
  expect_true(inherits(out, "koma_estimate"))
})

test_that("estimate works for ragged edge", {
  skip_if_not_installed(c("parallelly", "future"))

  # mock readline function always return "y"
  testthat::local_mocked_bindings(readline = function(...) "y", .package = "base")

  dates <- list(estimation = list(
    start = c(1976, 1),
    end = c(2023, 1)
  ))

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

  # use future::plan for parallel execution
  workers <- parallelly::availableCores(omit = 1)

  # Windows: Since Windows does not support forking, multisession
  # will spawn new R sessions for each parallel worker.
  # Testing here if all global variables are exported correctly for mutlisession.
  future::plan(future::multisession, workers = workers)

  suppressWarnings(
    result <- withr::with_seed(
      7,
      estimate(ts_data, sys_eq, dates,
        options = list(ndraws = 200),
        point_forecast = list(active = TRUE, central_tendency = "median")
      )
    )
  )

  # y and x matrices and current account and investment time series should
  # range from estimation start date to one quarter before forecast start date
  expected_time <- stats::ts(seq(from = 1976.50, to = 2023.00, by = 0.25),
    start = 1976.50, frequency = 4
  )
  expect_identical(stats::time(result$y_matrix), expected_time)
  expect_identical(stats::time(result$x_matrix), expected_time)

  expected_time <- stats::ts(seq(from = 1976.00, to = 2023.00, by = 0.25),
    start = 1976.00, frequency = 4
  )
  expect_identical(
    stats::time(result$ts_data$current_account),
    expected_time
  )
  expect_identical(
    stats::time(result$ts_data$investment),
    expected_time
  )
})

test_that("estimate throws error if model is unidentified", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ service + world_gdp,
    service ~ manufacturing + gdp,
    gdp == 0.5*manufacturing + 0.5*service "

  exogenous_variables <- c("real_interest_rate", "world_gdp")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  expect_error(
    estimate(ts_data, sys_eq, dates),
    "Model identification error: rank"
  )
})

test_that("summary works correctly", {
  out_estimation <- structure(
    list(
      estimates = simulated_data$estimates,
      sys_eq = simulated_data$sys_eq
    ),
    class = "koma_estimate"
  )

  result_summary <- summary(out_estimation, use_texreg = FALSE)
  expected_summary <- koma:::summary_statistics(
    names(simulated_data$estimates),
    simulated_data$estimates,
    simulated_data$sys_eq
  )
  expect_s3_class(result_summary, "koma_summary")
  expect_equal(result_summary$stats, expected_summary)
  expect_identical(result_summary$variables, names(simulated_data$estimates))
})

test_that("estimate throws error", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

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

  # arguments in ... must be used
  expect_warning(
    estimate(ts_data, sys_eq, dates,
      unused = TRUE, unused2 = 1,
      options = list(ndraws = 20)
    )
  )

  ts_data <- simulated_data$ts_data
  attr(ts_data$gdp, "method") <- NA

  # type mismatch of sys_eq
  expect_error(
    estimate(ts_data, dates, dates),
    "`sys_eq` must be of class"
  )

  # y_matrix or x_matrix contain NA values (move estimation start and warn)
  equations <-
    "real_interest_rate ~ gdp + service,
    investment ~  investment.L(1) + world_gdp"

  exogenous_variables <- c("world_gdp", "gdp", "service")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data
  ts_data$investment[1] <- NA
  attr(ts_data$real_interest_rate, "method") <- "none"
  dates$estimation$start <- c(1975, 1)
  expect_warning(
    estimate(ts_data, sys_eq, dates),
    "Estimation start moved to"
  )
})

test_that("missing series in ts_data", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

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
  # missing variables in ts_data
  ts_data$gdp <- NULL

  expect_error(
    estimate(ts_data, sys_eq, dates),
    "The following series are missing in `ts_data`"
  )
})

test_that("dates not provided", {
  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == (nom_manufacturing)*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data
  ts_data$nom_manufacturing <- ts_data$gdp

  # no dates
  dates <- list()
  expect_error(
    estimate(ts_data, sys_eq, dates),
    "Invalid"
  )

  # incomplete estimation dates
  dates <- list(estimation = list(
    end = c(2019, 4)
  ))
  expect_error(
    estimate(ts_data, sys_eq, dates),
    "Invalid"
  )

  # incomplete dynamic weight dates
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))
  expect_error(
    estimate(ts_data, sys_eq, dates),
    "Invalid"
  )
})

test_that("print", {
  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == (nom_consumption/nom_gdp)*consumption - (nom_service/nom_gdp)*service"

  # Vector of exogenous variables
  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)
  sys_eq$identities$gdp$weights$theta6_1 <- 1
  sys_eq$identities$gdp$weights$theta6_5 <- -0.1

  x <- structure(
    list(
      estimates = simulated_data$estimates,
      sys_eq = sys_eq
    ),
    class = "koma_estimate"
  )

  output <- testthat::capture_output(result <- print(x))
  expect_match(output, "consumption", fixed = TRUE)
  expect_true(inherits(result, "koma_estimate"))
})

test_that("estimate correctly reestimates model", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  estimates <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )
  expect_equal(length(estimates$estimates$manufacturing$beta_jw[[1]]), 3)
  expect_true(!"current_account" %in% names(estimates$estimates))

  # Test 1: Case reestimate SEM with changed equation
  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    current_account ~ world_gdp + current_account.L(1),
    manufacturing ~ manufacturing.L(1) + manufacturing.L(2) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  reestimates <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates,
      options = list(ndraws = 200),
      estimates = estimates
    )
  )

  expect_equal(
    reestimates$estimates$consumption, estimates$estimates$consumption
  )
  expect_equal(
    reestimates$estimates$service, estimates$estimates$service
  )
  expect_equal(names(reestimates$estimates), sys_eq$stochastic_equations)
  expect_equal(names(reestimates$estimates), sys_eq$stochastic_equations)
  # manufacturing was reestimated, with now 4 coefficients
  expect_equal(length(reestimates$estimates$manufacturing$beta_jw[[1]]), 4)
  expect_equal(length(reestimates$estimates$current_account$beta_jw[[1]]), 3)
  # Test 2: Case reestimate SEM with additional endogenous
  expect_true("current_account" %in% names(reestimates$estimates))
  # Test 3: Case remove endogenous from estimates
  expect_true(!"investment" %in% names(reestimates$estimates))

  # Test 4: Nothing to reestimate
  out <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates,
      options = list(ndraws = 200),
      estimates = reestimates
    )
  )
  expect_equal(out, reestimates)

  out <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )

  # seeds are equivalent
  expect_equal(
    attributes(out$estimates)$rng, attributes(estimates$estimates)$rng
  )

  # expect_equal(out$estimates$consumption, estimates$estimates$consumption)
  gibbs_settings <- get_gibbs_settings()
  gibbs_sampler <- gibbs_settings[[1]]

  draw_cons_out <- withr::with_seed(
    7,
    draw_parameters_j(
      out$y_matrix,
      out$x_matrix,
      out$sys_eq$character_gamma_matrix,
      out$sys_eq$character_beta_matrix,
      1,
      gibbs_sampler
    )
  )
  draw_cons_rest <- withr::with_seed(
    7,
    draw_parameters_j(
      reestimates$y_matrix,
      reestimates$x_matrix,
      reestimates$sys_eq$character_gamma_matrix,
      reestimates$sys_eq$character_beta_matrix,
      1,
      gibbs_sampler
    )
  )
  expect_equal(draw_cons_out, draw_cons_rest)
  draw_cons_est <- withr::with_seed(
    7,
    draw_parameters_j(
      estimates$y_matrix,
      estimates$x_matrix,
      estimates$sys_eq$character_gamma_matrix,
      estimates$sys_eq$character_beta_matrix,
      1,
      gibbs_sampler
    )
  )
  # expect_equal(draw_cons_rest, draw_cons_est)
})

test_that("estimate correctly estimates model with informative priors", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  equations <-
    "consumption ~ {0,1000}constant + {4,0.1}gdp + {9,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001},
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  # Test 1: Case estimates the SEM (with only 200 draws per equation)
  out <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )

  expect_length(out, 6)
  expect_identical(
    names(out$estimates),
    c(
      "consumption", "investment", "current_account", "manufacturing",
      "service"
    )
  )

  result_summary <- summary(out, variables = "consumption", use_texreg = FALSE)
  coef <- result_summary$stats$consumption$coef

  # gdp coefficient expected at 4
  expect_equal(coef[["gdp"]], 4, tolerance = 0.5)
  # consumption.L(1) coefficient expected at 9 with smaller variance
  expect_equal(coef[["consumption.L(1)"]], 9, tolerance = 0.2)
})

test_that("estimate with informative priors, that are too far from true value", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  # here setting prior mean of gdp to 1000 with very high certainty
  equations <-
    "consumption ~ {0,1000}constant + {1000,0.00001}gdp + {9,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001},
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  # Test 1: Case estimates the SEM (with only 200 draws per equation)
  out <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )

  expect_length(out, 6)
  expect_identical(
    names(out$estimates),
    c(
      "consumption", "investment", "current_account", "manufacturing",
      "service"
    )
  )

  # MCMC step is never accepted for consumption
  expect_equal(mean(out$estimates$consumption$count_accepted), 0)
})

test_that("estimate with no gamma parameters", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  equations <-
    "manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population"

  exogenous_variables <- c("world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  # Test 1: Case estimates the SEM (with only 200 draws per equation)
  out <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )

  expect_length(out, 6)
  expect_identical(
    names(out$estimates),
    c("manufacturing", "service")
  )
  expect_true(inherits(out, "koma_estimate"))
  expect_length(out$estimates$manufacturing[["beta_jw"]], 100)
  expect_length(out$estimates$service[["beta_jw"]], 100)
})

test_that("estimate with only one exogenous variable", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  equations <-
    "manufacturing ~ -1 + world_gdp,
    service ~ -1 + population"
  exogenous_variables <- c("world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  # Test 1: Case estimates the SEM (with only 200 draws per equation)
  out <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )

  expect_length(out, 6)
  expect_identical(
    names(out$estimates),
    c("manufacturing", "service")
  )
  expect_true(inherits(out, "koma_estimate"))
  expect_length(out$estimates$manufacturing[["beta_jw"]], 100)
  expect_length(out$estimates$service[["beta_jw"]], 100)

  # Case: informative
  equations <-
    "manufacturing ~ -1 + {0.5,0.001}world_gdp,
    service ~ -1 population"
  exogenous_variables <- c("world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  out <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )

  expect_length(out, 6)
  expect_identical(
    names(out$estimates),
    c("manufacturing", "service")
  )
  expect_true(inherits(out, "koma_estimate"))
  expect_length(out$estimates$manufacturing[["beta_jw"]], 100)
  expect_length(out$estimates$service[["beta_jw"]], 100)
})

test_that("estimate with only one equation", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  equations <- "manufacturing ~ world_gdp"
  exogenous_variables <- c("world_gdp")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  # Test 1: Case estimates the SEM (with only 200 draws per equation)
  out <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )

  expect_length(out, 6)
  expect_identical(names(out$estimates), c("manufacturing"))
  expect_true(inherits(out, "koma_estimate"))
  expect_length(out$estimates$manufacturing[["beta_jw"]], 100)

  # Case: informative
  equations <- "manufacturing ~ {0,1000}constant + world_gdp"
  exogenous_variables <- c("world_gdp")

  sys_eq <- system_of_equations(equations, exogenous_variables)
  out <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
  )

  expect_length(out, 6)
  expect_identical(names(out$estimates), c("manufacturing"))
  expect_true(inherits(out, "koma_estimate"))
  expect_length(out$estimates$manufacturing[["beta_jw"]], 100)
})

test_that("estimate an AR(1) model", {
  # Case: AR(1) model
  equations <- "manufacturing ~ -1 + manufacturing.L(1)"
  exogenous_variables <- c()

  sys_eq <- system_of_equations(equations, exogenous_variables)

  # Generate AR(1) process
  n <- 200 # Number of observations
  phi <- 0.8 # AR(1) coefficient
  sigma <- 1 # Standard deviation of white noise
  epsilon <- withr::with_seed(7, rnorm(n, mean = 0, sd = sigma))
  y <- numeric(n) # Initialize time series

  y[1] <- epsilon[1]
  for (t in 2:n) {
    y[t] <- phi * y[t - 1] + epsilon[t]
  }

  ts_data <- list(manufacturing = ets(y, start = c(1970, 1), frequency = 4, series_type = "rate", method = "percentage"))

  dates <- list(estimation = list(
    start = c(1971, 1),
    end = c(2019, 4)
  ))

  out <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates)
  )

  result <- extract_estimates_from_draws(sys_eq, out$estimates, central_tendency = "mean")
  expect_equal(
    result$gamma_matrix[[1, 1]],
    1
  )
  expect_equal(
    result$beta_matrix[[1, 1]],
    0
  )
  expect_equal(
    result$beta_matrix[[2, 1]],
    phi,
    tolerance = 0.1
  )
  expect_equal(
    result$sigma_matrix[[1, 1]],
    sigma,
    tolerance = 0.1
  )

  # Case: AR(1) model with informative prior
  equations <- "manufacturing ~ -1 + {0,1000}manufacturing.L(1) + {1, 5}"
  exogenous_variables <- c()

  sys_eq <- system_of_equations(equations, exogenous_variables)

  out <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates)
  )

  result <- extract_estimates_from_draws(sys_eq, out$estimates, central_tendency = "mean")
  expect_equal(
    result$gamma_matrix[[1, 1]],
    1
  )
  expect_equal(
    result$beta_matrix[[1, 1]],
    0
  )
  expect_equal(
    result$beta_matrix[[2, 1]],
    phi,
    tolerance = 0.1
  )
  expect_equal(
    result$sigma_matrix[[1, 1]],
    sigma,
    tolerance = 0.1
  )
})

test_that("estimate with equation specific tau", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  equations <-
    "consumption ~ {0,1000}constant + {4,0.1}gdp + {9,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001} [tau = 1.9],
    investment ~ gdp + current_account + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  estimates_1 <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates,
      options = list(ndraws = 2000)
    )
  )

  expect_equal(estimates_1$gibbs_specifications$consumption$tau, 1.9)
  expect_equal(estimates_1$gibbs_specifications$investment$tau, 1.1)

  equations <-
    "consumption ~ {0,1000}constant + {4,0.1}gdp + {9,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001} [tau = 1.1],
    investment ~ gdp + current_account + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  estimates_2 <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates,
      options = list(ndraws = 200)
    )
  )

  # lower acceptance rate for smaller tau (case specific)
  expect_true(mean(estimates_1$estimates$consumption$count_accepted) < mean(estimates_2$estimates$consumption$count_accepted))
})

test_that("estimate with equation specific gibbs options", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  equations <-
    "consumption ~ {0,1000}constant + {4,0.1}gdp + {9,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001} [tau = 0.9],
    investment ~ gdp + current_account + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp [ndraws = 1000, tau = 1.2],
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  estimates <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates,
      options = list(ndraws = 200)
    )
  )

  expected_result <- list(
    consumption = set_gibbs_spec(ndraws = 200, tau = 0.9),
    investment = set_gibbs_spec(ndraws = 200),
    current_account = set_gibbs_spec(ndraws = 1000, tau = 1.2),
    manufacturing = set_gibbs_spec(ndraws = 200),
    service = set_gibbs_spec(ndraws = 200)
  )
  expect_equal(estimates$gibbs_specifications, expected_result)

  expect_equal(length(estimates$estimates$current_account$beta_jw), 500)
  expect_equal(length(estimates$estimates$consumption$beta_jw), 100)
})

test_that("estimate, accpetance probability", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  equations <-
    "consumption ~ {0,1000}constant + {4,0.1}gdp + {9,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001} [ndraws = 200, acceptance_prob = c(0.2, 0.75)],
    investment ~ gdp + current_account + investment.L(1) + real_interest_rate [acceptance_prob = c(0.3, 0.5)],
    current_account ~ current_account.L(1) + world_gdp [tau = 1.2],
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp [acceptance_prob = c(0.5, 0.5)],
    gdp == 0.5*manufacturing + 0.5*service"

  # in the above SEM, we force trigger the warning that the acceptance
  # probability is not in the range for service

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data

  estimates <- withr::with_seed(
    7,
    estimate(ts_data, sys_eq, dates,
      options = list(ndraws = 500)
    )
  )

  expected_result <- list(
    consumption = set_gibbs_spec(ndraws = 200),
    investment = set_gibbs_spec(ndraws = 500),
    current_account = set_gibbs_spec(ndraws = 500, tau = 1.2),
    manufacturing = set_gibbs_spec(ndraws = 500),
    service = set_gibbs_spec(ndraws = 500)
  )
  expect_equal(estimates$gibbs_specifications, expected_result)
})

test_that("estimate, ts provided instead of ets", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  equations <-
    "consumption ~ gdp + consumption.L(1:2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- lapply(simulated_data$ts_data, as.ts)

  # mock response to YES with y
  expect_error(
    testthat::with_mocked_bindings(
      {
        withr::with_seed(
          7,
          estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
        )
      },
      readline = local({
        responses <- c("y", "n")
        i <- 0
        function(...) {
          i <<- i + 1
          responses[i]
        }
      }),
      .package = "base"
    ), NA
  )
})

test_that("estimate, ts provided instead of ets", {
  dates <- list(estimation = list(
    start = c(1977, 1),
    end = c(2019, 4)
  ))

  equations <-
    "consumption ~ gdp + consumption.L(1:2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- lapply(simulated_data$ts_data, as.ts)

  # mock response NO with n,
  # set series type to level and method to percentage
  expect_error(
    testthat::with_mocked_bindings(
      {
        withr::with_seed(
          7,
          estimate(ts_data, sys_eq, dates, options = list(ndraws = 200))
        )
      },
      readline = local({
        responses <- c("n", "level", "percentage", "n")
        i <- 0
        function(...) {
          i <<- i + 1
          responses[i]
        }
      }),
      .package = "base"
    ), NA
  )
})

test_that("convert_ts_data_to_ets applies defaults and exceptions", {
  ts_data <- lapply(simulated_data$ts_data[c("consumption", "manufacturing", "service")], as.ts)

  result <- testthat::with_mocked_bindings(
    {
      koma:::convert_ts_data_to_ets(ts_data)
    },
    readline = local({
      responses <- c("n", "level", "diff_log", "y", "manufacturing", "rate", "none", "y", "service", "level", "percentage", "n")
      i <- 0
      function(...) {
        i <<- i + 1
        responses[i]
      }
    }),
    .package = "base"
  )

  expect_true(all(sapply(result, inherits, "koma_ts")))
  expect_identical(attr(result$consumption, "series_type"), "level")
  expect_identical(attr(result$consumption, "method"), "diff_log")
  expect_identical(attr(result$manufacturing, "series_type"), "rate")
  expect_identical(attr(result$manufacturing, "method"), "none")
  expect_identical(attr(result$service, "series_type"), "level")
  expect_identical(attr(result$service, "method"), "percentage")
})

test_that("extract.koma_estimate returns texreg objects", {
  skip_if_not_installed("texreg")

  out_estimation <- structure(
    list(
      estimates = simulated_data$estimates,
      sys_eq = simulated_data$sys_eq
    ),
    class = "koma_estimate"
  )

  extracted <- texreg::extract(out_estimation)
  expect_type(extracted, "list")
  expect_true(all(names(simulated_data$estimates) %in% names(extracted)))
  expect_true(inherits(extracted$consumption, "texreg"))

  expected <- koma:::summary_statistics(
    "consumption",
    simulated_data$estimates,
    simulated_data$sys_eq
  )$consumption

  tol <- if (Sys.getenv("CI") == "true") 1e-1 else 1e-9
  expect_equal(extracted$consumption@coef.names, expected$coef.names)
  expect_equal(extracted$consumption@coef, expected$coef, tolerance = tol)
  expect_equal(extracted$consumption@ci.low, expected$ci.low, tolerance = tol)
  expect_equal(extracted$consumption@ci.up, expected$ci.up, tolerance = tol)
  expect_equal(length(extracted$consumption@pvalues), length(expected$coef))
  expect_equal(extracted$consumption@model.name, "KOMA")

  extracted_one <- texreg::extract(out_estimation, variables = "consumption")
  expect_true(inherits(extracted_one, "texreg"))
  expect_equal(extracted_one@coef, expected$coef, tolerance = tol)
})

test_that("summary errors when texreg missing and use_texreg TRUE", {
  out_estimation <- structure(
    list(
      estimates = simulated_data$estimates,
      sys_eq = simulated_data$sys_eq
    ),
    class = "koma_estimate"
  )

  expect_error(
    testthat::with_mocked_bindings(
      summary(out_estimation, use_texreg = TRUE),
      check_texreg_installed = function() FALSE,
      .env = environment(summary.koma_estimate)
    ),
    "texreg"
  )
})

test_that("summary falls back to non-texreg output when texreg missing", {
  out_estimation <- structure(
    list(
      estimates = simulated_data$estimates,
      sys_eq = simulated_data$sys_eq
    ),
    class = "koma_estimate"
  )

  expect_warning(
    testthat::with_mocked_bindings(
      summary(out_estimation),
      check_texreg_installed = function() FALSE,
      .env = environment(summary.koma_estimate)
    ),
    "texreg"
  )

  result_summary <-
    testthat::with_mocked_bindings(
      suppressWarnings(summary(out_estimation)),
      check_texreg_installed = function() FALSE,
      .env = environment(summary.koma_estimate)
    )

  expected_summary <- koma:::summary_statistics(
    names(simulated_data$estimates),
    simulated_data$estimates,
    simulated_data$sys_eq
  )
  expect_s3_class(result_summary, "koma_summary")
  expect_equal(result_summary$stats, expected_summary)
})

test_that("summary.koma_estimate, change bounds", {
  out_estimation <- structure(
    list(
      estimates = simulated_data$estimates,
      sys_eq = simulated_data$sys_eq
    ),
    class = "koma_estimate"
  )

  capture_summary <- function(...) {
    testthat::capture_output(
      suppressWarnings(
        testthat::with_mocked_bindings(
          print(summary(out_estimation, ...)),
          check_texreg_installed = function() FALSE,
          .env = environment(summary.koma_estimate)
        )
      )
    )
  }

  # default should match
  out_default <- capture_summary()
  out_default_explicit <- capture_summary(ci_low = 5, ci_up = 95)
  out_changed <- capture_summary(ci_low = 1, ci_up = 90)

  expect_true(identical(out_default, out_default_explicit))
  expect_false(identical(out_default, out_changed))
})

test_that("summary.koma_estimate, respects digits", {
  out_estimation <- structure(
    list(
      estimates = simulated_data$estimates,
      sys_eq = simulated_data$sys_eq
    ),
    class = "koma_estimate"
  )

  capture_summary <- function(...) {
    testthat::capture_output(
      suppressWarnings(
        testthat::with_mocked_bindings(
          print(summary(out_estimation, ...)),
          check_texreg_installed = function() FALSE,
          .env = environment(summary.koma_estimate)
        )
      )
    )
  }

  out2 <- capture_summary(variables = "consumption", digits = 2)
  out4 <- capture_summary(variables = "consumption", digits = 4)

  expect_match(out2, "1.84", fixed = TRUE)
  expect_match(out4, "1.8361", fixed = TRUE)
  expect_false(identical(out2, out4))
})

test_that("summary.koma_estimate respects digits in texreg output", {
  out_estimation <- structure(
    list(
      estimates = simulated_data$estimates,
      sys_eq = simulated_data$sys_eq
    ),
    class = "koma_estimate"
  )

  if (requireNamespace("texreg", quietly = TRUE)) {
    out_texreg <- testthat::capture_output(
      print(summary(
        out_estimation,
        variables = "consumption",
        digits = 5
      ))
    )
    expect_match(out_texreg, "1.83612", fixed = TRUE)
  }
})

test_that("print.koma_estimate filters variables", {
  out_estimation <- structure(
    list(
      estimates = simulated_data$estimates,
      sys_eq = simulated_data$sys_eq
    ),
    class = "koma_estimate"
  )

  output <- testthat::capture_output(
    print(out_estimation, variables = c("consumption", "investment"))
  )

  expect_match(output, "consumption", fixed = TRUE)
  expect_match(output, "investment", fixed = TRUE)
  expect_no_match(output, "service")

  expect_error(
    print(out_estimation, variables = "does_not_exist"),
    "not part of this estimate"
  )
})

test_that("print.koma_estimate, respects digits", {
  out_estimation <- structure(
    list(
      estimates = simulated_data$estimates,
      sys_eq = simulated_data$sys_eq
    ),
    class = "koma_estimate"
  )

  out2 <- testthat::capture_output(
    print(out_estimation, variables = "consumption", digits = 2)
  )

  out4 <- testthat::capture_output(
    print(out_estimation, variables = "consumption", digits = 4)
  )

  expect_match(out2, "0.36", fixed = TRUE)
  expect_match(out4, "0.3562", fixed = TRUE)
  expect_false(identical(out2, out4))
})

test_that("format.koma_estimate avoids substring replacement", {
  equations <- "y ~ world_gdp + y.L(1) + world_gdp_level.L(1),
  world_gdp_level == 1*world_gdp + 1*world_gdp_level.L(1)"
  sys_eq <- system_of_equations(equations, exogenous_variables = "world_gdp")

  beta_draws <- replicate(5, c(1.1, 0.5, 0.3, 2.0), simplify = FALSE)
  gamma_draws <- replicate(5, NA_real_, simplify = FALSE)

  estimates <- list(
    y = list(beta_jw = beta_draws, gamma_jw = gamma_draws)
  )

  est <- structure(
    list(estimates = estimates, sys_eq = sys_eq),
    class = "koma_estimate"
  )

  formatted <- cli::ansi_strip(paste(format(est), collapse = "\n"))

  expect_match(
    formatted,
    "0\\.3\\s*\\*\\s*world_gdp_level\\.L\\(1\\)"
  )
  expect_false(grepl(
    "0\\.3\\s*\\*\\s*2\\s*\\*\\s*world_gdp_level\\.L\\(1\\)",
    formatted
  ))
})
