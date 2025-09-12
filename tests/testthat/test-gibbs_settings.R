test_that("set gibbs specs", {
  result <- set_gibbs_spec(
    ndraws = c(2000, 1000, 50, 1000), burnin_ratio = c(0.2, 0.3, 0.4, 0.5)
  )

  expected_result <- structure(
    list(
      ndraws = c(2000, 1000, 50, 1000),
      burnin_ratio = c(0.2, 0.3, 0.4, 0.5),
      nstore = c(1, 1, 1, 1),
      tau = c(1.1, 1.1, 1.1, 1.1),
      burnin = c(400, 300, 20, 500),
      nsave = c(1600, 700, 30, 500)
    ),
    class = "gibbs_spec"
  )

  expect_equal(result, expected_result)
})

test_that("set gibbs specs, with unused args", {
  result <- set_gibbs_spec(
    ndraws = 1020, burnin_ratio = c(0.1, 0.4), nstore = 2, tau = 1, unused = "string"
  )

  expected_result <- structure(
    list(
      ndraws = c(1020, 1020),
      burnin_ratio = c(0.1, 0.4),
      nstore = c(2, 2),
      tau = c(1, 1),
      burnin = c(102, 408),
      nsave = c(459, 306)
    ),
    class = "gibbs_spec"
  )

  expect_equal(result, expected_result)
})

test_that("set_gibbs_spec, error on non-integer", {
  expect_error(
    set_gibbs_spec(ndraws = 1000.1), "ndraws must be a non-negative integer"
  )
})

test_that("validate_integerish, integer", {
  x <- 20
  name <- "ndraws"
  expect_no_error(validate_integerish(x, name))
})

test_that("validate_integerish, vector", {
  x <- c(200, 1000, 20L)
  name <- "ndraws"
  expect_no_error(validate_integerish(x, name))
})

test_that("validate_integerish, error", {
  x <- c(200, 1000.2, 20L)
  name <- "ndraws"
  expect_error(
    validate_integerish(x, name),
    "ndraws must be a non-negative integer"
  )
})

test_that("validate_integerish, error", {
  x <- c(-200, 1000, 20L)
  name <- "ndraws"
  expect_error(
    validate_integerish(x, name),
    "ndraws must be a non-negative integer"
  )
})

test_that("set_gibbs_settings, default", {
  equations <-
    "consumption ~ {0,1000}constant + {4,0.1}gdp + {9,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001},
    investment ~ gdp + current_account + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)
  equation_settings <- sys_eq$equation_settings[sys_eq$stochastic_equations]

  set_gibbs_settings(settings = NULL, equation_settings)

  result <- the$gibbs_sampler

  default_gibbs_spec <- set_gibbs_spec()

  expected_result <- structure(
    list(
      consumption = default_gibbs_spec,
      investment = default_gibbs_spec,
      current_account = default_gibbs_spec,
      manufacturing = default_gibbs_spec,
      service = default_gibbs_spec
    )
  )

  expect_equal(result, expected_result)
})

test_that("set_gibbs_settings, global user override", {
  equations <-
    "consumption ~ {0,1000}constant + {4,0.1}gdp + {9,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001},
    investment ~ gdp + current_account + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)
  equation_settings <- sys_eq$equation_settings[sys_eq$stochastic_equations]

  settings <- list(ndraws = 200)

  set_gibbs_settings(settings, equation_settings)

  result <- the$gibbs_sampler

  global_gibbs_spec <- set_gibbs_spec(ndraws = 200)

  expected_result <- structure(
    list(
      consumption = global_gibbs_spec,
      investment = global_gibbs_spec,
      current_account = global_gibbs_spec,
      manufacturing = global_gibbs_spec,
      service = global_gibbs_spec
    )
  )

  expect_equal(result, expected_result)
})

test_that("set_gibbs_settings, equation specific gibbs settings", {
  equations <-
    "consumption ~ {0,1000}constant + {4,0.1}gdp + {9,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001} [burnin_ratio = 0.1],
    investment ~ gdp + current_account + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp [ndraws = 1000],
    manufacturing ~ manufacturing.L(1) + world_gdp [ndraws = 300, burnin_ratio = 0.3],
    service ~ service.L(1) + population + gdp [tau = 1],
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)
  equation_settings <- sys_eq$equation_settings[sys_eq$stochastic_equations]

  set_gibbs_settings(settings = NULL, equation_settings)

  result <- the$gibbs_sampler

  expected_result <- structure(
    list(
      consumption = set_gibbs_spec(burnin_ratio = 0.1),
      investment = set_gibbs_spec(),
      current_account = set_gibbs_spec(ndraws = 1000),
      manufacturing = set_gibbs_spec(ndraws = 300, burnin_ratio = 0.3),
      service = set_gibbs_spec(tau = 1)
    )
  )

  expect_equal(result, expected_result)
})

test_that("set_gibbs_settings, global and equation specific gibbs settings", {
  equations <-
    "consumption ~ {0,1000}constant + {4,0.1}gdp + {9,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001} [burnin_ratio = 0.1],
    investment ~ gdp + current_account + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp [ndraws = 1000],
    manufacturing ~ manufacturing.L(1) + world_gdp [ndraws = 300, burnin_ratio = 0.3],
    service ~ service.L(1) + population + gdp [tau = 1],
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)
  equation_settings <- sys_eq$equation_settings[sys_eq$stochastic_equations]

  set_gibbs_settings(settings = list(ndraws = 1500), equation_settings)

  result <- the$gibbs_sampler

  expected_result <- list(
    consumption = set_gibbs_spec(ndraws = 1500, burnin_ratio = 0.1),
    investment = set_gibbs_spec(ndraws = 1500),
    current_account = set_gibbs_spec(ndraws = 1000),
    manufacturing = set_gibbs_spec(ndraws = 300, burnin_ratio = 0.3),
    service = set_gibbs_spec(ndraws = 1500, tau = 1)
  )

  expect_equal(result, expected_result)
})

test_that("set_gibbs_settings, ignores unused equation settings", {
  equations <-
    "consumption ~ {0,1000}constant + {4,0.1}gdp + {9,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001} [burnin_ratio = 0.1],
    investment ~ gdp + current_account + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp [ndraws = 1000, unused = 30],
    manufacturing ~ manufacturing.L(1) + world_gdp [ndraws = 300, burnin_ratio = 0.3],
    service ~ service.L(1) + population + gdp [tau = 1],
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)
  equation_settings <- sys_eq$equation_settings[sys_eq$stochastic_equations]

  set_gibbs_settings(settings = NULL, equation_settings)

  result <- the$gibbs_sampler

  expected_result <- list(
    consumption = set_gibbs_spec(burnin_ratio = 0.1),
    investment = set_gibbs_spec(),
    current_account = set_gibbs_spec(ndraws = 1000),
    manufacturing = set_gibbs_spec(ndraws = 300, burnin_ratio = 0.3),
    service = set_gibbs_spec(tau = 1)
  )

  expect_equal(result, expected_result)
})

test_that("get_gibbs_settings, specific equation", {
  spec <- list(
    consumption = set_gibbs_spec(burnin_ratio = 0.1),
    investment = set_gibbs_spec(),
    current_account = set_gibbs_spec(ndraws = 1000),
    manufacturing = set_gibbs_spec(ndraws = 300, burnin_ratio = 0.3),
    service = set_gibbs_spec(tau = 1)
  )
  assign("gibbs_sampler", spec, envir = the)

  result <- get_gibbs_settings(equation = "service")

  expected_result <- set_gibbs_spec(tau = 1)
  expect_equal(result, expected_result)
})

test_that("print_gibbs_settings", {
  equations <-
    "consumption ~ {0,1000}constant + {4,0.1}gdp + {9,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001} [burnin_ratio = 0.1],
    investment ~ gdp + current_account + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp [ndraws = 1000],
    manufacturing ~ manufacturing.L(1) + world_gdp [burnin_ratio = 0.3],
    service ~ service.L(1) + population + gdp [tau = 1],
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  spec <- list(
    consumption = set_gibbs_spec(ndraws = 2000L, burnin_ratio = 0.1, nstore = 1, tau = 1.1),
    investment = set_gibbs_spec(ndraws = 2000L, burnin_ratio = 0.5, nstore = 1, tau = 1.1),
    current_account = set_gibbs_spec(ndraws = 1000, burnin_ratio = 0.5, nstore = 1, tau = 1.1),
    manufacturing = set_gibbs_spec(ndraws = 2000L, burnin_ratio = 0.3, nstore = 1, tau = 1.1),
    service = set_gibbs_spec(ndraws = 2000L, burnin_ratio = 0.5, nstore = 1, tau = 1)
  )
  assign("gibbs_sampler", spec, envir = the)

  # overwritting deafult with global settings
  settings <- list(ndraws = 1000)
  default <- get_default_gibbs_spec()
  default <- utils::modifyList(default, settings)
  global_settings <- do.call(set_gibbs_spec, default)

  out <- cli::cli_fmt({
    print_gibbs_settings(global_settings)
  })

  # **system-wide header**
  expect_true(any(grepl("Gibbs Sampler Settings", out)))
  expect_true(any(grepl("System Wide Settings", out)))

  # **one of the bullets under system-wide**
  expect_true(any(grepl("Number of draws.*1000", out)))

  # **per-equation header and override**
  expect_true(any(grepl("consumption", out)))
  expect_true(any(grepl("ndraws.*2000", out)))
})
