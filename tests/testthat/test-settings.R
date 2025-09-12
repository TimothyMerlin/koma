test_that("build_settings", {
  equations <-
    "consumption ~ {0,1000}constant + {4,0.1}gdp + {9,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001} [burnin_ratio = 0.1],
    investment ~ gdp + current_account + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp [ndraws = 1000],
    manufacturing ~ manufacturing.L(1) + world_gdp [burnin_ratio = 0.3, unused = 0.4],
    service ~ service.L(1) + population + gdp [tau = 1],
    gdp == 0.5*manufacturing + 0.5*service"

  # in the above SEM, we have option `unused = 0.4` in the equation for
  # `manufacturing`
  # which is not a valid setting for the default (get_default_gibbs_spec())
  # and should be ignored

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  equation_settings <- sys_eq$equation_settings

  default <- get_default_gibbs_spec()

  result <- build_settings(
    default,
    settings = NULL,
    equation_settings
  )

  expected_result <- list(
    consumption = list(ndraws = 2000L, burnin_ratio = 0.1, nstore = 1, tau = 1.1),
    investment = list(ndraws = 2000L, burnin_ratio = 0.5, nstore = 1, tau = 1.1),
    current_account = list(ndraws = 1000, burnin_ratio = 0.5, nstore = 1, tau = 1.1),
    manufacturing = list(ndraws = 2000L, burnin_ratio = 0.3, nstore = 1, tau = 1.1),
    service = list(ndraws = 2000L, burnin_ratio = 0.5, nstore = 1, tau = 1),
    gdp = list(ndraws = 2000L, burnin_ratio = 0.5, nstore = 1, tau = 1.1)
  )

  expect_equal(result, expected_result)
})
