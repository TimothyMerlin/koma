test_that("estimate_sem", {
  sys_eq <- simulated_data$sys_eq
  weights <- list(gdp = list(manufacturing = -1.5, service = 0.5))
  sys_eq$identities <-
    update_identity_weights(weights, sys_eq$identities)

  x_matrix <- simulated_data$x_matrix
  y_matrix <- simulated_data$y_matrix

  # set default specifications
  equation_settings <- sys_eq$equation_settings[sys_eq$stochastic_equations]
  set_gibbs_settings(settings = list(ndraws = 200), simulated_data$sys_eq$equation_settings)

  # Test 1: Case estimates the SEM (with only 200 draws per equation)
  estimates <- withr::with_seed(7, estimate_sem(sys_eq, y_matrix, x_matrix))

  expected_estimated_eq <- c(
    "consumption", "investment", "current_account", "manufacturing",
    "service"
  )
  expect_identical(names(estimates), expected_estimated_eq)
  expect_length(estimates$consumption[["beta_jw"]], 100)

  # Test 2: Case reestimate parts of the SEM
  reestimates <- withr::with_seed(
    7,
    estimate_sem(sys_eq, y_matrix, x_matrix, eq_jx = c(1, 3L))
  )

  expect_identical(names(reestimates), c("consumption", "current_account"))
  expect_identical(
    names(reestimates$current_account),
    c(
      "beta_jw", "theta_jw", "gamma_jw", "omega_jw", "omega_tilde_jw",
      "count_accepted"
    )
  )
  expect_length(reestimates$consumption[["beta_jw"]], 100)

  # same seed should give same results
  expect_equal(estimates$consumption, reestimates$consumption)
})

test_that("estimate_sem with priors", {
  equations <-
    "consumption ~ {0,1000}constant + {2,0.01}gdp + {3,0.001}consumption.L(1) + {0,1000}consumption.L(2) + {3,0.001},
   investment ~ gdp + investment.L(1) + real_interest_rate,
   current_account ~ current_account.L(1) + world_gdp,
   manufacturing ~ manufacturing.L(1) + world_gdp,
   service ~ service.L(1) + population + gdp,
   gdp == -1.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  weights <- list(gdp = list(manufacturing = -1.5, service = 0.5))
  sys_eq$identities <-
    update_identity_weights(weights, sys_eq$identities)

  x_matrix <- simulated_data$x_matrix
  y_matrix <- simulated_data$y_matrix

  # set default specifications
  equation_settings <- sys_eq$equation_settings[sys_eq$stochastic_equations]
  set_gibbs_settings(settings = list(ndraws = 200), simulated_data$sys_eq$equation_settings)

  # Test 1: Case estimates the SEM (with only 200 draws per equation)
  estimates <- withr::with_seed(7, estimate_sem(sys_eq, y_matrix, x_matrix))

  expected_estimated_eq <- c(
    "consumption", "investment", "current_account", "manufacturing",
    "service"
  )
  expect_identical(names(estimates), expected_estimated_eq)
  expect_length(estimates$consumption[["beta_jw"]], 100)

  # Percentiles for beta
  # 50% corresponds to the posterior mean
  beta_q <- apply(
    simplify2array(estimates$consumption$beta_jw), 1, quantile,
    prob = c(0.05, 0.5, 0.95)
  )

  # Percentiles for gamma
  gamma_q <- quantile(
    simplify2array(estimates$consumption$gamma_jw),
    prob = c(0.05, 0.5, 0.95)
  )

  omega_q <- apply(
    simplify2array(estimates$consumption$omega_tilde_jw),
    seq_len(ncol(estimates$consumption$omega_tilde_jw[[1]])), stats::quantile,
    prob = c(0.05, 0.5, 0.95)
  )

  # consumption.L(1) coefficient expected at 2
  expect_equal(beta_q[[2, 2]], 3, tolerance = 0.2)
  # gdp coefficient expected at 2
  expect_equal(gamma_q[[2]], 2, tolerance = 0.2)
})

test_that("estimate_sem error in equation j", {
  # when not using witr::local_* covr::package_coverage() fails for
  # multiple tests. This test seems to somehow interfere / have adverse effects
  # on other tests (possible due to global variables).
  withr::local_envvar() # Ensures environment variables are restored
  withr::local_options() # Ensures options are reset after the test
  withr::local_seed(123) # Ensures any random seeds are isolated

  sys_eq <- simulated_data$sys_eq
  weights <- list(gdp = list(manufacturing = -1.5, service = 0.5))
  sys_eq$identities <- update_identity_weights(weights, sys_eq$identities)

  x_matrix <- simulated_data$x_matrix
  y_matrix <- simulated_data$y_matrix

  # trigger error
  y_matrix[, "consumption"] <- 1
  # set default specifications
  equation_settings <- sys_eq$equation_settings[sys_eq$stochastic_equations]
  set_gibbs_settings(settings = list(ndraws = 200), simulated_data$sys_eq$equation_settings)

  suppressWarnings(
    result <- estimate_sem(sys_eq, y_matrix, x_matrix)
  )
  expect_equal(result$consumption, NULL)
})
