test_that("draw_parameters_j_informative returns parameters for equation 1", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1

  ##### Fix environment variables for test
  ## Gibbs sampler specifications
  set_gibbs_settings(settings = list(ndraws = 200), simulated_data$sys_eq$equation_settings)

  ## Specify priors
  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))

  number_of_exogenous <- ncol(x_matrix)

  # with informative priors
  priors <-
    list(
      list(
        constant = list(0, 1000),
        gdp = list(10, 0.1),
        `consumption.L(1)` = list(0, 1000),
        `consumption.L(2)` = list(5, 0.01),
        epsilon = list(3, 0.001)
      ), list(), list(), list(), list(), list()
    )

  result <-
    withr::with_seed(
      7,
      draw_parameters_j_informative(
        y_matrix,
        x_matrix,
        character_gamma_matrix,
        character_beta_matrix,
        jx,
        priors
      )
    )

  # Percentiles for gamma
  gamma_q <- quantile(
    simplify2array(result$gamma_jw),
    prob = c(0.05, 0.5, 0.95)
  )
  beta_q <- apply(
    simplify2array(result$beta_jw), 1, quantile,
    prob = c(0.05, 0.5, 0.95)
  )

  # expect results to match prior for gdp and consumptionL2
  expect_equal(gamma_q[[2]], 10, tolerance = 0.05)
  expect_equal(beta_q[[2, 3]], 5, tolerance = 0.05)
})

test_that("draw_parameters_j_informative with diffuse priors", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1

  ##### Fix environment variables for test
  ## Gibbs sampler specifications
  set_gibbs_settings(settings = list(ndraws = 200), simulated_data$sys_eq$equation_settings)

  ## Specify priors
  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))

  number_of_exogenous <- ncol(x_matrix)

  # with diffuse priors
  priors <-
    list(
      list(
        constant = list(0, 1000),
        gdp = list(0, 1000),
        `consumption.L(1)` = list(0, 1000),
        `consumption.L(2)` = list(0, 1000),
        epsilon = list(3, 0.001)
      ), list(), list(), list(), list(), list()
    )

  result <-
    withr::with_seed(
      7,
      draw_parameters_j_informative(
        y_matrix,
        x_matrix,
        character_gamma_matrix,
        character_beta_matrix,
        jx,
        priors
      )
    )

  # Percentiles for beta
  # 50% corresponds to the posterior mean
  beta_q <- apply(
    simplify2array(result$beta_jw), 1, quantile,
    prob = c(0.05, 0.5, 0.95)
  )

  # Percentiles for gamma
  gamma_q <- quantile(
    simplify2array(result$gamma_jw),
    prob = c(0.05, 0.5, 0.95)
  )

  omega_q <- apply(
    simplify2array(result$omega_tilde_jw),
    seq_len(ncol(result$omega_tilde_jw[[1]])), stats::quantile,
    prob = c(0.05, 0.5, 0.95)
  )

  # True parameters of simulated data for equation 1 are:
  # consumption = 1.2 constant + 0.5 gdp + 0.5 consumptionL1 + 0.2 consumptionL2
  # beta contains constant, consumptionL1 and consumptionL2
  # gamma contains gdp
  # sigma is 0.5

  expected_beta <- structure(c(
    -1.07015324267421, 1.06053159574891, 2.48163945605022,
    0.337527525720606, 0.469649376673617, 0.634751542276184, 0.0142312245608084,
    0.21956041205714, 0.415034298410651
  ), dim = c(3L, 3L), dimnames = list(
    c("5%", "50%", "95%"), NULL
  ))

  expected_gamma <- c(
    `5%` = -3.21395432232359,
    `50%` = -0.772119407175861,
    `95%` = 0.947569664387587
  )

  expected_omega <- structure(c(
    0.536002192595383, 0.923982708419069, 3.68687021495534,
    -0.548111165459908, 0.103879654640507, 1.14116669300607, -0.548111165459908,
    0.103879654640507, 1.14116669300607, 0.316571701974609, 0.376360434884513,
    0.435419919519801
  ), dim = c(3L, 2L, 2L), dimnames = list(c("5%", "50%", "95%"), NULL, NULL))

  expect_equal(beta_q, expected_beta, tolerance = 0.05)
  expect_equal(gamma_q, expected_gamma, tolerance = 0.05)
  expect_equal(omega_q, expected_omega, tolerance = 0.05)
})

test_that("draw_parameters_j_informative with diffuse priors and no gamma priors", {
  skip_on_os("mac")
  skip_on_os("windows")

  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1

  ##### Fix environment variables for test
  ## Gibbs sampler specifications
  set_gibbs_settings(settings = list(ndraws = 200), simulated_data$sys_eq$equation_settings)

  ## Specify priors
  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))

  number_of_exogenous <- ncol(x_matrix)

  # with diffuse priors
  priors <-
    list(
      list(
        constant = list(0, 1000),
        `consumption.L(1)` = list(0, 1000),
        `consumption.L(2)` = list(0, 1000),
        epsilon = list(3, 0.001)
      ), list(), list(), list(), list(), list()
    )

  result <-
    withr::with_seed(
      7,
      draw_parameters_j_informative(
        y_matrix,
        x_matrix,
        character_gamma_matrix,
        character_beta_matrix,
        jx,
        priors
      )
    )

  # Percentiles for beta
  # 50% corresponds to the posterior mean
  beta_q <- apply(
    simplify2array(result$beta_jw), 1, quantile,
    prob = c(0.05, 0.5, 0.95)
  )

  # Percentiles for gamma
  gamma_q <- quantile(
    simplify2array(result$gamma_jw),
    prob = c(0.05, 0.5, 0.95)
  )

  omega_q <- apply(
    simplify2array(result$omega_tilde_jw),
    seq_len(ncol(result$omega_tilde_jw[[1]])), stats::quantile,
    prob = c(0.05, 0.5, 0.95)
  )

  # True parameters of simulated data for equation 1 are:
  # consumption = 1.2 constant + 0.5 gdp + 0.5 consumptionL1 + 0.2 consumptionL2
  # beta contains constant, consumptionL1 and consumptionL2
  # gamma contains gdp
  # sigma is 0.5

  expected_beta <- structure(c(
    0.831967884777193, 1.28323628998834, 1.81220257895342,
    0.368337564222256, 0.494145274488814, 0.575535385985741, 0.113124680405595,
    0.197199370265025, 0.319094251921225
  ), dim = c(3L, 3L), dimnames = list(c("5%", "50%", "95%"), NULL))

  expected_gamma <- c(
    `5%` = -0.666326868370084,
    `50%` = -0.275480153367785,
    `95%` = -0.0979837833542195
  )

  expected_omega <- structure(c(
    0.47578410057226, 0.547176303209218, 0.658666748334458,
    -0.137568323734368, -0.0449241478116035, 0.0793542029485639,
    -0.137568323734368, -0.0449241478116035, 0.0793542029485639,
    0.308866327509242, 0.360646340961647, 0.415597362863418
  ), dim = c(3L, 2L, 2L), dimnames = list(c("5%", "50%", "95%"), NULL, NULL))

  expect_equal(beta_q, expected_beta, tolerance = 0.05)
  expect_equal(gamma_q, expected_gamma, tolerance = 0.2)
  expect_equal(omega_q, expected_omega, tolerance = 0.1)
})

test_that("construct_priors_j, with two endogenous", {
  equations <-
    "consumption ~ {0.1,1000}1 + {0.4,0.1}gdp + {1,10}service + {0.9,10}consumption.L(1) + {0.1,1000}consumption.L(2) {4,0.002},
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.4*manufacturing + 0.6*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  priors <-
    list(
      list(
        constant = list(0.1, 1000),
        gdp = list(0.4, 0.1),
        service = list(1, 10),
        "consumption.L(1)" = list(0.9, 10),
        "consumption.L(2)" = list(0.1, 1000),
        epsilon = list(4, 0.002)
      ), list(), list(), list(), list(), list()
    )

  jx <- 1

  result <- construct_priors_j(
    priors, sys_eq$character_gamma_matrix, sys_eq$character_beta_matrix, jx
  )

  expected_result <-
    list(
      theta_mean = structure(c(
        0.1, 0.9, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ), dim = c(30L, 1L)),
      theta_vcv = structure(c(
        1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1000
      ), dim = c(30L, 30L)),
      omega_df = 4,
      omega_scale = structure(c(0.002, 0, 0, 0, 0.002, 0, 0, 0, 0.002),
        dim = c(3L, 3L)
      ),
      gamma_mean = structure(c(1, 0.4), dim = 2:1),
      gamma_vcv = structure(c(10, 0, 0, 0.1), dim = c(2L, 2L))
    )

  expect_equal(result, expected_result)
})

test_that("construct_priors_j, without endogenous", {
  equations <-
    "consumption ~ {0.1,1000}1 + gdp + {0.9,10}consumption.L(1) + {0.1,1000}consumption.L(2) {4,0.002},
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.4*manufacturing + 0.6*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  priors <-
    list(
      list(
        constant = list(0.1, 1000),
        "consumption.L(1)" = list(0.9, 10),
        "consumption.L(2)" = list(0.1, 1000),
        epsilon = list(4, 0.002)
      ), list(), list(), list(), list(), list()
    )

  jx <- 1

  result <- construct_priors_j(
    priors, sys_eq$character_gamma_matrix, sys_eq$character_beta_matrix, jx
  )


  expect_equal(
    names(result),
    c("theta_mean", "theta_vcv", "omega_df", "omega_scale")
  )
})
