test_that("draw_parameters_j_informative returns parameters for equation 1", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1

  ##### Fix environment variables for test
  ## Gibbs sampler specifications
  set_gibbs_settings(settings = list(ndraws = 200), simulated_data$sys_eq$equation_settings)
  gibbs_settings <- get_gibbs_settings()
  gibbs_sampler <- gibbs_settings[[colnames(character_gamma_matrix)[jx]]]

  ## Specify priors
  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))

  number_of_exogenous <- ncol(x_matrix)

  # with informative priors
  priors <-
    list(
      list(
        constant = list(0, 1000),
        gdp = list(10, 0.001),
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
        gibbs_sampler,
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

test_that("draw_parameters_j_informative keeps every nstore-th draw after burn-in", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1
  priors <- list(list(), list(), list(), list(), list(), list())

  run_sampler <- function(nstore) {
    withr::with_seed(
      7,
      draw_parameters_j_informative(
        y_matrix,
        x_matrix,
        character_gamma_matrix,
        character_beta_matrix,
        jx,
        set_gibbs_spec(ndraws = 25, burnin_ratio = 0.2, nstore = nstore),
        priors
      )
    )
  }

  unthinned <- run_sampler(1)
  thinned <- run_sampler(3)

  # burnin = 5, nsave = floor(20 / 3) = 6
  expect_length(unthinned$beta_jw, 20)
  expect_length(thinned$beta_jw, 6)
  expect_length(thinned$gamma_jw, 6)
  expect_identical(thinned$beta_jw, unthinned$beta_jw[seq(3, 18, by = 3)])
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
  gibbs_settings <- get_gibbs_settings()
  gibbs_sampler <- gibbs_settings[[colnames(character_gamma_matrix)[jx]]]

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
        gibbs_sampler,
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
    0.772744717450134, 1.23727575301782, 1.77416087404064,
    0.381416590751618, 0.5011071706404, 0.597563290445894, 0.0802165245952577,
    0.198600542825458, 0.300398966872362
  ), dim = c(3L, 3L), dimnames = list(
    c("5%", "50%", "95%"), NULL
  ))

  expected_gamma <- c(
    `5%` = -0.642552418493153,
    `50%` = -0.390933915894648,
    `95%` = -0.184550552906983
  )

  expected_omega <- structure(c(
    0.473499323686895, 0.537843066036547, 0.649576888580231,
    -0.132506278085996, -0.0412457318372337, 0.0752179009027843,
    -0.132506278085996, -0.0412457318372338, 0.0752179009027843,
    0.307039095503668, 0.357928475403219, 0.414715769519724
  ), dim = c(3L, 2L, 2L), dimnames = list(c("5%", "50%", "95%"), NULL, NULL))

  # This sampler path drifts across BLAS/LAPACK implementations despite a fixed
  # seed (e.g. reference BLAS vs. Apple Accelerate), so use the same bounds as
  # the test without gamma priors below.
  expect_equal(beta_q, expected_beta, tolerance = 0.12)
  expect_lte(max(abs(unname(gamma_q) - unname(expected_gamma))), 0.28)
  expect_equal(omega_q, expected_omega, tolerance = 0.15)
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
  gibbs_settings <- get_gibbs_settings()
  gibbs_sampler <- gibbs_settings[[colnames(character_gamma_matrix)[jx]]]

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
        gibbs_sampler,
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

  # This sampler path shows small cross-environment drift despite a fixed seed
  # (BLAS/LAPACK-level floating point differences compounding over 200
  # iterations). CRAN's tests-MKL flavor exceeded the previous gamma bound
  # (0.2136 vs. 0.21); widen it with more headroom.
  expect_equal(beta_q, expected_beta, tolerance = 0.12)
  expect_lte(max(abs(unname(gamma_q) - unname(expected_gamma))), 0.28)
  expect_equal(omega_q, expected_omega, tolerance = 0.15)
})

test_that("target_j_informative adds the likelihood to the gamma prior", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1

  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))
  expect_gt(number_endogenous_in_j, 0)

  gamma_jw <- matrix(0.4, number_endogenous_in_j, 1)
  omega_jw <- diag(number_endogenous_in_j + 1)
  theta_jw <- matrix(0, ncol(x_matrix), number_endogenous_in_j + 1)
  gamma_mean <- matrix(0, number_endogenous_in_j, 1)
  gamma_vcv <- diag(number_endogenous_in_j)

  target <- function(priors_j) {
    target_j_informative(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix, jx,
      gamma_jw, omega_jw, theta_jw, priors_j
    )
  }

  likelihood_term <- target(list())
  prior_term <-
    -log(multivariate_norm_pdf(gamma_jw, mu = gamma_mean, sigma = gamma_vcv))

  # the data must enter the target, not only the prior
  expect_gt(abs(likelihood_term), 0)
  expect_equal(
    target(list(gamma_mean = gamma_mean, gamma_vcv = gamma_vcv)),
    prior_term + likelihood_term
  )
})

test_that("target_j_informative is finite for a tight gamma prior far away", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1

  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))

  target <- function(gamma) {
    target_j_informative(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix, jx,
      gamma_jw = matrix(gamma, number_endogenous_in_j, 1),
      omega_jw = diag(number_endogenous_in_j + 1),
      theta_jw = matrix(0, ncol(x_matrix), number_endogenous_in_j + 1),
      priors_j = list(
        gamma_mean = matrix(10, number_endogenous_in_j, 1),
        gamma_vcv = diag(0.001, number_endogenous_in_j)
      )
    )
  }

  # more than 300 prior standard deviations from the prior mean
  expect_true(is.finite(target(-0.35)))
  expect_true(is.finite(target(-0.3)))
  # the Metropolis-Hastings step compares the two, so they must differ
  expect_false(is.nan(target(-0.3) - target(-0.35)))
})

test_that("draw_parameters_j_informative lets the data update a gamma prior", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1

  # prior far from the data on the contemporaneous endogenous regressor only
  priors <-
    list(list(gdp = list(5, 1)), list(), list(), list(), list(), list())

  result <-
    withr::with_seed(
      7,
      draw_parameters_j_informative(
        y_matrix,
        x_matrix,
        character_gamma_matrix,
        character_beta_matrix,
        jx,
        set_gibbs_spec(ndraws = 1000, burnin_ratio = 0.5, nstore = 1),
        priors
      )
    )

  gamma_draws <- unlist(result$gamma_jw)

  # without a prior the posterior is around -0.4 with sd 0.16; a {5, 1} prior
  # may pull it slightly, but the posterior must not just reproduce the prior
  expect_lt(mean(gamma_draws), 1)
  expect_lt(sd(gamma_draws), 0.5)
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
