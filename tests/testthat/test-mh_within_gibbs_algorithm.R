test_that("draw_parameters returns correct parameters for equation 1", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1

  ##### Fix environment variables for test
  ## Gibbs sampler specifications
  set_gibbs_settings(settings = NULL, simulated_data$sys_eq$equation_settings)
  gibbs_settings <- get_gibbs_settings()
  gibbs_sampler <- gibbs_settings[[colnames(character_gamma_matrix)[jx]]]

  result <-
    withr::with_seed(
      7,
      draw_parameters_j(
        y_matrix,
        x_matrix,
        character_gamma_matrix,
        character_beta_matrix,
        jx,
        gibbs_sampler
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
    0.766708163554736, 1.30198086868547, 1.8641199520759,
    0.375690517877449, 0.482808139140976, 0.591091226099493, 0.0920661493549338,
    0.203018478345194, 0.308526113032514
  ), dim = c(3L, 3L), dimnames = list(
    c("5%", "50%", "95%"), NULL
  ))

  expected_gamma <- c(
    `5%` = -0.671805581196672,
    `50%` = -0.358204333963515,
    `95%` = -0.0865327358691923
  )

  expected_omega <- structure(c(
    0.491956171794485, 0.591616829592262, 0.721437572166347,
    -0.185753176464255, -0.0597760055744618, 0.0659311830255429,
    -0.185753176464255, -0.0597760055744618, 0.0659311830255429,
    0.311349190997226, 0.370710489664196, 0.443122340788915
  ), dim = c(
    3L,
    2L, 2L
  ), dimnames = list(c("5%", "50%", "95%"), NULL, NULL))

  expect_equal(beta_q, expected_beta, tolerance = 0.05)
  expect_equal(gamma_q, expected_gamma, tolerance = 0.05)
  expect_equal(omega_q, expected_omega, tolerance = 0.05)
})

test_that("draw_parameters returns correct structure for gamma for an equation
without endogenous variables, that is when there are only lagged variables or
exogenous variables in the equation", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 4

  ##### Fix environment variables for test
  ## Gibbs sampler specifications
  set_gibbs_settings(settings = NULL, simulated_data$sys_eq$equation_settings)
  gibbs_settings <- get_gibbs_settings()
  gibbs_sampler <- gibbs_settings[[colnames(character_gamma_matrix)[jx]]]

  result <-
    withr::with_seed(
      7,
      draw_parameters_j(
        y_matrix,
        x_matrix,
        character_gamma_matrix,
        character_beta_matrix,
        jx,
        gibbs_sampler
      )
    )
  expected_gamma <- replicate(1000, NA, simplify = FALSE)

  expect_identical(result$gamma_jw, expected_gamma)
})

# Test Initialize Sampler
test_that("initialize_sampler correctly maximizes the target target function
when there is one endogenous variable", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1

  result_with_endogenous <- initialize_sampler(
    y_matrix,
    x_matrix,
    character_gamma_matrix,
    character_beta_matrix,
    jx
  )
  expect_equal(
    result_with_endogenous$gamma_parameters_j,
    structure(-0.34996818653039, dim = c(1L, 1L))
  )
  expect_equal(
    result_with_endogenous$cholesky_of_inverse_hessian,
    structure(0.175744390195533, dim = c(1L, 1L))
  )
})

test_that("initialize_sampler correctly maximizes the target target function
when there are no endogenous variables in equation", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 6

  result_without_endogenous <- initialize_sampler(
    y_matrix,
    x_matrix,
    character_gamma_matrix,
    character_beta_matrix,
    jx
  )
  expect_identical(result_without_endogenous$gamma_parameters_j, 0)
  expect_identical(result_without_endogenous$cholesky_of_inverse_hessian, NA)
})

# Test Draw Gamma j
test_that("draw_gamma_j correctly accepts the candidate gamma paramater for
the one endogenous variable case", {
  set.seed(100)

  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1
  tau <- 1.1
  gamma_parameters_1 <- structure(-0.34996818653039, dim = c(1L, 1L))
  cholesky_of_inverse_hessian <- structure(0.175744390195533, dim = c(1L, 1L))

  new_gamma_parameters_1 <- draw_gamma_j(
    y_matrix,
    x_matrix,
    character_gamma_matrix,
    character_beta_matrix,
    jx,
    gamma_parameters_1,
    tau,
    cholesky_of_inverse_hessian
  )
  expect_equal(
    new_gamma_parameters_1,
    structure(-0.475581731258366, dim = c(1L, 1L))
  )
})

test_that("draw_gamma_j correctly rejects the candidate gamma paramater for
the one endogenous variable case", {
  set.seed(7)

  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1
  tau <- 1.1
  gamma_parameters_1 <- structure(-0.34996818653039, dim = c(1L, 1L))
  cholesky_of_inverse_hessian <- structure(0.175744390195533, dim = c(1L, 1L))

  new_gamma_parameters_1 <- draw_gamma_j(
    y_matrix,
    x_matrix,
    character_gamma_matrix,
    character_beta_matrix,
    jx,
    gamma_parameters_1,
    tau,
    cholesky_of_inverse_hessian
  )
  expect_equal(
    new_gamma_parameters_1,
    gamma_parameters_1
  )
})

test_that("draw_gamma_j returns 0 when there are no endogenous variables", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 6
  tau <- 1.1
  gamma_parameters_6 <- 0
  cholesky_of_inverse_hessian <- NA

  result <- draw_gamma_j(
    y_matrix,
    x_matrix,
    character_gamma_matrix,
    character_beta_matrix,
    jx,
    gamma_parameters_6,
    tau,
    cholesky_of_inverse_hessian
  )

  expect_true(is.na(result))
})

# Test Draw Omega j
test_that("draw_omega_j correctly returns the Omega", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1
  gamma_parameters_1 <- structure(-0.34996818653039, dim = c(1L, 1L))

  result <- withr::with_seed(
    7,
    draw_omega_j(
      y_matrix, x_matrix, character_gamma_matrix,
      character_beta_matrix, jx, gamma_parameters_1
    )
  )

  expected_result_omega_tilde_jw <- matrix(c(
    0.471327192836488, -0.0987055617257134, -0.0987055617257134,
    0.416628322906185
  ), nrow = 2, ncol = 2, byrow = TRUE)
  expected_result_omega_jw <- matrix(c(
    0.591442497614644, -0.244512220350389, -0.244512220350389,
    0.416628322906185
  ), nrow = 2, ncol = 2, byrow = TRUE)

  expect_equal(result$omega_tilde_jw, expected_result_omega_tilde_jw)
  expect_equal(result$omega_jw, expected_result_omega_jw)
})

# Test Theta j
test_that("draw_theta_j returns the correct theta_jw and beta_jw in
the one endogenous variables case", {
  skip_on_os("mac")
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1
  gamma_parameters_j <- structure(-0.36, dim = c(1L, 1L))
  omega_tilde_jw <- matrix(c(
    0.17, 0.22, 0.22, 0.82
  ), nrow = 2, ncol = 2, byrow = TRUE)

  result <- withr::with_seed(
    7,
    draw_theta_j(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
      jx, gamma_parameters_j, omega_tilde_jw
    )
  )

  expected_result_theta_jw <- c(
    1.33785213690464, 0.553200107134962, 0.132042350013386,
    0.395058533772247, 0.0262040643823067,
    -0.127070095171351, -0.017387429595745, -0.150109902894841,
    -0.100230761546029, 0.0219065823268252, -0.0852778937502962,
    -0.24161797439946, -0.047251981429159
  )

  expected_result_beta_jw <- c(
    1.33785213690464, 0.553200107134962, 0.132042350013386
  )

  expect_equal(result$theta_jw, expected_result_theta_jw, tolerance = 0.2)
  beta_tol <- if (Sys.getenv("CI") == "true") 0.2 else 0.1
  expect_equal(result$beta_jw, expected_result_beta_jw, tolerance = beta_tol)
})

test_that("draw_theta_j returns the correct theta_jw and beta_jw in
the no endogenous variables case", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  # Equation 3 does not contain endogenous variables:
  # current_account == constant + current_accountL1 + world_gdp + epsilon # nolint
  jx <- 3
  gamma_parameters_j <- 0
  omega_tilde_jw <- matrix(c(
    0.08
  ), nrow = 1, byrow = TRUE)

  result <- withr::with_seed(
    7,
    draw_theta_j(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
      jx, gamma_parameters_j, omega_tilde_jw
    )
  )

  expected_result_theta_jw <- c(
    1.62903143338742, -0.577349603700648, 0.501512145598412
  )

  expected_result_beta_jw <- c(
    1.62903143338742, -0.577349603700648, 0.501512145598412
  )

  expect_equal(result$theta_jw, expected_result_theta_jw)
  expect_equal(result$beta_jw, expected_result_beta_jw)
})

# Test Target j
test_that("target_j correctly computes the target function
 for the jth equation", {
  # Test single parameter
  x_matrix <- simulated_data$x_matrix
  y_matrix <- simulated_data$y_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1
  parameters <- 0

  result <- target_j(
    y_matrix,
    x_matrix,
    character_gamma_matrix,
    character_beta_matrix,
    jx,
    parameters
  )

  # Check that the result is a single double value
  expect_type(result, "double")
  expect_equal(length(result), 1)

  # Check that the result is finite (not Inf, -Inf, or NaN)
  expect_true(is.finite(result))

  # Run the function with the same input data but different paramters
  new_parameters <- 1
  new_result <- target_j(
    y_matrix,
    x_matrix,
    character_gamma_matrix,
    character_beta_matrix,
    jx,
    new_parameters
  )

  # Check that the result has changed
  expect_false(isTRUE(all.equal(result, new_result)))

  # Test when too many parameters
  parameters <- c(0, 1)
  expect_error(
    target_j(
      y_matrix,
      x_matrix,
      character_gamma_matrix,
      character_beta_matrix,
      jx,
      parameters
    )
  )
})

test_that("target_j returns NA when there are no gamma parameters
 in the jth equation", {
  # Test case when there is no gamma coefficient in equation j and
  # therefore no parameter to target
  x_matrix <- simulated_data$x_matrix
  y_matrix <- simulated_data$y_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 6

  parameters <- NA

  suppressWarnings(
    result <- target_j(
      y_matrix,
      x_matrix,
      character_gamma_matrix,
      character_beta_matrix,
      jx,
      parameters
    )
  )

  expect_true(is.na(result))
})
