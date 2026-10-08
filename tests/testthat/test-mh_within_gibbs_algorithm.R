test_that("draw_parameters_j handles an equation with no free coefficients", {
  x_matrix <- cbind(1, seq_len(20))
  y_matrix <- cbind(rep(c(-1, 1), 10), seq_len(20))
  character_gamma_matrix <- diag(2)
  character_beta_matrix <- matrix(c("0", "0", "beta_12", "beta_22"), 2)
  gibbs_sampler <- new_gibbs_spec(6, 0.5, 1, 1.1)

  result <- draw_parameters_j(
    y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
    1, gibbs_sampler
  )

  expect_length(result$beta_jw, 3)
  expect_true(all(lengths(result$beta_jw) == 0))
  expect_equal(result$theta_jw, rep(list(matrix(0, 2, 1)), 3))
  expect_true(all(is.na(unlist(result$gamma_jw))))
  expect_true(all(is.finite(unlist(result$omega_jw))))
  expect_true(all(unlist(result$omega_jw) > 0))
  expect_equal(result$omega_tilde_jw, result$omega_jw)
})

test_that("draw_parameters_j saves theta_jw in the same form as the informative
sampler", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1
  gibbs_sampler <- new_gibbs_spec(6, 0.5, 1, 1.1)

  result <- withr::with_seed(7, draw_parameters_j(
    y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
    jx, gibbs_sampler
  ))
  result_informative <- withr::with_seed(7, draw_parameters_j_informative(
    y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
    jx, gibbs_sampler, list(list())
  ))

  # The full vectorized Theta_j: 10 exogenous rows times 2 columns, with the
  # betas restricted to zero (rows 4 to 10 of the first column) as zeros
  theta_jw <- result$theta_jw[[1]]
  expect_identical(dim(theta_jw), dim(result_informative$theta_jw[[1]]))
  expect_identical(dim(theta_jw), c(20L, 1L))
  expect_true(all(theta_jw[4:10] == 0))
  expect_equal(theta_jw[1:3], result$beta_jw[[1]])
})

test_that("draw_parameters returns correct parameters for equation 1", {
  skip_on_cran()
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
  skip_on_cran()
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

test_that("draw_parameters_j keeps every nstore-th draw after burn-in", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1

  run_sampler <- function(nstore) {
    withr::with_seed(
      7,
      draw_parameters_j(
        y_matrix,
        x_matrix,
        character_gamma_matrix,
        character_beta_matrix,
        jx,
        set_gibbs_spec(ndraws = 25, burnin_ratio = 0.2, nstore = nstore)
      )
    )
  }

  unthinned <- run_sampler(1)
  thinned <- run_sampler(3)

  # burnin = 5, nsave = floor(20 / 3) = 6
  expect_length(unthinned$beta_jw, 20)
  expect_length(thinned$beta_jw, 6)
  expect_length(thinned$gamma_jw, 6)
  expect_length(thinned$omega_tilde_jw, 6)
  expect_identical(thinned$beta_jw, unthinned$beta_jw[seq(3, 18, by = 3)])
  expect_identical(thinned$gamma_jw, unthinned$gamma_jw[seq(3, 18, by = 3)])
})

test_that("draw_parameters_j gives the same draws for ts and plain matrices", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  expect_s3_class(y_matrix, "ts")
  plain <- function(x) matrix(as.numeric(x), nrow(x), dimnames = dimnames(x))

  run_sampler <- function(y_matrix, x_matrix) {
    withr::with_seed(
      7,
      draw_parameters_j(
        y_matrix,
        x_matrix,
        simulated_data$character_gamma_matrix,
        simulated_data$character_beta_matrix,
        1,
        set_gibbs_spec(ndraws = 20)
      )
    )
  }

  expect_equal(
    run_sampler(y_matrix, x_matrix),
    run_sampler(plain(y_matrix), plain(x_matrix))
  )
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
    jx,
    crossprod(x_matrix),
    xbtxb_for(x_matrix, character_beta_matrix, jx),
    equation_data = construct_equation_data(
      y_matrix, x_matrix, character_gamma_matrix,
      character_beta_matrix, jx
    )
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
  jx <- 3

  result_without_endogenous <- initialize_sampler(
    y_matrix,
    x_matrix,
    character_gamma_matrix,
    character_beta_matrix,
    jx,
    equation_data = construct_equation_data(
      y_matrix, x_matrix, character_gamma_matrix,
      character_beta_matrix, jx
    )
  )
  expect_identical(result_without_endogenous$gamma_parameters_j, 0)
  expect_identical(result_without_endogenous$cholesky_of_inverse_hessian, NA)
})

test_that("draw_parameters_j explains why the sampler cannot start", {
  # A constant endogenous regressor leaves the target flat in gamma, so the
  # Hessian at the optimum is zero. This used to fail inside solve() with
  # "Lapack routine dgesv: system is exactly singular".
  x_matrix <- cbind(1, seq_len(20))
  y_matrix <- cbind(rep(c(-1, 1), 10) + seq_len(20), rep(3, 20))
  character_gamma_matrix <- matrix(c("1", "-gamma1_2", "0", "1"), 2)
  character_beta_matrix <- matrix(
    c("constant1", "beta1_2", "constant2", "beta2_2"), 2
  )

  expect_error(
    draw_parameters_j(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
      1, new_gibbs_spec(6, 0.5, 1, 1.1)
    ),
    "cannot be started"
  )
})

test_that("construct_cholesky_of_inverse_hessian needs a positive definite
Hessian", {
  hessian <- matrix(c(4, 1, 1, 2), 2)
  expect_equal(
    construct_cholesky_of_inverse_hessian(hessian),
    t(chol(solve(hessian)))
  )
  # not at a maximum in one direction
  expect_error(
    construct_cholesky_of_inverse_hessian(matrix(c(4, 0, 0, -1), 2)),
    "cannot be started"
  )
  expect_error(
    construct_cholesky_of_inverse_hessian(matrix(NaN)),
    "cannot be started"
  )
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
    cholesky_of_inverse_hessian,
    crossprod(x_matrix),
    xbtxb_for(x_matrix, character_beta_matrix, jx),
    equation_data = construct_equation_data(
      y_matrix, x_matrix, character_gamma_matrix,
      character_beta_matrix, jx
    )
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
    cholesky_of_inverse_hessian,
    crossprod(x_matrix),
    xbtxb_for(x_matrix, character_beta_matrix, jx),
    equation_data = construct_equation_data(
      y_matrix, x_matrix, character_gamma_matrix,
      character_beta_matrix, jx
    )
  )
  expect_equal(
    new_gamma_parameters_1,
    gamma_parameters_1
  )
})

test_that("draw_gamma_j handles a target that is not a number", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 1
  gamma_parameters_1 <- structure(-0.34996818653039, dim = c(1L, 1L))
  cholesky_of_inverse_hessian <- structure(0.175744390195533, dim = c(1L, 1L))

  draw <- function() {
    withr::with_seed(
      7,
      draw_gamma_j(
        y_matrix,
        x_matrix,
        character_gamma_matrix,
        character_beta_matrix,
        jx,
        gamma_parameters_1,
        tau = 1.1,
        cholesky_of_inverse_hessian,
        crossprod(x_matrix),
        xbtxb_for(x_matrix, character_beta_matrix, jx),
        equation_data = construct_equation_data(
          y_matrix, x_matrix, character_gamma_matrix,
          character_beta_matrix, jx
        )
      )
    )
  }
  is_current <- function(gamma) isTRUE(all.equal(gamma, gamma_parameters_1))

  # real data does not produce this, so the target is replaced
  # at the candidate only: the candidate is rejected
  testthat::local_mocked_bindings(
    target_j = function(..., gamma_parameters_j) {
      if (is_current(gamma_parameters_j)) 100 else NaN
    }
  )
  expect_equal(draw(), gamma_parameters_1)

  # at the current value: the chain could never move, so stop
  testthat::local_mocked_bindings(target_j = function(...) NaN)
  expect_error(draw(), "not finite at the current")
  testthat::local_mocked_bindings(target_j = function(...) Inf)
  expect_error(draw(), "not finite at the current")
})

test_that("draw_gamma_j returns 0 when there are no endogenous variables", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 3
  tau <- 1.1
  gamma_parameters_3 <- 0
  cholesky_of_inverse_hessian <- NA

  result <- draw_gamma_j(
    y_matrix,
    x_matrix,
    character_gamma_matrix,
    character_beta_matrix,
    jx,
    gamma_parameters_3,
    tau,
    cholesky_of_inverse_hessian,
    equation_data = construct_equation_data(
      y_matrix, x_matrix, character_gamma_matrix,
      character_beta_matrix, jx
    )
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
      character_beta_matrix, jx, gamma_parameters_1,
      crossprod(x_matrix),
      xbtxb_for(x_matrix, character_beta_matrix, jx),
      equation_data = construct_equation_data(
        y_matrix, x_matrix, character_gamma_matrix,
        character_beta_matrix, jx
      )
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

  theta_permutation <- construct_theta_permutation(
    character_beta_matrix, jx, nrow(character_beta_matrix) * 2
  )

  result <- withr::with_seed(
    7,
    draw_theta_j(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
      jx, gamma_parameters_j, omega_tilde_jw, crossprod(x_matrix),
      theta_permutation = theta_permutation,
      equation_data = construct_equation_data(
        y_matrix, x_matrix, character_gamma_matrix,
        character_beta_matrix, jx
      )
    )
  )

  # theta_jw is the full vectorized Theta_j, with the betas restricted to
  # zero (rows 4 to 10 of the first column) included as zeros
  expected_result_theta_jw <- matrix(c(
    1.33785213690464, 0.553200107134962, 0.132042350013386,
    rep(0, 7),
    0.395058533772247, 0.0262040643823067,
    -0.127070095171351, -0.017387429595745, -0.150109902894841,
    -0.100230761546029, 0.0219065823268252, -0.0852778937502962,
    -0.24161797439946, -0.047251981429159
  ), ncol = 1)

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

  theta_permutation <- construct_theta_permutation(
    character_beta_matrix, jx, nrow(character_beta_matrix)
  )

  result <- withr::with_seed(
    7,
    draw_theta_j(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
      jx, gamma_parameters_j, omega_tilde_jw, crossprod(x_matrix),
      theta_permutation = theta_permutation,
      equation_data = construct_equation_data(
        y_matrix, x_matrix, character_gamma_matrix,
        character_beta_matrix, jx
      )
    )
  )

  expected_result_beta_jw <- c(
    1.62903143338742, -0.577349603700648, 0.501512145598412
  )

  # theta_jw is the full vectorized Theta_j: the free betas sit at their
  # rows (constant, current_account.L(1), world_gdp), the rest are zero
  expected_result_theta_jw <- matrix(0, 10, 1)
  expected_result_theta_jw[c(1, 5, 9)] <- expected_result_beta_jw

  expect_equal(result$theta_jw, expected_result_theta_jw)
  expect_equal(result$beta_jw, expected_result_beta_jw)
})

test_that("draw_theta_j stops on a permutation of the wrong length", {
  y_matrix <- simulated_data$y_matrix
  x_matrix <- simulated_data$x_matrix
  character_gamma_matrix <- simulated_data$character_gamma_matrix
  character_beta_matrix <- simulated_data$character_beta_matrix
  jx <- 3
  # one parameter too many
  theta_permutation <- construct_theta_permutation(
    character_beta_matrix, jx, nrow(character_beta_matrix) + 1
  )

  expect_error(
    draw_theta_j(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
      jx, 0, matrix(0.08), crossprod(x_matrix),
      theta_permutation = theta_permutation,
      equation_data = construct_equation_data(
        y_matrix, x_matrix, character_gamma_matrix,
        character_beta_matrix, jx
      )
    ),
    "permutation"
  )
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
    parameters,
    crossprod(x_matrix),
    xbtxb_for(x_matrix, character_beta_matrix, jx),
    equation_data = construct_equation_data(
      y_matrix, x_matrix, character_gamma_matrix,
      character_beta_matrix, jx
    )
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
    new_parameters,
    crossprod(x_matrix),
    xbtxb_for(x_matrix, character_beta_matrix, jx),
    equation_data = construct_equation_data(
      y_matrix, x_matrix, character_gamma_matrix,
      character_beta_matrix, jx
    )
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
      parameters,
      crossprod(x_matrix),
      xbtxb_for(x_matrix, character_beta_matrix, jx),
      equation_data = construct_equation_data(
        y_matrix, x_matrix, character_gamma_matrix,
        character_beta_matrix, jx
      )
    ),
    "number of gamma parameters"
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
  jx <- 3

  parameters <- NA

  expect_warning(
    result <- target_j(
      y_matrix,
      x_matrix,
      character_gamma_matrix,
      character_beta_matrix,
      jx,
      parameters,
      equation_data = construct_equation_data(
        y_matrix, x_matrix, character_gamma_matrix,
        character_beta_matrix, jx
      )
    ),
    "Equation 3 does not contain any gamma parameters. Returning NA.",
    fixed = TRUE
  )

  expect_true(is.na(result))
})
