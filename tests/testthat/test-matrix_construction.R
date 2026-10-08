test_that("construct_equation_data preserves subsets and dimensions", {
  y_matrix <- matrix(seq_len(24), 8, 3)
  x_matrix <- matrix(seq_len(32), 8, 4)
  gamma <- matrix("0", 3, 3)
  diag(gamma) <- "1"
  gamma[2, 1] <- "-gamma1_2"
  gamma[c(1, 3), 2] <- c("-gamma2_1", "-gamma2_3")
  beta <- matrix("0", 4, 3)
  beta[c(1, 3), 1] <- c("constant1", "beta1_3")
  beta[, 2] <- paste0("beta2_", seq_len(4))
  beta[2, 3] <- "beta3_2"

  for (jx in seq_len(3)) {
    result <- construct_equation_data(y_matrix, x_matrix, gamma, beta, jx)
    gamma_positions <- grep("gamma", gamma[, jx])
    beta_positions <- grep("^0", beta[, jx], invert = TRUE)
    expected_y <- if (length(gamma_positions)) y_matrix[, gamma_positions] else NA

    expect_identical(result$y_matrix_j, expected_y)
    expect_identical(result$x_b, x_matrix[, beta_positions, drop = FALSE])
    expect_identical(result$beta_positions, beta_positions)
    expect_identical(result$gamma_count, length(gamma_positions))
    expect_identical(result$number_of_observations, nrow(y_matrix))
    expect_identical(result$number_of_exogenous, nrow(beta))
  }
})

test_that("construct_theta_bar_j uses precomputed prior precision", {
  x_matrix <- cbind(1, c(-2, -1, 0, 1, 2))
  z_matrix_j <- cbind(c(1, 3, 2, 5, 4), c(2, 1, 4, 3, 6))
  theta_vcv <- diag(c(2, 3, 4, 5))
  theta_vcv[1, 2] <- theta_vcv[2, 1] <- 0.5
  theta_mean <- matrix(c(0.2, -0.3, 0.4, 0.1), ncol = 1)
  theta_precision <- solve(theta_vcv)
  priors_j <- list(
    theta_precision = theta_precision,
    theta_precision_mean = theta_precision %*% theta_mean
  )
  xtx <- crossprod(x_matrix)

  for (omega_tilde_jw in list(diag(2), matrix(c(2, 0.3, 0.3, 1), 2))) {
    likelihood_precision <- kronecker(solve(omega_tilde_jw), xtx)
    expected_vcv <- solve(likelihood_precision + solve(theta_vcv))
    theta_hat <- c(solve(xtx, crossprod(x_matrix, z_matrix_j)))
    expected_mean <- expected_vcv %*% (
      likelihood_precision %*% theta_hat + solve(theta_vcv) %*% theta_mean
    )

    result <- construct_theta_bar_j(
      x_matrix, z_matrix_j, priors_j, omega_tilde_jw, xtx
    )

    expect_equal(result$xi_bar, expected_vcv)
    expect_equal(result$theta_bar, expected_mean)
  }
})

test_that("construct_y_matrix_j returns the correct subset of y_matrix", {
  y_matrix <- matrix(c(1:24), ncol = 3, dimnames = list(c(), c("a", "b", "c")))
  character_gamma_matrix <- matrix(
    c(1, 0, 0, 0, 1, "-theta_32", "-gamma_13", 0, 1),
    ncol = 3, dimnames = list(c(), c("a", "b", "c")), byrow = TRUE
  )
  jx <- 1

  result <- construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)
  expected_output <- c(17:24)

  # Compare the result with the expected output
  expect_identical(result, expected_output)
})

test_that("construct_y_matrix_j returns NA when there are no gamma
parameters", {
  y_matrix <- matrix(c(1:24), ncol = 3, dimnames = list(c(), c("a", "b", "c")))
  character_gamma_matrix <- matrix(
    c(1, 0, 0, 0, 1, "-theta_32", "-gamma_13", 0, 1),
    ncol = 3, dimnames = list(c(), c("a", "b", "c")), byrow = TRUE
  )
  jx <- 2

  expect_warning(construct_y_matrix_j(y_matrix, character_gamma_matrix, jx))
  result <- suppressWarnings(
    construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)
  )

  expect_true(is.na(result))
})

test_that("construct_z_matrix_j returns the correct Z_j matrix for the one
endogenous variable case", {
  gamma_parameters_j <- 0.5
  y_matrix <- matrix(c(1:24), ncol = 3, dimnames = list(c(), c("a", "b", "c")))
  y_matrix_j <- c(17:24)
  jx <- 1

  result <- construct_z_matrix_j(
    gamma_parameters_j, y_matrix, y_matrix_j, jx
  )

  expected_output <- structure(c(
    -7.5, -7, -6.5, -6, -5.5, -5, -4.5, -4, 17, 18, 19,
    20, 21, 22, 23, 24
  ), dim = c(8L, 2L), dimnames = list(NULL, c(
    "",
    "y_matrix_j"
  )))

  expect_identical(result, expected_output)
})

test_that("construct_z_matrix_j finishes in error when arguments contain NAs", {
  # Function should finish in error if there are no endogenous variables
  # in equation jx
  gamma_parameters_j <- NA
  y_matrix <- matrix(c(1:24), ncol = 3, dimnames = list(c(), c("a", "b", "c")))
  y_matrix_j <- NA
  jx <- 2

  expect_error(construct_z_matrix_j(
    gamma_parameters_j, y_matrix, y_matrix_j, jx
  ), "y_matrix_j cannot contain NAs.")

  y_matrix_j <- c(17:24)
  expect_error(construct_z_matrix_j(
    gamma_parameters_j, y_matrix, y_matrix_j, jx
  ), "gamma_parameters_j cannot contain NAs.")
})

test_that("construct_beta_hat_j_matrix computes beta_hat_j correctly", {
  x_matrix <- matrix(c(1:24), ncol = 3, dimnames = list(c(), c("a", "b", "c")))
  z_matrix_j <- matrix(
    c(-7.5, -7, -6.5, -6, -5.5, -5, -4.5, -4, 17, 18, 19, 20, 21, 22, 23, 24),
    nrow = 8, ncol = 2, dimnames = list(NULL, c("y_j-Y_j*gamma_j", "Y_j"))
  )
  character_beta_matrix <- matrix(c("beta_11", 0, 0, "beta_12", 0, 0, 0, 0, 0),
    ncol = 3, byrow = TRUE
  )
  jx <- 1

  result <- construct_beta_hat_j_matrix(
    x_matrix, z_matrix_j, character_beta_matrix, jx,
    xbtxb_for(x_matrix, character_beta_matrix, jx),
    equation_data = construct_equation_data(
      z_matrix_j, x_matrix, matrix("0", 1, ncol(character_beta_matrix)),
      character_beta_matrix, jx
    )
  )

  expected_output <- matrix(c(1.5, -1, 0), nrow = 3)
  expect_equal(result, expected_output)
})

test_that("construct_beta_hat_j_matrix returns zeros when no betas are free", {
  x_matrix <- cbind(1, seq_len(8))
  y_matrix <- cbind(seq_len(8), seq_len(8)^2)
  character_beta_matrix <- matrix("0", nrow = 2, ncol = 2)
  character_gamma_matrix <- matrix(c("1", "gamma_21", "0", "1"), 2)
  equation_data <- construct_equation_data(
    y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix, 1
  )
  z_matrix_j <- construct_z_matrix_j(
    0.5, y_matrix, equation_data$y_matrix_j, 1
  )

  expect_equal(
    construct_beta_hat_j_matrix(
      x_matrix, z_matrix_j, character_beta_matrix, 1,
      crossprod(equation_data$x_b), equation_data
    ),
    matrix(0, nrow = 2, ncol = 1)
  )
})

test_that("construct_pi_hat_0 correctly computes pi_hat_0", {
  set.seed(7)
  x_matrix <- matrix(stats::rnorm(24),
    ncol = 3,
    dimnames = list(c(), c("a", "b", "c"))
  )
  z_matrix_j <- matrix(
    stats::rnorm(16),
    nrow = 8, ncol = 2, dimnames = list(NULL, c("y_j-Y_j*gamma_j", "Y_j"))
  )

  result <- construct_pi_hat_0(x_matrix, z_matrix_j, crossprod(x_matrix))

  expected_output <- matrix(
    c(0.0640975604962179, 0.0718475273146385, -0.21740793920036),
    nrow = 3, ncol = 1, dimnames = list(c("a", "b", "c"), NULL)
  )
  expect_equal(result, expected_output)
})

test_that("construct_theta_hat_j correctly computes theta_hat for one
endogenous variable case", {
  set.seed(7)
  x_matrix <- matrix(stats::rnorm(24),
    ncol = 3,
    dimnames = list(c(), c("a", "b", "c"))
  )
  z_matrix_j <- matrix(
    stats::rnorm(16),
    nrow = 8, ncol = 2, dimnames = list(NULL, c("y_j-Y_j*gamma_j", "Y_j"))
  )

  result <- construct_theta_hat_j(x_matrix, z_matrix_j, crossprod(x_matrix))

  expected_output <- matrix(
    c(
      0.16509440554976, 0.156197287926606, -0.638519570858842,
      0.0640975604962179, 0.0718475273146385, -0.21740793920036
    ),
    nrow = 3, ncol = 2, dimnames = list(
      c("a", "b", "c"),
      c("y_j-Y_j*gamma_j", "Y_j")
    )
  )
  expect_equal(result, expected_output)
})

# The permutation matrix P that moves the zero restrictions to the end, built
# element by element. construct_theta_permutation() must give the same order.
permutation_matrix_for <- function(character_beta_matrix, jx,
                                   number_of_parameters) {
  number_of_exogenous <- nrow(character_beta_matrix)
  permutation_matrix <- matrix(0, number_of_parameters, number_of_parameters)

  fpos <- grep("^0", character_beta_matrix[, jx], invert = TRUE)
  for (ix in seq_along(fpos)) {
    permutation_matrix[ix, fpos[ix]] <- 1
  }

  fposend <- grep("\\b0\\b", character_beta_matrix[, jx])
  seperate_blocks_at <- number_of_parameters - length(fposend)
  for (ix in seq_along(fposend)) {
    permutation_matrix[seperate_blocks_at + ix, fposend[ix]] <- 1
  }

  if (number_of_parameters > number_of_exogenous) {
    permutation_matrix[
      (length(fpos) + 1):seperate_blocks_at,
      (number_of_exogenous + 1):number_of_parameters
    ] <- diag(number_of_parameters - number_of_exogenous)
  }
  permutation_matrix
}

test_that("construct_theta_permutation matches the permutation matrix", {
  character_beta_matrix <- simulated_data$character_beta_matrix
  number_of_exogenous <- nrow(character_beta_matrix)

  # equation 1 has one endogenous regressor, equation 3 has none
  cases <- list(
    list(jx = 1, number_of_parameters = 2 * number_of_exogenous),
    list(jx = 3, number_of_parameters = number_of_exogenous)
  )
  for (case in cases) {
    result <- construct_theta_permutation(
      character_beta_matrix, case$jx, case$number_of_parameters
    )
    permutation <- result$permutation
    permutation_matrix <- permutation_matrix_for(
      character_beta_matrix, case$jx, case$number_of_parameters
    )

    theta <- withr::with_seed(7, stats::rnorm(case$number_of_parameters))
    xi <- crossprod(withr::with_seed(
      8,
      matrix(
        stats::rnorm(case$number_of_parameters^2),
        case$number_of_parameters
      )
    ))

    # P theta and P Xi P'
    expect_equal(theta[permutation], c(permutation_matrix %*% theta))
    expect_equal(
      xi[permutation, permutation],
      permutation_matrix %*% xi %*% t(permutation_matrix)
    )

    # P' theta permutes back
    theta_back <- numeric(case$number_of_parameters)
    theta_back[permutation] <- theta
    expect_equal(theta_back, c(t(permutation_matrix) %*% theta))

    # the zero restrictions are the last block
    n_zero <- sum(character_beta_matrix[, case$jx] == "0")
    expect_equal(
      result$seperate_blocks_at, case$number_of_parameters - n_zero
    )
    expect_equal(
      tail(permutation, n_zero),
      unname(which(character_beta_matrix[, case$jx] == "0"))
    )
  }
})
