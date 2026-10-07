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
    xbtxb_for(x_matrix, character_beta_matrix, jx)
  )

  expected_output <- matrix(c(1.5, -1, 0), nrow = 3)
  expect_equal(result, expected_output)
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
