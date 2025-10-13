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
    x_matrix, z_matrix_j, character_beta_matrix, jx
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

  result <- construct_pi_hat_0(x_matrix, z_matrix_j)

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

  result <- construct_theta_hat_j(x_matrix, z_matrix_j)

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

test_that("matrix powers are computed correctly", {
  mat <- matrix(c(1, 2, 3, 4), 2, 2)
  horizon <- 5

  # Using custom function
  mat_powers <- powers(mat, horizon)

  # Compare with explicit multiplication (manual check)
  expect_equal(mat_powers[[1]], diag(2)) # mat^0
  expect_equal(mat_powers[[2]], mat) # mat^1
  expect_equal(mat_powers[[3]], mat %*% mat) # mat^2
  expect_equal(mat_powers[[4]], mat %*% mat %*% mat) # mat^3
  expect_equal(mat_powers[[5]], mat %*% mat %*% mat %*% mat) # mat^4
})
