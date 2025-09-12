test_that("multivariate_norm fails for incompatible arguments", {
  mu <- c(0, 0)
  # Wrong shape for covariance matrix
  sigma <- matrix(c(10, 3, 3, 2, 4, 7), nrow = 2, ncol = 3)
  expect_error(
    multivariate_norm(n = 1, mu = mu, sigma = sigma),
    "incompatible arguments"
  )
})

test_that("multivariate_norm fails for non-positive-definite Sigma", {
  mu <- c(0, 0)
  # Not a positive-definite matrix
  sigma <- matrix(c(-1, 0, 0, -1), 2, 2)
  expect_error(
    multivariate_norm(n = 1, mu = mu, sigma = sigma),
    "'Sigma' is not positive definite"
  )
})

test_that("multivariate_norm returns correct dimensions for n > 1", {
  mu <- c(0, 0)
  sigma <- matrix(c(10, 3, 3, 2), 2, 2)
  out <- multivariate_norm(n = 10, mu = mu, sigma = sigma)
  expect_equal(dim(out), c(10, 2)) # Output shape should be 10 x 2
})

test_that("multivariate_norm returns values", {
  mu <- c(0, 2)
  sigma <- matrix(c(10, 3, 3, 2), 2, 2)
  # Test when empirical = FALSE
  expect_silent(
    multivariate_norm(n = 100, mu = mu, sigma = sigma, empirical = FALSE)
  )
  # Test when empirical = TRUE
  expect_silent(
    multivariate_norm(n = 100, mu = mu, sigma = sigma, empirical = TRUE)
  )
})

test_that("multivariate_norm returns correct values with set seed for a
specific covariance matrix", {
  mu <- c(0, 2, 3)
  # Define matrix L
  l <- matrix(c(
    0.1345123871289, 0.2231, 0.47572483740298374, 0, 0.3,
    0.5545389472043, 0, 0, 0.6
  ), nrow = 3, byrow = TRUE)
  l_t <- t(l)
  # Calculate product of L and L_t to get positive definite matrix
  sigma <- l %*% l_t

  expected_result <- c(-1.16620342106838, 0.755026779388611, 1.47164315861421)
  expect_equal(
    withr::with_seed(7, multivariate_norm(n = 1, mu = mu, sigma = sigma)),
    expected_result
  )
})

test_that("multivariate_norm returns correct values with set seed when
covariance matrix is the identity matrix", {
  mu <-
    structure(c(
      1.28292792437263, 0.494181488603918, 0.200331967377298,
      -0.618483036013203, 0.0509572243812283, -0.0248410589515807,
      0.00773231403516218, -0.0316109950612034, -0.0431545778697347,
      -0.165377434290823, -0.0471056861741089, -0.222717428491224,
      -0.0788314846869081
    ), dim = c(13L, 1L))
  sigma <- diag(13)

  expected_result <- c(
    2.01400145611189, -1.03387691861028, 0.227131414207839, -1.84699247163267,
    -1.56723485025481, 0.65987535959536, -0.527749970992126, -0.122590610279912,
    -0.130381947729156, -0.58103052065925, 2.80178284943234, 0.0332937088063138,
    1.02251935423244
  )
  expect_equal(
    withr::with_seed(327, multivariate_norm(n = 1, mu = mu, sigma = sigma)),
    expected_result
  )
})

test_that("multivariate_norm_pdf", {
  sigma <- matrix(c(10, 3, 3, 2), 2, 2)
  mu <- c(0, 0)
  x <- c(1, 1)
  result <- multivariate_norm_pdf(x, mu, sigma)

  expected_result <- 0.0365325375583396

  expect_equal(result, expected_result)
})
