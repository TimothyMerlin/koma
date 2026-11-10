reduced_form_reference <- function(companion) {
  gamma_inv <- solve(companion$gamma_matrix)
  times_inv <- function(m) if (is.null(m)) NULL else m %*% gamma_inv
  list(
    companion_pi = times_inv(companion$beta_tilde_matrix),
    companion_theta = times_inv(companion$phi_matrix),
    companion_d = times_inv(companion$c_matrix)
  )
}

random_posterior <- function(n, p, exogenous) {
  set.seed(1)
  gamma_matrix <- diag(n) + matrix(stats::rnorm(n * n, sd = 0.2), n, n)
  phi_matrix <- lapply(seq_len(p), function(l) {
    matrix(stats::rnorm(n * n, sd = 0.2), n, n)
  })
  beta_matrix <- matrix(
    stats::rnorm((1 + length(exogenous)) * n), 1 + length(exogenous), n,
    dimnames = list(c("constant", exogenous), NULL)
  )
  list(gamma_matrix = gamma_matrix, phi_matrix = phi_matrix,
       beta_matrix = beta_matrix)
}

test_that("construct_reduced_form equals the inverse of the companion Gamma", {
  posterior <- random_posterior(n = 4, p = 3, exogenous = c("x1", "x2"))
  companion <- construct_companion_matrix(posterior, c("x1", "x2"))

  expect_equal(
    construct_reduced_form(companion),
    reduced_form_reference(companion)
  )
})

test_that("construct_reduced_form without lagged endogenous variables", {
  posterior <- random_posterior(n = 3, p = 0, exogenous = "x1")
  companion <- construct_companion_matrix(posterior, "x1")

  expect_equal(
    construct_reduced_form(companion),
    reduced_form_reference(companion)
  )
})

test_that("construct_reduced_form with one lag and no exogenous variables", {
  posterior <- random_posterior(n = 3, p = 1, exogenous = character())
  companion <- construct_companion_matrix(posterior, character())

  expect_equal(
    construct_reduced_form(companion),
    reduced_form_reference(companion)
  )
})
