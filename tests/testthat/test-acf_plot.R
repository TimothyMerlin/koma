test_that("acf_plot includes expected coefficient labels", {
  skip_if_not_installed("ggplot2")
  set.seed(123)

  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix("beta", nrow = 1, ncol = 1),
    character_gamma_matrix = matrix("gamma", nrow = 1, ncol = 1)
  )

  beta_draws <- replicate(120, rnorm(1, 0.2, 0.1), simplify = FALSE)
  gamma_draws <- replicate(120, rnorm(1, -0.1, 0.1), simplify = FALSE)
  omega_draws <- replicate(120, matrix(abs(rnorm(1, 0.3, 0.05)), 1, 1), simplify = FALSE)

  estimates <- list(
    y = list(
      beta_jw = beta_draws,
      gamma_jw = gamma_draws,
      omega_tilde_jw = omega_draws
    )
  )

  obj <- structure(
    list(estimates = estimates, sys_eq = sys_eq),
    class = "koma_estimate"
  )

  fig <- acf_plot(obj, variables = "y", params = c("beta", "gamma", "sigma"))
  expect_s3_class(fig, "ggplot")
  expect_true(all(c("lag", "acf", "label") %in% names(fig$data)))
  expect_setequal(
    unique(fig$data$label),
    c("y:beta:x", "y:gamma:y", "y:sigma:omega")
  )
})

test_that("acf_plot filters parameter groups", {
  skip_if_not_installed("ggplot2")
  set.seed(321)

  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix("beta", nrow = 1, ncol = 1),
    character_gamma_matrix = matrix("gamma", nrow = 1, ncol = 1)
  )

  beta_draws <- replicate(50, rnorm(1, 0.2, 0.1), simplify = FALSE)
  gamma_draws <- replicate(50, rnorm(1, -0.1, 0.1), simplify = FALSE)
  omega_draws <- replicate(50, matrix(abs(rnorm(1, 0.3, 0.05)), 1, 1), simplify = FALSE)

  estimates <- list(
    y = list(
      beta_jw = beta_draws,
      gamma_jw = gamma_draws,
      omega_tilde_jw = omega_draws
    )
  )

  obj <- structure(
    list(estimates = estimates, sys_eq = sys_eq),
    class = "koma_estimate"
  )

  fig <- acf_plot(obj, variables = "y", params = "beta")
  expect_s3_class(fig, "ggplot")
  expect_setequal(unique(fig$data$label), "y:beta:x")
})

test_that("acf_plot validates inputs", {
  skip_if_not_installed("ggplot2")
  set.seed(999)

  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix("beta", nrow = 1, ncol = 1),
    character_gamma_matrix = matrix("gamma", nrow = 1, ncol = 1)
  )

  beta_draws <- replicate(10, rnorm(1, 0.2, 0.1), simplify = FALSE)
  gamma_draws <- replicate(10, rnorm(1, -0.1, 0.1), simplify = FALSE)
  omega_draws <- replicate(10, matrix(abs(rnorm(1, 0.3, 0.05)), 1, 1), simplify = FALSE)

  estimates <- list(
    y = list(
      beta_jw = beta_draws,
      gamma_jw = gamma_draws,
      omega_tilde_jw = omega_draws
    )
  )

  obj <- structure(
    list(estimates = estimates, sys_eq = sys_eq),
    class = "koma_estimate"
  )

  expect_error(acf_plot(obj, variables = 1), "`variables`")
  expect_error(acf_plot(obj, params = 1), "`params`")
  expect_error(acf_plot(obj, thin = "a"), "`thin`")
  expect_error(acf_plot(obj, thin = 0), "`thin`")
  expect_error(acf_plot(obj, max_draws = "a"), "`max_draws`")
  expect_error(acf_plot(obj, max_draws = 0), "`max_draws`")
  expect_error(acf_plot(obj, max_lag = "a"), "`max_lag`")
  expect_error(acf_plot(obj, max_lag = 0), "`max_lag`")
  expect_error(acf_plot(obj, conf_level = "a"), "`conf_level`")
  expect_error(acf_plot(obj, conf_level = 0), "`conf_level`")
  expect_error(acf_plot(obj, conf_level = 1), "`conf_level`")
  expect_error(acf_plot(obj, facet_ncol = 0), "`facet_ncol`")
  expect_error(acf_plot(obj, scales = "bad"), "`scales`")
  expect_error(acf_plot(obj, interactive = "yes"), "`interactive`")
})

test_that("acf_plot uses conf_level for significance bands", {
  skip_if_not_installed("ggplot2")

  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix("beta", nrow = 1, ncol = 1),
    character_gamma_matrix = matrix(0, nrow = 1, ncol = 1)
  )

  estimates <- list(
    y = list(
      beta_jw = as.list(seq_len(20)),
      gamma_jw = list(),
      omega_tilde_jw = list()
    )
  )

  obj <- structure(
    list(estimates = estimates, sys_eq = sys_eq),
    class = "koma_estimate"
  )

  fig <- acf_plot(obj, variables = "y", params = "beta", conf_level = 0.8)
  expect_s3_class(fig, "ggplot")

  expected <- stats::qnorm((1 + 0.8) / 2) / sqrt(20)
  observed <- unique(fig$data$confint)
  expect_length(observed, 1)
  expect_equal(observed, expected, tolerance = 1e-10)
})

test_that("acf_plot supports interactive output", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  set.seed(456)

  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix("beta", nrow = 1, ncol = 1),
    character_gamma_matrix = matrix("gamma", nrow = 1, ncol = 1)
  )

  beta_draws <- replicate(40, rnorm(1, 0.2, 0.1), simplify = FALSE)
  gamma_draws <- replicate(40, rnorm(1, -0.1, 0.1), simplify = FALSE)

  estimates <- list(
    y = list(
      beta_jw = beta_draws,
      gamma_jw = gamma_draws,
      omega_tilde_jw = list()
    )
  )

  obj <- structure(
    list(estimates = estimates, sys_eq = sys_eq),
    class = "koma_estimate"
  )

  fig <- acf_plot(obj, variables = "y", params = c("beta", "gamma"), interactive = TRUE)
  expect_s3_class(fig, "plotly")
})
