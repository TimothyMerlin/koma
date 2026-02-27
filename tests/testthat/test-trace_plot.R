test_that("trace_plot includes expected coefficient labels", {
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

  fig <- trace_plot(obj, variables = "y", params = c("beta", "gamma", "sigma"))
  expect_s3_class(fig, "ggplot")
  expect_true(all(c("draw", "value", "label") %in% names(fig$data)))
  expect_setequal(
    unique(fig$data$label),
    c("y:beta:x", "y:gamma:y", "y:sigma:omega")
  )
})

test_that("trace_plot filters parameter groups", {
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

  fig <- trace_plot(obj, variables = "y", params = "beta")
  expect_s3_class(fig, "ggplot")
  expect_setequal(unique(fig$data$label), "y:beta:x")
})

test_that("trace_plot validates inputs", {
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

  expect_error(trace_plot(obj, variables = 1), "`variables`")
  expect_error(trace_plot(obj, params = 1), "`params`")
  expect_error(trace_plot(obj, thin = "a"), "`thin`")
  expect_error(trace_plot(obj, thin = 0), "`thin`")
  expect_error(trace_plot(obj, max_draws = "a"), "`max_draws`")
  expect_error(trace_plot(obj, max_draws = 0), "`max_draws`")
  expect_error(trace_plot(obj, facet_ncol = 0), "`facet_ncol`")
  expect_error(trace_plot(obj, scales = "bad"), "`scales`")
  expect_error(trace_plot(obj, interactive = "yes"), "`interactive`")
})

test_that("trace_plot supports interactive output", {
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

  fig <- trace_plot(obj, variables = "y", params = c("beta", "gamma"), interactive = TRUE)
  expect_s3_class(fig, "plotly")
})
