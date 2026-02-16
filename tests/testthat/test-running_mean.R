test_that("running_mean includes expected labels and columns", {
  set.seed(123)

  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix("beta", nrow = 1, ncol = 1),
    character_gamma_matrix = matrix("gamma", nrow = 1, ncol = 1)
  )

  beta_draws <- replicate(40, rnorm(1, 0.2, 0.1), simplify = FALSE)
  gamma_draws <- replicate(40, rnorm(1, -0.1, 0.1), simplify = FALSE)
  omega_draws <- replicate(40, matrix(abs(rnorm(1, 0.3, 0.05)), 1, 1), simplify = FALSE)

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

  out <- running_mean(obj, variables = "y", params = c("beta", "gamma", "sigma"))
  expect_true(all(c(
    "draw", "value", "label", "draw_position", "in_grace_window"
  ) %in% names(out)))
  expect_setequal(
    unique(out$label),
    c("y:beta:x", "y:gamma:y", "y:sigma:omega")
  )
})

test_that("running_mean computes recursive means", {
  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix("beta", nrow = 1, ncol = 1),
    character_gamma_matrix = matrix(0, nrow = 1, ncol = 1)
  )

  estimates <- list(
    y = list(
      beta_jw = list(1, 3, 5),
      gamma_jw = list(),
      omega_tilde_jw = list()
    )
  )

  obj <- structure(
    list(estimates = estimates, sys_eq = sys_eq),
    class = "koma_estimate"
  )

  out <- running_mean(obj, variables = "y", params = "beta")
  out <- out[order(out$draw), ]

  expect_equal(out$draw, 1:3)
  expect_equal(out$value, c(1, 2, 3))
})

test_that("running_mean computes full-chain running means before max_draws crop", {
  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix("beta", nrow = 1, ncol = 1),
    character_gamma_matrix = matrix(0, nrow = 1, ncol = 1)
  )

  estimates <- list(
    y = list(
      beta_jw = as.list(1:10),
      gamma_jw = list(),
      omega_tilde_jw = list()
    )
  )

  obj <- structure(
    list(estimates = estimates, sys_eq = sys_eq),
    class = "koma_estimate"
  )

  out <- running_mean(obj, variables = "y", params = "beta", max_draws = 3)
  out <- out[order(out$draw), ]

  expect_equal(out$draw, 8:10)
  expect_equal(out$value, c(4.5, 5, 5.5))
})

test_that("running_mean respects grace_draws", {
  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix("beta", nrow = 1, ncol = 1),
    character_gamma_matrix = matrix(0, nrow = 1, ncol = 1)
  )

  estimates <- list(
    y = list(
      beta_jw = as.list(1:5),
      gamma_jw = list(),
      omega_tilde_jw = list()
    )
  )

  obj <- structure(
    list(estimates = estimates, sys_eq = sys_eq),
    class = "koma_estimate"
  )

  out <- running_mean(obj, variables = "y", params = "beta", grace_draws = 2)
  out <- out[order(out$draw), ]

  expect_equal(out$draw_position, 1:5)
  expect_equal(out$in_grace_window, c(TRUE, TRUE, FALSE, FALSE, FALSE))
})

test_that("running_mean validates inputs", {
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

  expect_error(running_mean(obj, variables = 1), "`variables`")
  expect_error(running_mean(obj, params = 1), "`params`")
  expect_error(running_mean(obj, thin = "a"), "`thin`")
  expect_error(running_mean(obj, thin = 0), "`thin`")
  expect_error(running_mean(obj, max_draws = "a"), "`max_draws`")
  expect_error(running_mean(obj, max_draws = 0), "`max_draws`")
  expect_error(running_mean(obj, grace_draws = "a"), "`grace_draws`")
  expect_error(running_mean(obj, grace_draws = -1), "`grace_draws`")
})

test_that("running_mean_plot returns expected labels", {
  skip_if_not_installed("ggplot2")
  set.seed(123)

  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix("beta", nrow = 1, ncol = 1),
    character_gamma_matrix = matrix("gamma", nrow = 1, ncol = 1)
  )

  beta_draws <- replicate(40, rnorm(1, 0.2, 0.1), simplify = FALSE)
  gamma_draws <- replicate(40, rnorm(1, -0.1, 0.1), simplify = FALSE)
  omega_draws <- replicate(40, matrix(abs(rnorm(1, 0.3, 0.05)), 1, 1), simplify = FALSE)

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

  fig <- running_mean_plot(obj, variables = "y", params = c("beta", "gamma", "sigma"))
  expect_s3_class(fig, "ggplot")
  expect_setequal(
    unique(fig$data$label),
    c("y:beta:x", "y:gamma:y", "y:sigma:omega")
  )
})

test_that("running_mean_plot adds a grey grace band", {
  skip_if_not_installed("ggplot2")

  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix("beta", nrow = 1, ncol = 1),
    character_gamma_matrix = matrix(0, nrow = 1, ncol = 1)
  )

  estimates <- list(
    y = list(
      beta_jw = as.list(1:5),
      gamma_jw = list(),
      omega_tilde_jw = list()
    )
  )

  obj <- structure(
    list(estimates = estimates, sys_eq = sys_eq),
    class = "koma_estimate"
  )

  fig <- running_mean_plot(obj, variables = "y", params = "beta", grace_draws = 2)
  expect_match(class(fig$layers[[1]]$geom)[1], "GeomRect")
  expect_equal(fig$layers[[1]]$data$xmin, 1)
  expect_equal(fig$layers[[1]]$data$xmax, 2)
})

test_that("running_mean_plot validates plot-specific inputs", {
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

  expect_error(running_mean_plot(obj, facet_ncol = 0), "`facet_ncol`")
  expect_error(running_mean_plot(obj, scales = "bad"), "`scales`")
  expect_error(running_mean_plot(obj, interactive = "yes"), "`interactive`")
})

test_that("running_mean_plot supports interactive output", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  set.seed(456)

  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix("beta", nrow = 1, ncol = 1),
    character_gamma_matrix = matrix("gamma", nrow = 1, ncol = 1)
  )

  beta_draws <- replicate(20, rnorm(1, 0.2, 0.1), simplify = FALSE)
  gamma_draws <- replicate(20, rnorm(1, -0.1, 0.1), simplify = FALSE)

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

  fig <- running_mean_plot(obj, variables = "y", params = c("beta", "gamma"), interactive = TRUE)
  expect_s3_class(fig, "plotly")
})
