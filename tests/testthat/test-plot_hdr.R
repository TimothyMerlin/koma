test_that("plot.koma_estimate_hdr produces an HDR boxplot", {
  skip_if_not_installed("ggplot2")
  set.seed(7)

  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix(1, nrow = 1),
    character_gamma_matrix = matrix(1, nrow = 1)
  )

  beta_draws <- replicate(200, rnorm(1, 0.2, 0.1), simplify = FALSE)
  gamma_draws <- replicate(200, rnorm(1, -0.3, 0.1), simplify = FALSE)

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

  res <- hdr(obj,
    probs = c(0.5, 0.99),
    n_grid = 256
  )

  fig <- plot(res, variables = "y")
  expect_s3_class(fig, "ggplot")
})

test_that("plot.koma_estimate_hdr shows multimodal HDR boxes", {
  skip_if_not_installed("ggplot2")
  set.seed(11)

  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix(1, nrow = 1),
    character_gamma_matrix = matrix(1, nrow = 1)
  )

  bimodal <- c(rnorm(150, -1, 0.2), rnorm(150, 1, 0.2))
  beta_draws <- as.list(bimodal)
  gamma_draws <- replicate(300, rnorm(1, 0, 0.1), simplify = FALSE)

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

  res <- hdr(obj,
    probs = c(0.5, 0.99),
    n_grid = 512
  )

  fig <- plot(res, variables = "y", interactive = FALSE)
  expect_s3_class(fig, "ggplot")
  built <- ggplot2::ggplot_build(fig)
  rect_data <- built$data[[1]]
  expect_true(nrow(rect_data) > 2)
})

test_that("plot.koma_estimate_hdr supports a single HDR level", {
  skip_if_not_installed("ggplot2")
  set.seed(21)

  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix(1, nrow = 1),
    character_gamma_matrix = matrix(1, nrow = 1)
  )

  beta_draws <- replicate(200, rnorm(1, 0.1, 0.1), simplify = FALSE)
  gamma_draws <- replicate(200, rnorm(1, -0.2, 0.1), simplify = FALSE)

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

  res <- hdr(obj, probs = 0.5, n_grid = 256)
  fig <- plot(res, variables = "y", interactive = FALSE)

  expect_s3_class(fig, "ggplot")
  built <- ggplot2::ggplot_build(fig)
  rect_data <- built$data[[1]]
  expect_true(nrow(rect_data) > 0)
  expect_equal(length(unique(rect_data$fill)), 1)
})
