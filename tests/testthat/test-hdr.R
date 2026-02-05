test_that("hdr.default returns expected structure for single level", {
  set.seed(123)
  x <- rnorm(2000)
  res <- hdr(x, probs = 0.8, n_grid = 2048)

  expect_s3_class(res, "koma_hdr")
  expect_true(is.list(res$intervals))
  expect_length(res$intervals, 1)
  expect_true(is.matrix(res$intervals[[1]]))
  expect_identical(colnames(res$intervals[[1]]), c("lower", "upper"))
  expect_true(is.numeric(res$mode))
  expect_true(is.numeric(res$cutoff))
  expect_true(is.numeric(res$mass))
  expect_true(res$mass >= 0.79 && res$mass <= 0.81)
})

test_that("hdr.default supports multiple probs levels", {
  set.seed(1)
  x <- rnorm(1500)
  res <- hdr(x, probs = c(0.5, 0.8, 0.95), n_grid = 1024)

  expect_length(res$intervals, 3)
  expect_identical(length(res$cutoff), 3L)
  expect_identical(length(res$mass), 3L)
  expect_true(all(res$probs == sort(unique(res$probs))))
})

test_that("hdr.default grid integration handles full mass", {
  set.seed(9)
  x <- rnorm(1000)

  res <- hdr(
    x,
    probs = 1,
    integration = "grid",
    n_grid = 512
  )

  interval <- res$intervals[[1]]
  expect_true(is.matrix(interval))
  expect_equal(nrow(interval), 1L)
  expect_false(anyNA(res$cutoff))
  expect_false(anyNA(res$mass))
  expect_true(unname(res$mass) <= 1)
})

test_that("hdr.default switch integration methods", {
  # integration method are approximately and asymptotically equivalent
  set.seed(202)
  x <- c(rnorm(600, -2, 0.7), rnorm(600, 2, 0.7))

  target_prob <- 0.8
  res_grid <- hdr(
    x,
    probs = target_prob,
    integration = "grid"
  )
  res_mc <- hdr(
    x,
    probs = target_prob,
    integration = "monte_carlo",
    mc_use_observed = FALSE,
    mc_draws = 4000
  )

  tol <- 0.02
  expect_true(is.numeric(res_grid$cutoff))
  expect_true(is.numeric(res_mc$cutoff))
  expect_lt(abs(res_grid$cutoff - res_mc$cutoff), tol)
  expect_equal(res_grid$intervals, res_mc$intervals, tolerance = tol)
  expect_equal(res_grid$mode, res_mc$mode)
  expect_equal(unname(res_grid$mass), target_prob, tolerance = tol)
  expect_equal(unname(res_mc$mass), target_prob, tolerance = tol)
})

test_that("hdr.default monte carlo integration with different cutoff types", {
  set.seed(202)
  x <- c(rnorm(600, -2, 0.7), rnorm(600, 2, 0.7))

  target_prob <- 0.8
  res_mc_type_1 <- withr::with_seed(
    7,
    hdr(
      x,
      probs = target_prob,
      integration = "monte_carlo",
      mc_use_observed = FALSE,
      mc_quantile_type = 1,
      mc_draws = 4000
    )
  )
  res_mc_type_7 <- withr::with_seed(
    7,
    hdr(
      x,
      probs = target_prob,
      integration = "monte_carlo",
      mc_use_observed = FALSE,
      mc_quantile_type = 7,
      mc_draws = 4000
    )
  )

  tol <- 0.002
  expect_lt(abs(res_mc_type_1$cutoff - res_mc_type_7$cutoff), tol)
  expect_equal(res_mc_type_1, res_mc_type_7, tolerance = tol)
})

test_that("hdr.koma_estimate computes HDRs for coefficients", {
  set.seed(42)

  sys_eq <- list(
    endogenous_variables = c("y", "z"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix(c(1, 0), nrow = 1),
    character_gamma_matrix = matrix(c(1, -1, 0, 1), nrow = 2)
  )

  beta_draws <- replicate(100, rnorm(1, 0.2, 0.1), simplify = FALSE)
  gamma_draws <- replicate(100, rnorm(1, -0.3, 0.1), simplify = FALSE)
  omega_draws <- replicate(100, rlnorm(1, 0, 0.1), simplify = FALSE)

  estimates <- list(
    y = list(
      beta_jw = beta_draws,
      gamma_jw = gamma_draws,
      omega_tilde_jw = omega_draws
    ),
    z = list(
      beta_jw = beta_draws,
      gamma_jw = gamma_draws,
      omega_tilde_jw = omega_draws
    )
  )

  obj <- structure(
    list(estimates = estimates, sys_eq = sys_eq),
    class = "koma_estimate"
  )

  res <- hdr(obj,
    variables = "y",
    probs = c(0.5, 0.8),
    include_sigma = TRUE,
    n_grid = 512
  )

  expect_s3_class(res, "koma_estimate_hdr")
  expect_s3_class(res, "koma_hdr")
  expect_true("y" %in% names(res$intervals))
  expect_true("beta" %in% names(res$intervals$y))
  expect_true("gamma" %in% names(res$intervals$y))
  expect_true("sigma" %in% names(res$intervals$y))
  expect_true("x" %in% names(res$intervals$y$beta))
  expect_true("z" %in% names(res$intervals$y$gamma))
  expect_true("omega" %in% names(res$intervals$y$sigma))
  expect_true("outliers" %in% names(res))
  expect_true("y" %in% names(res$outliers))
})

test_that("hdr.koma_estimate matches variable names exactly", {
  set.seed(11)

  sys_eq <- list(
    endogenous_variables = c("y.1", "yX1"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix(c(1, 0), nrow = 1),
    character_gamma_matrix = matrix(1, nrow = 2, ncol = 2)
  )

  beta_draws <- replicate(50, rnorm(1, 0.2, 0.1), simplify = FALSE)

  estimates <- list(
    `y.1` = list(
      beta_jw = beta_draws,
      gamma_jw = list(),
      omega_tilde_jw = list()
    ),
    yX1 = list(
      beta_jw = beta_draws,
      gamma_jw = list(),
      omega_tilde_jw = list()
    )
  )

  obj <- structure(
    list(estimates = estimates, sys_eq = sys_eq),
    class = "koma_estimate"
  )

  res <- hdr(obj, variables = "y.1", probs = 0.8, n_grid = 256)

  expect_true("y.1" %in% names(res$intervals))
  expect_false("yX1" %in% names(res$intervals))
})

test_that("hdr validates inputs", {
  # `x` must contain at least two finite numeric values.
  expect_error(hdr(1))
  # `probs` must be numeric values in (0, 1] or [0, 100].
  expect_error(hdr(c(1, 2), probs = 0))
  # `probs` must be in (0, 1] or [0, 100].
  expect_error(hdr(c(1, 2), probs = 101))
})

test_that("summary.koma_estimate_hdr errors on missing variables", {
  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix(1, nrow = 1),
    character_gamma_matrix = matrix(1, nrow = 1)
  )

  beta_draws <- replicate(50, rnorm(1, 0.2, 0.1), simplify = FALSE)
  gamma_draws <- replicate(50, rnorm(1, -0.3, 0.1), simplify = FALSE)

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

  res <- hdr(obj, probs = 0.8, n_grid = 256)

  expect_error(
    summary(res, variables = "consump"),
    "Variable not found in endogenous variables"
  )
})

test_that("summary.koma_forecast_hdr prints without error", {
  mode_ts <- stats::ts(c(0.5, 1.5), start = c(2020, 1), frequency = 4)
  time_labels <- format(stats::time(mode_ts), trim = TRUE)
  interval_1 <- matrix(c(0, 1), ncol = 2, dimnames = list(NULL, c("lower", "upper")))
  interval_2 <- matrix(c(1, 2), ncol = 2, dimnames = list(NULL, c("lower", "upper")))
  intervals <- list(level_90 = list(
    y = stats::setNames(list(interval_1, interval_2), time_labels)
  ))

  obj <- structure(
    list(
      intervals = intervals,
      mode = list(y = mode_ts),
      cutoff = list(),
      mass = list(),
      probs = 0.9
    ),
    class = c("koma_forecast_hdr", "koma_hdr")
  )

  expect_silent(capture.output(summary(obj)))
  expect_silent(capture.output(summary(obj, horizon = 1)))
  expect_silent(capture.output(summary(obj, horizon = time_labels[1])))
})
