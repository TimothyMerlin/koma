test_that("hdi.default returns expected structure", {
  set.seed(1)
  x <- rnorm(200)

  res <- hdi(x, probs = c(0.5, 0.9))

  expect_s3_class(res, "koma_hdi")
  expect_true(is.numeric(res$mode))
  expect_true(all(is.na(res$cutoff)))
  expect_equal(sort(res$probs), res$probs)
  expect_equal(length(res$intervals), 2)
  expect_true(all(vapply(res$intervals, nrow, integer(1)) == 1L))
  expect_true(all(vapply(res$intervals, ncol, integer(1)) == 2L))
  expect_true(all(vapply(res$mass, is.numeric, logical(1))))
})

test_that("hdi.default matches normal quantiles for symmetric draws", {
  set.seed(4)
  x <- rnorm(5000)

  res <- hdi(x, probs = 0.8)
  interval <- res$intervals[[1]]

  expected <- stats::qnorm(c(0.1, 0.9))
  expect_equal(unname(interval[1, "lower"]), expected[1], tolerance = 0.05)
  expect_equal(unname(interval[1, "upper"]), expected[2], tolerance = 0.05)
})

test_that("hdi.koma_estimate returns coefficient intervals", {
  set.seed(2)

  sys_eq <- list(
    endogenous_variables = c("y"),
    total_exogenous_variables = c("x"),
    character_beta_matrix = matrix(1, nrow = 1),
    character_gamma_matrix = matrix(2, nrow = 1)
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

  res <- hdi(obj, probs = 0.8)

  expect_s3_class(res, "koma_estimate_hdi")
  expect_s3_class(res, "koma_hdi")
  expect_true(!is.null(res$intervals$y$beta$x))
  expect_true(!is.null(res$intervals$y$gamma$y))
})

test_that("hdi.koma_estimate matches variable names exactly", {
  set.seed(12)

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

  res <- hdi(obj, variables = "y.1", probs = 0.8)

  expect_true("y.1" %in% names(res$intervals))
  expect_false("yX1" %in% names(res$intervals))
})

test_that("hdr and hdi are close for unimodal draws", {
  set.seed(3)
  x <- rnorm(5000)

  hdr_res <- hdr(x, probs = 0.8, integration = "grid", n_grid = 4096)
  hdi_res <- hdi(x, probs = 0.8)

  hdr_interval <- hdr_res$intervals[[1]]
  hdi_interval <- hdi_res$intervals[[1]]

  expect_equal(nrow(hdr_interval), 1L)
  expect_equal(nrow(hdi_interval), 1L)
  hdr_width <- diff(hdr_interval[1, ])
  lower_diff <- abs(hdr_interval[1, "lower"] - hdi_interval[1, "lower"])
  upper_diff <- abs(hdr_interval[1, "upper"] - hdi_interval[1, "upper"])
  expect_lt(lower_diff / hdr_width, 0.05)
  expect_lt(upper_diff / hdr_width, 0.05)
})

test_that("summary.koma_forecast_hdi prints without error", {
  mode_ts <- stats::ts(c(0.5, 1.5), start = c(2020, 1), frequency = 4)
  lower_ts <- stats::ts(c(0, 1), start = c(2020, 1), frequency = 4)
  upper_ts <- stats::ts(c(1, 2), start = c(2020, 1), frequency = 4)

  intervals <- list(level_90 = list(
    y = list(lower = lower_ts, upper = upper_ts)
  ))

  obj <- structure(
    list(
      intervals = intervals,
      mode = list(y = mode_ts),
      cutoff = list(),
      mass = list(),
      probs = 0.9
    ),
    class = c("koma_forecast_hdi", "koma_hdi")
  )

  expect_silent(capture.output(summary(obj)))
  expect_silent(capture.output(summary(obj, horizon = 1)))
  expect_silent(capture.output(summary(obj, horizon = format(stats::time(mode_ts), trim = TRUE)[1])))
})
