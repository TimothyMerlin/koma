estimate_residual_model <- function() {
  dates <- list(
    estimation = list(start = c(1977, 1), end = c(2019, 4))
  )
  equations <- "manufacturing ~ world_gdp + manufacturing.L(1),
    service ~ population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"
  sys_eq <- system_of_equations(equations, c("world_gdp", "population"))

  withr::with_seed(
    7,
    estimate(simulated_data$ts_data, sys_eq, dates,
      options = list(gibbs = list(ndraws = 50))
    )
  )
}

test_that("residuals are the structural residuals at the mean coefficients", {
  skip_on_cran()
  est <- estimate_residual_model()
  result <- residuals(est)

  # identities have no residual
  expect_named(result, c("manufacturing", "service"))
  expect_s3_class(result$service, "ts")
  expect_equal(stats::tsp(result$service), stats::tsp(est$y_matrix))

  mean_draw <- function(draws) Reduce(`+`, draws) / length(draws)
  y <- est$y_matrix
  x <- est$x_matrix

  # service = gamma * gdp + beta' x + u, with a contemporaneous regressor
  service <- est$estimates$service
  beta <- mean_draw(service$beta_jw)
  gamma <- as.numeric(mean_draw(service$gamma_jw))
  expected <- y[, "service"] - gamma * y[, "gdp"] -
    x[, c("constant", "population")] %*% beta
  expect_equal(as.numeric(result$service), as.numeric(expected))

  manufacturing <- est$estimates$manufacturing
  beta <- mean_draw(manufacturing$beta_jw)
  expected <- y[, "manufacturing"] -
    x[, c("constant", "manufacturing.L(1)", "world_gdp")] %*% beta
  expect_equal(as.numeric(result$manufacturing), as.numeric(expected))
})

test_that("residuals recover the true errors of simulated data", {
  skip_on_cran()
  endogenous <- c("y1", "y2", "y3")
  exogenous <- c("x1", "x2", "x3")
  x_names <- c("constant", "y1.L(1)", exogenous)

  # y3 has two contemporaneous regressors, y1 and y2
  gamma <- diag(3)
  dimnames(gamma) <- list(endogenous, endogenous)
  gamma["y1", "y3"] <- -0.5
  gamma["y2", "y3"] <- 0.3
  beta <- matrix(0, 5, 3, dimnames = list(x_names, endogenous))
  beta[c("constant", "y1.L(1)", "x1"), "y1"] <- c(0.2, 0.5, 0.8)
  beta[c("constant", "x2"), "y2"] <- c(-0.1, 0.6)
  beta[c("constant", "x3"), "y3"] <- c(0.3, 0.4)

  sample <- withr::with_seed(3, generate_sample_data(
    sample_size = 400, sample_start = c(1900, 1), burnin = 50,
    gamma_matrix = gamma, beta_matrix = beta, sigma_matrix = diag(0.1, 3),
    endogenous_variables = endogenous, exogenous_variables = exogenous,
    predetermined_variables = "y1.L(1)"
  ))
  # the data are generated from y %*% gamma = x %*% beta + u
  true_errors <- stats::ts(
    sample$y_matrix %*% gamma - sample$x_matrix %*% beta,
    start = c(1900, 1), frequency = 4
  )

  sys_eq <- system_of_equations(
    "y1 ~ x1 + y1.L(1), y2 ~ x2, y3 ~ y1 + y2 + x3", exogenous
  )
  ts_data <- lapply(
    sample$ts_data[c(endogenous, exogenous)],
    as_ets,
    series_type = "rate", method = "none"
  )
  est <- withr::with_seed(3, estimate(
    ts_data, sys_eq, list(estimation = list(start = c(1900, 2), end = c(1999, 4))),
    options = list(gibbs = list(ndraws = 200))
  ))

  result <- residuals(est)
  for (variable in endogenous) {
    truth <- stats::window(true_errors[, variable],
      start = stats::start(result[[variable]]),
      end = stats::end(result[[variable]])
    )
    # equal up to the estimation error of the coefficients
    expect_gt(stats::cor(result[[variable]], truth), 0.99)
    expect_lt(mean(abs(result[[variable]] - truth)), 0.05)
  }
})

test_that("quantiles are taken over the residuals of each draw", {
  skip_on_cran()
  est <- estimate_residual_model()
  probs <- c(0.05, 0.5, 0.95)

  service <- est$estimates$service
  by_draw <- vapply(seq_along(service$beta_jw), function(jx) {
    as.numeric(
      est$y_matrix[, "service"] -
        as.numeric(service$gamma_jw[[jx]]) * est$y_matrix[, "gdp"] -
        est$x_matrix[, c("constant", "population")] %*% service$beta_jw[[jx]]
    )
  }, numeric(nrow(est$y_matrix)))
  expected <- t(apply(by_draw, 1, stats::quantile, probs = probs, names = FALSE))

  result <- residuals(est, central_tendency = "median", probs = probs)$service
  expect_equal(as.numeric(result[, c("q_5", "q_50", "q_95")]), as.numeric(expected))

  # the median column uses the median of each coefficient over the draws
  median_draw <- function(draws) apply(do.call(cbind, draws), 1, stats::median)
  expected_median <- est$y_matrix[, "service"] -
    median_draw(service$gamma_jw) * est$y_matrix[, "gdp"] -
    est$x_matrix[, c("constant", "population")] %*% median_draw(service$beta_jw)
  expect_equal(as.numeric(result[, "median"]), as.numeric(expected_median))
})

test_that("fitted values and residuals add up to the data", {
  skip_on_cran()
  est <- estimate_residual_model()

  for (central_tendency in c("mean", "median")) {
    res <- residuals(est, central_tendency = central_tendency)
    fit <- fitted(est, central_tendency = central_tendency)
    expect_named(fit, c("manufacturing", "service"))
    for (variable in names(fit)) {
      expect_equal(
        as.numeric(fit[[variable]] + res[[variable]]),
        as.numeric(est$y_matrix[, variable])
      )
    }
  }
})

test_that("residuals and fitted values return quantiles over the draws", {
  skip_on_cran()
  est <- estimate_residual_model()

  res <- residuals(est, probs = c(0.05, 0.95))
  expect_equal(colnames(res$service), c("mean", "q_5", "q_95"))
  expect_equal(stats::tsp(res$service), stats::tsp(est$y_matrix))
  expect_true(all(res$service[, "q_5"] <= res$service[, "mean"]))
  expect_true(all(res$service[, "mean"] <= res$service[, "q_95"]))
  # the central column equals the residuals without quantiles
  expect_equal(as.numeric(res$service[, "mean"]), as.numeric(residuals(est)$service))

  fit <- fitted(est, central_tendency = "median", probs = 0.5)
  expect_equal(colnames(fit$manufacturing), c("median", "q_50"))
  expect_equal(stats::tsp(fit$manufacturing), stats::tsp(est$y_matrix))
})

test_that("residuals rejects invalid arguments", {
  skip_on_cran()
  est <- estimate_residual_model()

  expect_error(residuals(est, central_tendency = "mode"), "mean")
  expect_error(residuals(est, probs = 1.5), "between 0 and 1")
  expect_error(residuals(est, probs = "a"), "numeric")
  expect_error(residuals(est, foo = 1))
})
