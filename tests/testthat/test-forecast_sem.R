test_that("forecast_sem", {
  skip_on_cran()
  sys_eq <- simulated_data$sys_eq
  exogenous_variables <- simulated_data$exogenous_variables
  estimates <- simulated_data$estimates
  restrictions <- NULL
  ts_data <- simulated_data$ts_data
  dates <- list(estimation = list(), forecast = list(), current = NULL)
  dates$estimation$start <- c(1977, 1)
  dates$estimation$end <- c(2018, 4) # use pre-COVID-19 data
  # Current quarter (published data)
  dates$current <- c(2018, 4)
  # Begin of forecasts
  dates$forecast$start <- c(2019, 1)
  # Last quarter of forecast
  dates$forecast$end <- c(2019, 2)
  # Dates for Dynamic Weights calculation
  dates$dynamic_weights$start <- c(1992, 1)
  dates$dynamic_weights$end <- c(2018, 4)
  dates <- dates_to_num(dates, frequency = 4)

  # Define identity weights
  sys_eq$identities$gdp$weights$theta_gamma6_4 <- 0.5
  sys_eq$identities$gdp$weights$theta_gamma6_5 <- 0.5

  # Forecast horizon
  horizon <- length(seq(dates$forecast$start, dates$forecast$end, by = 1 / 4))

  ##### Create Lagged Variables
  ts_data <- create_lagged_variables(
    ts_data, sys_eq$endogenous_variables,
    exogenous_variables, sys_eq$predetermined_variables
  )
  ##### Construct Y and X matrix up to estimation_end
  balanced_data <- construct_balanced_data(
    ts_data, sys_eq$endogenous_variables,
    sys_eq$total_exogenous_variables,
    dates$estimation$start, dates$estimation$end
  )
  y_matrix <- balanced_data$y_matrix
  forecast_x_matrix <- stats::window(
    as_mets(ts_data[sys_eq$exogenous_variables]),
    start = dates$forecast$start,
    end = dates$forecast$end
  )

  freq <- 4
  # Case 1: Point estimates
  result <- forecast_sem(
    sys_eq, estimates, restrictions,
    y_matrix, forecast_x_matrix, horizon, freq,
    forecast_dates = dates$forecast,
    approximate = TRUE,
    probs = NULL
  )

  expect_true(all(c("mean", "median") %in% names(result)))

  expected_result <- structure(
    c(
      5.06362194561031, 4.80313168078516, 5.01077864344463,
      4.05315146122831, 0.542600872899129, 1.13132240196818, 0.451589786047729,
      0.542166813873791, 0.113673001423857, -0.120928186359856, 0.282631393735793,
      0.210619313756968
    ),
    dim = c(2L, 6L),
    dimnames = list(
      NULL,
      c("consumption", "investment", "current_account", "manufacturing", "service", "gdp")
    ), tsp = c(2019, 2019.25, 4),
    class = c("mts", "ts", "matrix", "array")
  )

  expect_equal(result$median, expected_result)

  # Case 2: Density forecasts
  result <- forecast_sem(
    sys_eq, estimates, restrictions,
    y_matrix, forecast_x_matrix, horizon, freq, dates$forecast,
    approximate = FALSE,
    probs = get_quantiles()
  )

  expected_cols <- c(
    "consumption", "investment", "current_account",
    "manufacturing", "service", "gdp"
  )
  expected_tsp <- c(2019, 2019.25, 4)

  lapply(result$quantiles, function(q) {
    expect_true(inherits(q, "ts"))
    expect_equal(dim(q), c(2L, 6L))
    expect_equal(colnames(q), expected_cols)
    expect_equal(stats::tsp(q), expected_tsp)
  })

  tol <- 1e-8
  q_5 <- result$quantiles$q_5
  q_50 <- result$quantiles$q_50
  q_95 <- result$quantiles$q_95
  expect_true(all(q_5 <= q_50 + tol))
  expect_true(all(q_50 <= q_95 + tol))
})

test_that("validate_identities warns when components are missing", {
  mat <- matrix(
    c(
      1, 1,
      2, 2
    ),
    ncol = 2,
    byrow = TRUE
  )
  colnames(mat) <- c("agg", "c1")
  ts_out <- stats::ts(mat, start = c(2023, 2), frequency = 4)

  expect_warning(
    validate_identities(ts_out, identities = list(
      agg = list(
        components = list(c1 = "w1", c2 = "w2"),
        weights = list(w1 = 0.5, w2 = 0.5)
      )
    )),
    "could not be checked"
  )
})

test_that("validate_identities warns when weights are non-numeric", {
  mat <- matrix(
    c(
      1, 1, 1,
      2, 2, 2
    ),
    ncol = 3,
    byrow = TRUE
  )
  colnames(mat) <- c("agg", "c1", "c2")
  ts_out <- stats::ts(mat, start = c(2023, 2), frequency = 4)

  expect_warning(
    validate_identities(ts_out, identities = list(
      agg = list(
        components = list(c1 = "w1", c2 = "w2"),
        weights = list(w1 = 0.5, w2 = "theta")
      )
    )),
    "could not be checked"
  )
})


test_that("validate_identities warns on deviations", {
  # Construct a simple ts_out with one identity variable and two components
  mat <- matrix(
    c(
      1, 2, 2, # identity too low
      2, 2, 2 # identity matches
    ),
    ncol = 3,
    byrow = TRUE
  )
  colnames(mat) <- c("agg", "c1", "c2")
  ts_out <- stats::ts(mat, start = c(2023, 2), frequency = 4)

  expect_warning(
    validate_identities(ts_out,
      identities = list(
        agg = list(
          components = list(c1 = "w1", c2 = "w2"),
          weights = list(w1 = 0.5, w2 = 0.5)
        )
      )
    ),
    "Identity"
  )
})

test_that("validate_identities is quiet when identities match", {
  mat <- matrix(
    c(
      1, 1, 1,
      2, 2, 2
    ),
    ncol = 3,
    byrow = TRUE
  )
  colnames(mat) <- c("agg", "c1", "c2")
  ts_out <- stats::ts(mat, start = c(2023, 2), frequency = 4)

  expect_silent(
    validate_identities(ts_out, identities = list(
      agg = list(
        components = list(c1 = "w1", c2 = "w2"),
        weights = list(w1 = 0.5, w2 = 0.5)
      )
    ))
  )
})

test_that("validate_identities warns on deviations with exogenous x_matrix", {
  ts_out <- stats::ts(
    cbind(agg = c(4, 5), c1 = c(1, 2)),
    start = c(2023, 2), frequency = 4
  )
  x_matrix <- cbind(x1 = c(2, 3))
  identities <- list(
    agg = list(
      components = list(c1 = "w1", x1 = "w2"),
      weights = list(w1 = 1, w2 = 1)
    )
  )

  # Only the first horizon violates agg == c1 + x1.
  expect_warning(
    validate_identities(ts_out, identities, x_matrix = x_matrix),
    'Identity "agg" not satisfied at horizon "1"'
  )
})

test_that("validate_identities uses distinct endogenous and exogenous weights", {
  sys_eq <- system_of_equations(
    "a ~ x1, b ~ a, y == 0.3*b + 0.7*x1",
    exogenous_variables = "x1"
  )
  mat <- matrix(
    c(
      1.7, 1,
      2.7, 2
    ),
    ncol = 2,
    byrow = TRUE
  )
  colnames(mat) <- c("y", "b")
  ts_out <- stats::ts(mat, start = c(2023, 2), frequency = 4)

  x_matrix <- matrix(c(2, 3), ncol = 1)
  colnames(x_matrix) <- "x1"

  expect_silent(
    validate_identities(ts_out,
      identities = sys_eq$identities,
      x_matrix = x_matrix
    )
  )
})

test_that("projection and eigen conditional draws match empirically", {
  set.seed(123)

  n <- 3
  horizon <- 4
  d <- n * horizon

  base <- matrix(rnorm(d * d), nrow = d)
  omega <- crossprod(base) # t(base) %*% base (if base is full rank then positive definite)
  # forces two zero eigenvalues → singular positive semidefinite
  zero_idx <- (d - 1):d
  omega[zero_idx, ] <- 0
  omega[, zero_idx] <- 0

  R <- matrix(0, nrow = 2, ncol = d)
  R[1, 1] <- 1
  R[2, n + 1] <- 1
  # set restriction values
  r <- matrix(c(0.2, -0.1), ncol = 1)

  A <- R %*% omega %*% t(R)
  ev_omega <- eigen(omega, symmetric = TRUE)
  idx_omega <- ev_omega$values > 0
  U_omega <- ev_omega$vectors[, idx_omega, drop = FALSE]
  D_omega <- ev_omega$values[idx_omega]

  n_draws <- 2000
  proj_draws <- matrix(NA_real_, nrow = d, ncol = n_draws)
  eig_draws <- matrix(NA_real_, nrow = d, ncol = n_draws)

  for (i in seq_len(n_draws)) {
    z <- rnorm(length(D_omega))
    v_uncond <- as.vector(U_omega %*% (sqrt(D_omega) * z))

    proj <- koma:::draw_conditional_innovations(
      v_uncond_vec = v_uncond,
      omega_matrix_h = omega,
      R = R,
      r = r,
      A = A,
      method = "projection"
    )

    eig <- koma:::draw_conditional_innovations(
      v_uncond_vec = v_uncond,
      omega_matrix_h = omega,
      R = R,
      r = r,
      A = A,
      method = "eigen"
    )

    proj_draws[, i] <- proj$draw_vec
    eig_draws[, i] <- eig$draw_vec
  }

  mean_proj <- rowMeans(proj_draws)
  mean_eig <- rowMeans(eig_draws)

  # Analytic conditional mean given R v = r.
  mu <- omega %*% t(R) %*% solve(A, r)
  Omega_c <- omega - omega %*% t(R) %*% solve(A) %*% R %*% omega
  Omega_c <- 0.5 * (Omega_c + t(Omega_c))

  # Mean MC error scales with marginal variance and 1/sqrt(n_draws).
  marginal_var <- pmax(diag(Omega_c), 0)
  se_mean <- sqrt(marginal_var / n_draws)
  idx_var <- se_mean > sqrt(.Machine$double.eps)
  z_proj <- max(abs((mean_proj[idx_var] - mu[idx_var]) / se_mean[idx_var]))
  z_eig <- max(abs((mean_eig[idx_var] - mu[idx_var]) / se_mean[idx_var]))
  alpha <- 1e-3
  z_thresh <- stats::qnorm(1 - alpha / d)
  expect_lt(z_proj, z_thresh)
  expect_lt(z_eig, z_thresh)
  if (any(!idx_var)) {
    expect_lt(max(abs(mean_proj[!idx_var] - mu[!idx_var])), 1e-10)
    expect_lt(max(abs(mean_eig[!idx_var] - mu[!idx_var])), 1e-10)
  }

  cov_proj <- stats::cov(t(proj_draws))
  cov_eig <- stats::cov(t(eig_draws))

  rel_frob <- function(S, T) {
    norm(S - T, "F") / max(norm(T, "F"), 1e-12)
  }
  # Covariance MC error from Wishart theory; larger Sigma => larger expected error.
  cov_rel_tol <- function(Sigma, n, k = 5) {
    diag_sigma <- diag(Sigma)
    var_ij <- (Sigma^2 + outer(diag_sigma, diag_sigma)) / (n - 1)
    se_frob <- sqrt(sum(var_ij))
    k * se_frob / max(norm(Sigma, "F"), 1e-12)
  }
  tol_cov <- cov_rel_tol(Omega_c, n_draws)
  expect_lt(rel_frob(cov_proj, Omega_c), tol_cov)
  expect_lt(rel_frob(cov_eig, Omega_c), tol_cov)

  # check if restrictions are fullfilled
  # computes for every draw: R %*% v - r
  proj_resid <- sweep(R %*% proj_draws, 1, as.vector(r), "-")
  eig_resid <- sweep(R %*% eig_draws, 1, as.vector(r), "-")
  # residuals of Rv - r are not exactly zero due to floating point arithmetics
  expect_lt(max(abs(proj_resid)), 1e-8)
  # and due to dropping small eigenvalues (> tol)
  expect_lt(max(abs(eig_resid)), 1e-6)
})

test_that("eigen conditional draws keep variances of a small scale", {
  set.seed(123)

  # restrict the first variable only
  R <- matrix(c(1, 0), nrow = 1)
  r <- matrix(0)

  eigen_draws <- function(omega) {
    A <- R %*% omega %*% t(R)
    vapply(seq_len(2000), function(i) {
      koma:::draw_conditional_innovations(
        v_uncond_vec = c(0, 0),
        omega_matrix_h = omega,
        R = R,
        r = r,
        A = A,
        method = "eigen"
      )$draw_vec
    }, numeric(2))
  }

  # all variances are small
  draws <- eigen_draws(diag(c(1e-12, 1e-12)))
  expect_equal(draws[1, ], rep(0, 2000))
  # the unrestricted variable keeps its variance of 1e-12
  expect_equal(stats::var(draws[2, ]) / 1e-12, 1, tolerance = 0.2)
})

test_that("eigen conditional draws meet restrictions on every variable", {
  set.seed(123)

  omega <- crossprod(matrix(rnorm(36), nrow = 6))
  R <- diag(6)
  r <- matrix(1:6)

  draw <- koma:::draw_conditional_innovations(
    v_uncond_vec = rep(0, 6),
    omega_matrix_h = omega,
    R = R,
    r = r,
    A = R %*% omega %*% t(R),
    method = "eigen"
  )

  expect_equal(draw$draw_vec, as.vector(r), tolerance = 1e-8)
})
