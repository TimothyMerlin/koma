test_that("quantiles_from_estimates handles 1D data correctly", {
  target_median <- 0.5
  random_numbers <- withr::with_seed(
    7,
    stats::rnorm(1000, mean = target_median, sd = 0.1)
  )
  actual_median <- median(random_numbers)
  # If actual median is not close enough to target median, adjust the sequence
  difference <- target_median - actual_median
  adjusted_random_numbers <- random_numbers + difference

  data <-
    lapply(
      seq_len(1000),
      function(x) adjusted_random_numbers[x]
    )

  result <- quantiles_from_estimates(data,
    probs = c(0.01, 0.05, 0.5, 0.95, 0.99)
  )

  expected_result <-
    list(
      q_1 = c(`1%` = 0.278410156638919),
      q_5 = c(`5%` = 0.343298554990821),
      q_50 = c(`50%` = 0.5),
      q_95 = c(`95%` = 0.662185542504314),
      q_99 = c(`99%` = 0.731013768149522)
    )

  expect_equal(result, expected_result)
})

test_that("quantiles_from_estimates handles 2D data correctly", {
  data <- withr::with_seed(
    3,
    lapply(
      seq_len(100),
      function(x) runif(10)
    )
  )

  result <- quantiles_from_estimates(data, probs = c(0.05, 0.5, 0.95))

  expected_result <- list(q_5 = c(
    0.0843172634253278, 0.0557286608265713, 0.050990910991095,
    0.0113698793807998, 0.0429686858667992, 0.0620293236919679,
    0.0477024836698547, 0.0564856770681217, 0.118190691887867,
    0.0987022302928381
  ), q_50 = c(
    0.492475403938442,
    0.506454277317971, 0.441116455709562, 0.557146636187099, 0.58535134466365,
    0.539672697079368, 0.554669658537023, 0.43523066106718, 0.5508469841443,
    0.547085489262827
  ), q_95 = c(
    0.95882744488772, 0.926560049213003,
    0.951615844434127, 0.966894607373979, 0.933863173937425, 0.941162972373422,
    0.972531610087026, 0.922562993247993, 0.983353775274008, 0.925183013710193
  ))

  expect_equal(result, expected_result)
})

test_that("quantiles_from_estimates with 3D data also returning mean", {
  data <- withr::with_seed(
    3,
    lapply(
      seq_len(100),
      function(x) array(runif(4), dim = c(2, 2))
    )
  )
  result <- quantiles_from_estimates(data, include_mean = TRUE)

  expected_result <-
    list(
      q_5 = c(0.092757079238072, 0.0753395772655494),
      q_50 = c(
        0.579750949633308,
        0.489297762396745
      ),
      q_95 = c(0.959081962436903, 0.925620576238725),
      q_mean = c(0.52849774397444, 0.503689532666467)
    )

  expect_equal(result, expected_result)
})

test_that("quantiles_from_forecasts", {
  generate_fake_forecast <- function(rows, cols, start, end, freq) {
    data <- matrix(runif(rows * cols), nrow = rows, ncol = cols)
    colnames(data) <- c(
      "consumption", "investment", "current_account", "manufacturing",
      "service", "gdp"
    )
    stats::ts(data, start = start, end = end, frequency = freq)
  }

  # Generate fake forecasts
  num_draws <- 5
  start_forecast <- 2023.25
  end_forecast <- 2025.75
  fake_forecasts <- lapply(1:num_draws, function(x) {
    generate_fake_forecast(11, 6, start_forecast, end_forecast, 4)
  })

  result <- quantiles_from_forecasts(
    fake_forecasts, 4,
    probs = c(0.25, 0.5, 0.75)
  )

  expect_true(is.list(result))

  # Check if time series names correspond to quantiles
  expected_quantiles <- c("q_25", "q_50", "q_75")
  expect_named(result, expected_quantiles)

  # Check time
  expected_time <- seq(from = start_forecast, to = end_forecast, by = 0.25)
  expect_equal(
    as.vector(stats::time(result$q_50)),
    expected_time
  )
})

test_that("quantiles_from_forecasts keeps matrix shape for one column", {
  generate_fake_forecast <- function(rows, start, end, freq) {
    data <- matrix(runif(rows), nrow = rows, ncol = 1)
    colnames(data) <- "manufacturing"
    stats::ts(data, start = start, end = end, frequency = freq)
  }

  num_draws <- 5
  start_forecast <- 2023.25
  end_forecast <- 2025.75
  fake_forecasts <- lapply(seq_len(num_draws), function(x) {
    generate_fake_forecast(11, start_forecast, end_forecast, 4)
  })

  result <- quantiles_from_forecasts(
    fake_forecasts, 4,
    probs = c(0.25, 0.5, 0.75),
    include_mean = TRUE
  )

  expect_equal(dim(result$q_50), c(11, 1))
  expect_equal(dim(result$q_mean), c(11, 1))
})

test_that("quantiles_from_forecasts with data in levels", {
  generate_fake_forecast <- function(rows, cols, start, end, freq) {
    data <- matrix(runif(rows * cols), nrow = rows, ncol = cols)
    colnames(data) <- c(
      "consumption", "investment", "current_account", "manufacturing",
      "service", "gdp"
    )
    ts_data <- ets(data,
      start = start, end = end, frequency = freq,
      series_type = "rate", method = "diff_log"
    )
    attr(ts_data, "anker") <- c(100, start)
    ts_data
  }

  # Generate fake forecasts
  num_draws <- 5
  start_forecast <- 2023.25
  end_forecast <- 2025.75
  fake_forecasts <- lapply(1:num_draws, function(x) {
    out <- level(generate_fake_forecast(11, 6, start_forecast, end_forecast, 4))
    out[1, 1] <- NA
    out
  })

  # Run the function
  result <- quantiles_from_forecasts(
    fake_forecasts, 4,
    probs = c(0.25, 0.5, 0.75)
  )

  # Check if the output is a list of time series
  expect_true(is.list(result))

  # Check if time series names correspond to quantiles
  expected_quantiles <- c("q_25", "q_50", "q_75")
  expect_named(result, expected_quantiles)

  # Check time
  expected_time <- seq(
    from = start_forecast - 0.25, to = end_forecast, by = 0.25
  )
  expect_equal(
    as.vector(stats::time(result$q_50)),
    expected_time
  )
})
