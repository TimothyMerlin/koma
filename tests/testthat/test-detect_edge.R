test_that("detect_edge returns the earliest date correctly", {
  # Create sample ts_data
  ts1 <- stats::ts(1:9, start = c(2019, 1), frequency = 4)
  ts2 <- stats::ts(1:10, start = c(2019, 1), frequency = 4)
  ts_data <- list(var1 = ts1, var2 = ts2)

  # Test without truncation
  result <- detect_edge(ts_data, 2019, 2021.25)
  expect_equal(result$date, 2021)
  expect_equal(result$variable_names, "var1")
})

test_that("detect_edge handles multiple variables with unobserved", {
  # Create sample ts_data
  ts1 <- stats::ts(1:10, start = c(2020, 1), frequency = 4)
  ts2 <- stats::ts(1:10, start = c(2020, 1), frequency = 4)
  ts_data <- list(var1 = ts1, var2 = ts2)

  result <- detect_edge(ts_data, 2020, 2022.5)
  expect_equal(result$date, 2022.25)
  expect_equal(sort(result$variable_names), c("var1", "var2"))
})

test_that("detect_edge handles estimation_end correctly", {
  # Create sample ts_data
  ts1 <- stats::ts(1:10, start = c(2020, 1), frequency = 4)
  ts2 <- stats::ts(1:10, start = c(2021, 1), frequency = 4)
  ts_data <- list(var1 = ts1, var2 = ts2)

  # Test when with different sample start
  result <- detect_edge(ts_data, 2020, 2022)
  expect_equal(result$date, 2022)
  expect_equal(result$variable_names, names(ts_data))
})

test_that("detect_edge stops when estimation_end is before data start", {
  # Create sample ts_data
  ts1 <- stats::ts(1:10, start = c(2018, 1), frequency = 4)
  ts2 <- stats::ts(1:10, start = c(2020, 1), frequency = 4)
  ts_data <- list(var1 = ts1, var2 = ts2)

  estimation_start <- 2018
  estimation_end <- 2020

  # Test with estimation_end earlier than earliest date
  expect_error(
    detect_edge(ts_data, estimation_start, estimation_end),
    "var2"
  )
})

test_that("detect_edge detects the latest start date correctly", {
  ts1 <- stats::ts(1:10, start = c(2018, 1), frequency = 4)
  ts2 <- stats::ts(1:10, start = c(2020, 1), frequency = 4)
  ts_data <- list(var1 = ts1, var2 = ts2)

  result <- detect_edge(ts_data, 2017, 2025, direction = "start")
  expect_equal(result$date, 2020)
  expect_equal(result$variable_names, "var2")
})
