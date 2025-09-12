test_that("set_restrictions restricts multiple ts", {
  # Case: One step ahead restrictions for multiple time series

  # Create sample ts_data
  ts1 <- stats::ts(1:10, start = c(2019, 1), frequency = 4)
  ts2 <- stats::ts(1:10, start = c(2019, 1), frequency = 4)
  ts_data <- list(var1 = ts1, var2 = ts2)

  variables_to_restrict <- c("var1", "var2")

  restrict_at_date <- 2021.25

  result <- set_restrictions(
    ts_data,
    variables_to_restrict,
    start = restrict_at_date,
    end = restrict_at_date
  )

  expected_result <- list(
    var1 = list(
      horizon = 1L,
      value = 10L
    ),
    var2 = list(
      horizon = 1L,
      value = 10L
    )
  )

  expect_identical(result, expected_result)
})

test_that("set_restrictions restricts single ts", {
  # Case: One step ahead restrictions for single time series

  # Create sample ts_data
  ts1 <- stats::ts(1:10, start = c(2019, 1), frequency = 4)
  ts2 <- stats::ts(1:10, start = c(2019, 1), frequency = 4)
  ts_data <- list(var1 = ts1, var2 = ts2)

  variables_to_restrict <- c("var1")

  restrict_at_date <- 2021.25

  result <- set_restrictions(
    ts_data,
    variables_to_restrict,
    start = restrict_at_date,
    end = restrict_at_date
  )

  expected_result <- list(
    var1 = list(
      horizon = 1L,
      value = 10L
    )
  )

  expect_identical(result, expected_result)
})

test_that("set_restrictions restricts multiple ts", {
  # Case: multi step ahead restrictions for multiple time series

  # Create sample ts_data
  ts1 <- stats::ts(1:11, start = c(2019, 1), frequency = 4)
  ts2 <- stats::ts(1:11, start = c(2019, 1), frequency = 4)
  ts_data <- list(var1 = ts1, var2 = ts2)

  variables_to_restrict <- c("var1", "var2")

  restrict_start <- 2021.25
  restrict_end <- 2021.5

  result <- set_restrictions(
    ts_data,
    variables_to_restrict,
    start = restrict_start,
    end = restrict_end
  )

  expected_result <- list(
    var1 = list(
      horizon = c(1L, 2L),
      value = c(10L, 11L)
    ),
    var2 = list(
      horizon = c(1L, 2L),
      value = c(10L, 11L)
    )
  )

  expect_identical(result, expected_result)
})

test_that("set_restrictions restricts single ts", {
  # Case: multi step ahead restrictions for single time series

  # Create sample ts_data
  ts1 <- stats::ts(1:9, start = c(2019, 1), frequency = 4)
  ts2 <- stats::ts(1:11, start = c(2019, 1), frequency = 4)
  ts_data <- list(var1 = ts1, var2 = ts2)

  variables_to_restrict <- c("var2")

  restrict_start <- 2021.25
  restrict_end <- 2021.5

  result <- set_restrictions(
    ts_data,
    variables_to_restrict,
    start = restrict_start,
    end = restrict_end
  )

  expected_result <- list(
    var2 = list(
      horizon = c(1L, 2L),
      value = c(10L, 11L)
    )
  )

  expect_identical(result, expected_result)
})

test_that("set_restrictions restricts multiple ts with ragged edge", {
  # Case: multi step ahead restrictions for multiple time series, with ragged
  # edge.

  # Create sample ts_data
  ts1 <- stats::ts(1:12, start = c(2019, 1), frequency = 4)
  ts2 <- stats::ts(1:10, start = c(2019, 1), frequency = 4)
  ts3 <- stats::ts(1:11, start = c(2019, 1), frequency = 4)
  ts_data <- list(var1 = ts1, var2 = ts2, var3 = ts3)

  variables_to_restrict <- c("var1", "var2", "var3")

  restrict_start <- 2021.25
  restrict_end <- 2021.75

  result <- set_restrictions(
    ts_data,
    variables_to_restrict,
    start = restrict_start,
    end = restrict_end
  )

  expected_result <- list(
    var1 = list(
      horizon = c(1L, 2L, 3L),
      value = c(10L, 11L, 12L)
    ),
    var2 = list(
      horizon = c(1L),
      value = c(10L)
    ),
    var3 = list(
      horizon = c(1L, 2L),
      value = c(10L, 11L)
    )
  )

  expect_identical(result, expected_result)
})
