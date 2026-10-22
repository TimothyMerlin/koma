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

test_that("restrictions_to_horizon converts a ts to horizon and value", {
  restrictions <- list(
    gdp = stats::ts(c(0.3, 0.2), start = c(2025, 2), frequency = 4)
  )

  result <- restrictions_to_horizon(restrictions, 2025, 4, 4)

  expect_identical(
    result,
    list(gdp = list(horizon = c(2L, 3L), value = c(0.3, 0.2)))
  )
})

test_that("restrictions_to_horizon skips missing values in a ts", {
  restrictions <- list(
    gdp = stats::ts(c(0.3, NA, 0.2), start = c(2025, 1), frequency = 4)
  )

  result <- restrictions_to_horizon(restrictions, 2025, 4, 4)

  expect_identical(
    result,
    list(gdp = list(horizon = c(1L, 3L), value = c(0.3, 0.2)))
  )
})

test_that("restrictions_to_horizon converts dates to horizons", {
  expected <- list(gdp = list(horizon = 2L, value = 0.5))

  # c(year, quarter) and the numeric date give the same horizon
  expect_identical(
    restrictions_to_horizon(list(gdp = list(date = c(2025, 2), value = 0.5)), 2025, 4, 4),
    expected
  )
  expect_identical(
    restrictions_to_horizon(list(gdp = list(date = 2025.25, value = 0.5)), 2025, 4, 4),
    expected
  )

  expected <- list(gdp = list(horizon = c(1L, 3L), value = c(0.5, 0.7)))
  # several dates as numeric vector or as list of c(year, quarter)
  expect_identical(
    restrictions_to_horizon(
      list(gdp = list(date = c(2025, 2025.5), value = c(0.5, 0.7))), 2025, 4, 4
    ),
    expected
  )
  expect_identical(
    restrictions_to_horizon(
      list(gdp = list(date = list(c(2025, 1), c(2025, 3)), value = c(0.5, 0.7))),
      2025, 4, 4
    ),
    expected
  )
})

test_that("restrictions_to_horizon keeps horizon restrictions unchanged", {
  restrictions <- list(
    gdp = list(value = c(0.5, 0.7), horizon = c(1, 3)),
    manufacturing = stats::ts(1, start = c(2025, 4), frequency = 4)
  )

  result <- restrictions_to_horizon(restrictions, 2025, 4, 4)

  expect_identical(result$gdp, restrictions$gdp)
  expect_identical(result$manufacturing, list(horizon = 4L, value = 1))
  expect_null(restrictions_to_horizon(NULL, 2025, 4, 4))
  expect_identical(restrictions_to_horizon(list(), 2025, 4, 4), list())
})

test_that("restrictions_to_horizon rejects invalid dates", {
  check <- function(restrictions, pattern) {
    expect_error(
      restrictions_to_horizon(restrictions, 2025, 4, 4),
      pattern,
      class = "rlang_error"
    )
  }

  # before the forecast start and after the forecast end
  check(list(gdp = list(date = c(2024, 4), value = 0.5)), "2025 Q1.*2025 Q4")
  check(list(gdp = list(date = c(2026, 1), value = 0.5)), "2025 Q1.*2025 Q4")
  check(
    list(gdp = stats::ts(c(0.5, 0.4), start = c(2025, 4), frequency = 4)),
    "2025 Q1.*2025 Q4"
  )
  # not a quarter
  check(list(gdp = list(date = 2025.1, value = 0.5)), "period")
  # date and value differ in length
  check(list(gdp = list(date = c(2025, 2025.25), value = 0.5)), "same length")
  # date and horizon together
  check(list(gdp = list(date = c(2025, 2), horizon = 2, value = 0.5)), "either")
  # frequency of the ts does not match the data
  check(list(gdp = stats::ts(0.5, start = 2025, frequency = 1)), "frequency")
  # ts without any value
  check(list(gdp = stats::ts(NA_real_, start = c(2025, 1), frequency = 4)), "no values")
  # koma_ts in levels
  check(
    list(gdp = ets(100, start = c(2025, 1), frequency = 4, series_type = "level")),
    "rate"
  )
})
