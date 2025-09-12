test_that("dates_to_num for case with one date", {
  result <- dates_to_num(c(2013, 2), 4)
  expect_identical(result, 2013.25)

  # contains NULL
  expect_equal(dates_to_num(NULL, 4), NULL)
})

test_that("dates_to_num is list", {
  x <- list(
    estimation = list(start = c(2012, 1), end = c(2024, 4))
  )
  result <- dates_to_num(x, 4)

  expect_identical(result, list(estimation = list(start = 2012, end = 2024.75)))

  x <- list(
    estimation = list(start = c(2012, 1), end = c(2024, 4)),
    forecast = list(start = 2019.25, end = c(2024, 4))
  )
  result <- dates_to_num(x, 4)

  expect_identical(
    result,
    list(
      estimation = list(start = 2012, end = 2024.75),
      forecast = list(start = 2019.25, end = 2024.75)
    )
  )
})

test_that("dates_to_str for double vector with one date", {
  result <- dates_to_str(c(2013, 2), 4)
  expect_identical(result, "2013 Q2")

  # contains NULL
  expect_equal(dates_to_str(NULL, 4), NULL)
})

test_that("dates_to_str for Date objects", {
  d <- as.Date("2021-11-05")
  expect_identical(dates_to_str(d, 4), "2021 Q4")
  expect_identical(dates_to_str(d, 12), "2021-11")
  # fallback to full date
  expect_identical(dates_to_str(d, 7), "2021-11-05")
})

test_that("dates_to_str for POSIXct objects", {
  dt <- as.POSIXct("2022-03-15 14:30:00", tz = "UTC")
  expect_identical(dates_to_str(dt, 4), "2022 Q1")
})

test_that("dates_to_str for list inputs", {
  x <- list(
    estimation = list(start = c(2012, 1), end = c(2024, 4))
  )
  result <- dates_to_str(x, 4)
  expect_identical(
    result,
    list(estimation = list(start = "2012 Q1", end = "2024 Q4"))
  )

  x2 <- list(
    a = c(2012, 1),
    b = as.Date("2023-02-20")
  )
  result2 <- dates_to_str(x2, 12)
  expect_identical(
    result2,
    list(a = "2012-01", b = "2023-02")
  )
})

test_that("num_to_dates for case with one date", {
  x <- 2013.25
  result <- num_to_dates(x, 4)

  expect_identical(result, c(2013, 2))
})

test_that("num_to_dates is list", {
  dates <- list(
    estimation = list(start = 2012, end = 2024.75)
  )
  result <- num_to_dates(dates, 4)

  expect_identical(
    result,
    list(estimation = list(start = c(2012, 1), end = c(2024, 4)))
  )

  dates <- list(
    estimation = list(start = c(2012), end = c(2024.75)),
    forecast = list(start = 2019.25, end = c(2024, 4))
  )
  result <- num_to_dates(dates, 4)

  expect_identical(
    result,
    list(
      estimation = list(start = c(2012, 1), end = c(2024, 4)),
      forecast = list(start = c(2019, 2), end = c(2024, 4))
    )
  )
})

test_that("Iterating by quarters works correctly with quarterly frequency", {
  expect_equal(iterate_n_periods(2020.00, 3, 4), 2020.75)
  expect_equal(iterate_n_periods(2020.00, 5, 4), 2021.25)
  expect_equal(iterate_n_periods(2020.75, 2, 4), 2021.25)
  expect_equal(iterate_n_periods(c(2020, 4), 2, 4), 2021.25)
})

test_that("Iterating by months works correctly with monthly frequency", {
  expect_equal(round(iterate_n_periods(2020.00, 11, 12), 4), 2020.9167)
  expect_equal(round(iterate_n_periods(2020.00, 13, 12), 4), 2021.0833)
  expect_equal(round(iterate_n_periods(c(2020, 12), 2, 12), 4), 2021.0833)
})

test_that("Iterating by years works correctly with yearly frequency", {
  expect_equal(iterate_n_periods(2022, 1, 1), 2023)
  expect_equal(iterate_n_periods(2020, 5, 1), 2025)
})
