test_that("detect_lag extracts correct lag number from variable name", {
  expect_equal(detect_lag("variable.L(3)"), 3L)
  expect_equal(detect_lag("variable.L(10)"), 10L)
  expect_equal(detect_lag("variable.L(2:3)"), c(2L, 3L))
  expect_equal(detect_lag("variable"), NA)
  expect_equal(detect_lag("variableL"), NA)
  expect_equal(detect_lag("variable.L(s)"), NA)
})

test_that("detect_lag fails if incorrect input type", {
  expect_error(
    detect_lag(-1),
    "The input should be a character string with the variable names."
  )

  expect_error(
    detect_lag(c("variable.L(1)", "variable.L(2)")),
    "The input should be a character string with the variable names."
  )
})

test_that("detect_lag fails if incorrect lag detected", {
  expect_error(
    detect_lag("variable.L(0)"),
    "Detected lag number is"
  )

  expect_error(
    detect_lag("variable.L(1.1)"),
    "Detected lag number is"
  )
})

test_that("get_max_lag returns the correct maximum lag", {
  # Test case 1: Vector of variables with lag
  variable_names <- c(
    "variable.L(2)", "variable", "variable.L(1)", "variable.L(3)",
    "variable.L(4)"
  )
  result <- get_max_lag(variable_names)
  expected <- 4L

  expect_equal(result, expected,
    info = "The maximum lag should be 4."
  )

  # Test case 2: Vector of variables with variable without lag
  variable_names <- c("variable", "variable")
  expect_equal(get_max_lag(variable_names), NA_integer_)

  # Test case 3: Return NA if NULL value provided
  expect_equal(get_max_lag(NULL), NA)
})
