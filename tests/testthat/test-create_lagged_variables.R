test_that("create_lagged_variables", {
  equations <- "x2 ~ constant + x2.L(1) + x2.L(3) + x"

  exogenous_variables <- c("x")

  # Create sample ts_data
  x <- stats::ts(1:12, start = c(2019, 1), frequency = 4)
  x2 <- stats::ts(1:12, start = c(2019, 1), frequency = 4)
  ts_data <- list(x = x, x2 = x2)

  sys_eq <- system_of_equations(equations, exogenous_variables)
  endogenous_variables <- sys_eq$endogenous_variables
  predetermined_variables <- sys_eq$predetermined_variables
  total_exogenous_variables <- sys_eq$total_exogenous_variables

  result <- create_lagged_variables(
    ts_data,
    endogenous_variables,
    exogenous_variables,
    predetermined_variables
  )

  x <- stats::ts(1:12, start = c(2019, 1), frequency = 4)
  x2 <- stats::ts(1:12, start = c(2019, 1), frequency = 4)
  `x2.L(1)` <- stats::ts(1:12, start = c(2019, 2), frequency = 4)
  `x2.L(3)` <- stats::ts(1:12, start = c(2019, 4), frequency = 4)
  expected_result <- list(x = x, x2 = x2, "x2.L(1)" = `x2.L(1)`, "x2.L(3)" = `x2.L(3)`)

  expect_equal(result, expected_result)
})
