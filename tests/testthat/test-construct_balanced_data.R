test_that("construct_balanced_data works", {
  sample_start <- c(1976, 1)
  dates <- list(estimation = list(), forecast = list(), current = NULL)
  start <- 1976
  end <- 2019.75

  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)
  endogenous_variables <- sys_eq$endogenous_variables
  total_exogenous_variables <- sys_eq$total_exogenous_variables
  x_matrix <- simulated_data$x_matrix
  y_matrix <- simulated_data$y_matrix

  # Construct tslist from x and y matrix of simulated data
  tslist_growth_x <- lapply(
    colnames(x_matrix),
    function(x) {
      stats::ts(
        x_matrix[, x],
        frequency = 4, start = sample_start
      )
    }
  )

  tslist_growth_y <- lapply(
    colnames(y_matrix),
    function(x) {
      stats::ts(
        y_matrix[, x],
        frequency = 4, start = sample_start
      )
    }
  )

  ts_data <- c(tslist_growth_x, tslist_growth_y)
  names(ts_data) <- c(
    colnames(x_matrix),
    colnames(y_matrix)
  )

  ts_data$`investment.L(1)`[1] <- NA
  ts_data$`consumption.L(1)`[1] <- NA
  ts_data$`consumption.L(2)`[1:2] <- NA
  ts_data$`investment.L(1)`[1] <- NA
  ts_data$`current_account.L(1)`[1] <- NA
  ts_data$`manufacturing.L(1)`[1] <- NA
  ts_data$`service.L(1)`[1] <- NA

  suppressWarnings(
    result <- construct_balanced_data(
      ts_data, endogenous_variables,
      total_exogenous_variables, start, end
    )
  )

  expect_warning(
    construct_balanced_data(
      ts_data, endogenous_variables,
      total_exogenous_variables, start, end
    ),
    "Estimation start moved to"
  )

  expect_identical(result$number_of_observations, 174L)
  expect_identical(names(result), c(
    "truncated_ts_data", "y_matrix", "x_matrix",
    "number_of_observations", "edge", "freq"
  ))
})

test_that("construct_balanced_data works", {
  sample_start <- c(1976, 1)
  dates <- list(estimation = list(), forecast = list(), current = NULL)
  truncate_start <- 1977
  truncate_end <- 2019.75

  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)
  endogenous_variables <- sys_eq$endogenous_variables
  exogenous_variables <- sys_eq$exogenous_variables
  predetermined_variables <- sys_eq$predetermined_variables
  total_exogenous_variables <- sys_eq$total_exogenous_variables
  x_matrix <- simulated_data$x_matrix
  y_matrix <- simulated_data$y_matrix

  ts_data <- simulated_data$ts_data
  ts_data <- create_lagged_variables(
    ts_data, endogenous_variables, exogenous_variables, predetermined_variables
  )

  ts_with_nas <- ets(
    1:200,
    frequency = 4, start = c(1977, 1),
    series_type = "rate"
  )
  ts_with_nas[80:90] <- NA
  ts_data <- c(ts_data, list(ts_with_NAs = ts_with_nas))

  result <- construct_balanced_data(
    ts_data, endogenous_variables,
    total_exogenous_variables, truncate_start, truncate_end
  )

  expect_identical(result$number_of_observations, 172L)
  expect_identical(names(result), c(
    "truncated_ts_data", "y_matrix", "x_matrix",
    "number_of_observations", "edge", "freq"
  ))
  expect_identical(
    result$edge$variable_names,
    c(
      "real_interest_rate", "world_gdp", "population", "consumption",
      "investment", "current_account", "manufacturing", "service",
      "gdp", "consumption.L(1)", "consumption.L(2)", "investment.L(1)",
      "current_account.L(1)", "manufacturing.L(1)", "service.L(1)"
    )
  )
})

test_that("construct_balanced_data truncates at edge with ragged data", {
  # Create sample ts_data
  endogenous_ts_1 <- stats::ts(1:19, start = c(2019, 1), frequency = 4)
  endogenous_ts_2 <- stats::ts(1:20, start = c(2019, 1), frequency = 4)
  exogenous_ts_1 <- stats::ts(1:20, start = c(2019, 1), frequency = 4)
  exogenous_ts_2 <- stats::ts(1:20, start = c(2019, 1), frequency = 4)

  ts_data <- list(
    endogenous_ts_1 = endogenous_ts_1,
    endogenous_ts_2 = endogenous_ts_2,
    exogenous_ts_1 = exogenous_ts_1,
    exogenous_ts_2 = exogenous_ts_2
  )
  endogenous_variables <- c("endogenous_ts_1", "endogenous_ts_2")
  exogenous_to_include <- c("constant", "exogenous_ts_1", "exogenous_ts_2")
  estimation_start <- 2019
  estimation_end <- 2023.75

  result <- construct_balanced_data(
    ts_data, endogenous_variables,
    exogenous_to_include, estimation_start, estimation_end
  )
  expect_identical(result$number_of_observations, 19L)

  expected_y_matrix_1 <- structure(1:19, tsp = c(2019, 2023.5, 4), class = "ts")
  expect_identical(result$y_matrix[, 1], expected_y_matrix_1)
  expect_identical(colnames(result$y_matrix), endogenous_variables)
  expect_identical(colnames(result$x_matrix), exogenous_to_include)
  expect_identical(result$edge$date, 2023.5)
  expect_identical(names(result), c(
    "truncated_ts_data", "y_matrix", "x_matrix", "number_of_observations",
    "edge", "freq"
  ))
})

test_that("construct_balanced_data truncates at edge in one exogenous case", {
  # Create sample ts_data
  endogenous_ts_1 <- stats::ts(1:19, start = c(2019, 1), frequency = 4)
  endogenous_ts_2 <- stats::ts(1:20, start = c(2019, 1), frequency = 4)
  exogenous_ts_1 <- stats::ts(1:20, start = c(2019, 1), frequency = 4)

  ts_data <- list(
    endogenous_ts_1 = endogenous_ts_1,
    endogenous_ts_2 = endogenous_ts_2,
    exogenous_ts_1 = exogenous_ts_1
  )
  endogenous_variables <- c("endogenous_ts_1", "endogenous_ts_2")
  exogenous_to_include <- c("constant", "exogenous_ts_1")
  estimation_start <- 2019
  estimation_end <- 2023.75

  result <- construct_balanced_data(
    ts_data, endogenous_variables,
    exogenous_to_include, estimation_start, estimation_end
  )
  expect_identical(result$number_of_observations, 19L)

  expected_y_matrix_1 <- structure(1:19, tsp = c(2019, 2023.5, 4), class = "ts")
  expect_identical(result$y_matrix[, 1], expected_y_matrix_1)
  expect_identical(colnames(result$y_matrix), endogenous_variables)
  expect_identical(colnames(result$x_matrix), exogenous_to_include)
  expect_identical(result$edge$date, 2023.5)
  expect_identical(names(result), c(
    "truncated_ts_data", "y_matrix", "x_matrix", "number_of_observations",
    "edge", "freq"
  ))
})

test_that("construct_balanced_data, SEM with only one equation", {
  sample_start <- c(1976, 1)
  dates <- list(estimation = list(), forecast = list(), current = NULL)
  truncate_start <- 1977
  truncate_end <- 2019.75

  equations <-
    "manufacturing ~ world_gdp"
  exogenous_variables <- c("world_gdp")

  sys_eq <- system_of_equations(equations, exogenous_variables)
  endogenous_variables <- sys_eq$endogenous_variables
  total_exogenous_variables <- sys_eq$total_exogenous_variables
  x_matrix <- simulated_data$x_matrix
  y_matrix <- simulated_data$y_matrix

  ts_data <- simulated_data$ts_data

  result <- construct_balanced_data(
    ts_data, endogenous_variables,
    total_exogenous_variables, truncate_start, truncate_end
  )

  expect_true(inherits(result$y_matrix, "matrix"))
  expect_true(inherits(result$x_matrix, "matrix"))
})

# Test 1: Checking that the function correctly identifies NA-containing columns
test_that("validate_matrices, with NAs", {
  dataset1 <- data.frame(
    A = c(1, 2, NA, 4),
    B = c("a", "b", "c", "d")
  )

  dataset2 <- data.frame(
    C = c(NA, 5, 6, 7),
    D = c("x", "y", "z", NA)
  )

  expect_error(
    validate_matrices(dataset1, dataset2)
  )
  matrix1 <- matrix(
    c(1, NA, 3, 4, 5, NA),
    nrow = 2,
    dimnames = list(
      c(),
      c("a", "b", "c")
    )
  )
  expect_error(
    validate_matrices(matrix1, dataset2)
  )
})

# Test 2: Checking that the function correctly identifies no NA-containing
# columns
test_that("No NA-containing columns are identified correctly", {
  df1 <- data.frame(a = c(1, 2, 3), b = c(3, 4, 5))
  df2 <- data.frame(c = c(6, 7, 8), d = c(9, 10, 11))
  m1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)

  expect_true(validate_matrices(df1, df2))
  expect_true(validate_matrices(df1, m1))
})
