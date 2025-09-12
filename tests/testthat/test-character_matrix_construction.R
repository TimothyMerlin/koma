test_that("construct_gamma_matrix constructs the Gamma matrix correctly", {
  # Test case 1: Constructing the gamma matrix
  equations <- c(
    "consumption ~ constant + gdp + consumption.L(1) + consumption.L(2)",
    "investment ~ constant + gdp + investment.L(1) + real_interest_rate",
    "current_account ~ constant + current_account.L(1) + world_gdp",
    "manufacturing ~ constant + manufacturing.L(1) + world_gdp",
    "service ~ constant + service.L(1) + population + gdp",
    "gdp == manufacturing + service"
  )
  endogenous_variables <- c(
    "consumption", "investment", "current_account",
    "manufacturing", "service", "gdp"
  )
  expected_matrix <- matrix(
    c(
      1, 0, 0, 0, 0, 0,
      0, 1, 0, 0, 0, 0,
      0, 0, 1, 0, 0, 0,
      0, 0, 0, 1, 0, "-theta6_4",
      0, 0, 0, 0, 1, "-theta6_5",
      "-gamma1_6", "-gamma2_6", 0, 0, "-gamma5_6", 1
    ),
    nrow = 6, ncol = 6, byrow = TRUE,
    dimnames = list(
      c(
        "consumption", "investment", "current_account", "manufacturing",
        "service", "gdp"
      ), c(
        "consumption", "investment", "current_account",
        "manufacturing", "service", "gdp"
      )
    )
  )
  result <-
    construct_gamma_matrix(equations, endogenous_variables)

  expect_identical(result, expected_matrix)
})

test_that("construct_gamma_matrix constructs the Gamma matrix correctly", {
  # Test case: same result with and without variables defined for weight
  # calculations
  equations <- c(
    "consumption ~ constant + gdp + consumption.L(1) + consumption.L(2)",
    "investment ~ constant + gdp + investment.L(1) + real_interest_rate",
    "current_account ~ constant + current_account.L(1) + world_gdp",
    "manufacturing ~ constant + manufacturing.L(1) + world_gdp",
    "service ~ constant + service.L(1) + population + gdp",
    "gdp == 0.5*manufacturing + 0.5*service",
    "nom_manufacturing == 1*manufacturing + 1*defl_manufacturing"
  )
  endogenous_variables <- c(
    "consumption", "investment", "current_account",
    "manufacturing", "service", "gdp", "nom_manufacturing"
  )

  expected_result <-
    construct_gamma_matrix(equations, endogenous_variables)

  equations <- c(
    "consumption ~ constant + gdp + consumption.L(1) + consumption.L(2)",
    "investment ~ constant + gdp + investment.L(1) + real_interest_rate",
    "current_account ~ constant + current_account.L(1) + world_gdp",
    "manufacturing ~ constant + manufacturing.L(1) + world_gdp",
    "service ~ constant + service.L(1) + population + gdp",
    "gdp == (nom_manufacturing/nom_gdp)*manufacturing + (nom_service/nom_gdp)*service",
    "nom_manufacturing == 1*manufacturing + 1*defl_manufacturing"
  )

  result <-
    construct_gamma_matrix(equations, endogenous_variables)

  expect_identical(result, expected_result)
})

test_that("construct_gamma_matrix, with lagged identity component", {
  equations <- c(
    "consumption ~ constant + gdp + consumption.L(1) + consumption.L(2)",
    "investment ~ constant + gdp + investment.L(1) + real_interest_rate",
    "current_account ~ constant + current_account.L(1) + world_gdp",
    "manufacturing ~ constant + manufacturing.L(1) + world_gdp",
    "service ~ constant + service.L(1) + population + gdp",
    "gdp == 0.5*manufacturing + (nom_service/nom_gdp)*service + current_account.L(1)",
    "nom_service == 1*service + 1*defl_service",
    "nom_gdp == 1*gdp + 1*defl_gdp"
  )
  endogenous_variables <- c(
    "consumption", "investment", "current_account",
    "manufacturing", "service", "gdp", "nom_service", "nom_gdp"
  )

  result <- construct_gamma_matrix(equations, endogenous_variables)

  expected_result <- matrix(
    c(
      1, 0, 0, 0, 0, 0, 0, 0,
      0, 1, 0, 0, 0, 0, 0, 0,
      0, 0, 1, 0, 0, 0, 0, 0,
      0, 0, 0, 1, 0, "-theta6_4", 0, 0,
      0, 0, 0, 0, 1, "-theta6_5", "-theta7_5", 0,
      "-gamma1_6", "-gamma2_6", 0, 0, "-gamma5_6", 1, 0, "-theta8_6",
      0, 0, 0, 0, 0, 0, 1, 0,
      0, 0, 0, 0, 0, 0, 0, 1
    ),
    nrow = 8, ncol = 8, byrow = TRUE,
    dimnames = list(
      c(
        "consumption", "investment", "current_account", "manufacturing",
        "service", "gdp", "nom_service", "nom_gdp"
      ), c(
        "consumption", "investment", "current_account",
        "manufacturing", "service", "gdp", "nom_service", "nom_gdp"
      )
    )
  )
  expect_identical(result, expected_result)
})

test_that("construct_beta_matrix constructs the B matrix correctly", {
  # Test case 1: Constructing the B matrix
  equations <- c(
    "consumption ~ constant + gdp + consumption.L(1) + consumption.L(2)",
    "investment ~ constant + gdp + investment.L(1) + real_interest_rate",
    "current_account ~ constant + current_account.L(1) + world_gdp",
    "manufacturing ~ constant + manufacturing.L(1) + world_gdp",
    "service ~ constant + service.L(1) + population + gdp",
    "gdp == manufacturing + service"
  )
  exogenous_variables <- c(
    "constant", "consumption.L(1)", "consumption.L(2)", "investment.L(1)",
    "current_account.L(1)", "manufacturing.L(1)", "service.L(1)",
    "real_interest_rate", "world_gdp", "population"
  )
  expected_matrix <- matrix(
    c(
      "constant1", "constant2", "constant3", "constant4", "constant5", "0",
      "beta1_2", "0", "0", "0", "0", "0",
      "beta1_3", "0", "0", "0", "0", "0",
      "0", "beta2_4", "0", "0", "0", "0",
      "0", "0", "beta3_5", "0", "0", "0",
      "0", "0", "0", "beta4_6", "0", "0",
      "0", "0", "0", "0", "beta5_7", "0",
      "0", "beta2_8", "0", "0", "0", "0",
      "0", "0", "beta3_9", "beta4_9", "0", "0",
      "0", "0", "0", "0", "beta5_10", "0"
    ),
    nrow = 10, ncol = 6, byrow = TRUE,
    dimnames = list(
      c(
        "constant", "consumption.L(1)", "consumption.L(2)", "investment.L(1)",
        "current_account.L(1)", "manufacturing.L(1)", "service.L(1)",
        "real_interest_rate", "world_gdp", "population"
      ), c(
        "consumption", "investment",
        "current_account", "manufacturing", "service", "gdp"
      )
    )
  )
  result_matrix <-
    construct_beta_matrix(equations, exogenous_variables)

  expect_identical(result_matrix, expected_matrix)
})

test_that("construct_beta_matrix works correctly", {
  # Test case: same result with and without variables defined for weight
  # calculations
  equations <- c(
    "consumption ~ constant + gdp + consumption.L(1) + consumption.L(2)",
    "investment ~ constant + gdp + investment.L(1) + real_interest_rate",
    "current_account ~ constant + current_account.L(1) + world_gdp",
    "manufacturing ~ constant + manufacturing.L(1) + world_gdp",
    "service ~ constant + service.L(1) + population + gdp",
    "gdp == 0.5*manufacturing + 0.5*service",
    "nom_manufacturing == 1*defl_manufacturing + 1*manufacturing"
  )
  exogenous_variables <- c(
    "constant", "consumption.L(1)", "consumption.L(2)", "investment.L(1)",
    "current_account.L(1)", "manufacturing.L(1)", "service.L(1)",
    "real_interest_rate", "world_gdp", "population", "defl_manufacturing"
  )

  expected_result <-
    construct_beta_matrix(equations, exogenous_variables)

  equations <- c(
    "consumption ~ constant + gdp + consumption.L(1) + consumption.L(2)",
    "investment ~ constant + gdp + investment.L(1) + real_interest_rate",
    "current_account ~ constant + current_account.L(1) + world_gdp",
    "manufacturing ~ constant + manufacturing.L(1) + world_gdp",
    "service ~ constant + service.L(1) + population + gdp",
    "gdp == (nom_manufacturing/nom_gdp)*manufacturing + (nom_service/nom_gdp)*service",
    "nom_manufacturing == 1*defl_manufacturing + 1*manufacturing"
  )

  result <-
    construct_beta_matrix(equations, exogenous_variables)

  expect_identical(result, expected_result)
})

test_that("construct_beta_matrix, with lagged identity component", {
  equations <- c(
    "consumption ~ constant + gdp + consumption.L(1) + consumption.L(2)",
    "investment ~ constant + gdp + investment.L(1) + real_interest_rate",
    "current_account ~ constant + current_account.L(1) + world_gdp",
    "manufacturing ~ constant + manufacturing.L(1) + world_gdp",
    "service ~ constant + service.L(1) + population + gdp",
    "gdp == 0.5*manufacturing + (nom_service/nom_gdp)*service + 1*current_account.L(1)"
  )
  exogenous_variables <- c(
    "constant", "consumption.L(1)", "consumption.L(2)", "investment.L(1)",
    "current_account.L(1)", "manufacturing.L(1)", "service.L(1)",
    "real_interest_rate", "world_gdp", "population", "nom_service", "nom_gdp"
  )

  result <- construct_beta_matrix(equations, exogenous_variables)

  expected_result <- matrix(
    c(
      "constant1", "constant2", "constant3", "constant4", "constant5", "0",
      "beta1_2", "0", "0", "0", "0", "0",
      "beta1_3", "0", "0", "0", "0", "0",
      "0", "beta2_4", "0", "0", "0", "0",
      "0", "0", "beta3_5", "0", "0", "theta6_5",
      "0", "0", "0", "beta4_6", "0", "0",
      "0", "0", "0", "0", "beta5_7", "0",
      "0", "beta2_8", "0", "0", "0", "0",
      "0", "0", "beta3_9", "beta4_9", "0", "0",
      "0", "0", "0", "0", "beta5_10", "0",
      "0", "0", "0", "0", "0", "0",
      "0", "0", "0", "0", "0", "0"
    ),
    nrow = 12, ncol = 6, byrow = TRUE,
    dimnames = list(
      c(
        "constant", "consumption.L(1)", "consumption.L(2)", "investment.L(1)",
        "current_account.L(1)", "manufacturing.L(1)", "service.L(1)",
        "real_interest_rate", "world_gdp", "population", "nom_service",
        "nom_gdp"
      ), c(
        "consumption", "investment",
        "current_account", "manufacturing", "service", "gdp"
      )
    )
  )
  expect_identical(result, expected_result)
})
