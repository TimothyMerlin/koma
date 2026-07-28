test_that("model_identification works", {
  raw_equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(raw_equations, exogenous_variables)

  expect_true(
    model_identification(
      sys_eq$character_gamma_matrix,
      sys_eq$character_beta_matrix,
      sys_eq$identities
    )
  )
})

test_that("model_identification throws error for unidentified structure", {
  # System of equations not fullfilling the order condition
  raw_equations <-
    "manufacturing ~ gdp + service + real_interest_rate + population + world_gdp,
    service ~ manufacturing + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  # order condition must fullfill:  K*_j >= M_j - 1
  # K - K_j >= M_j - 1
  # i.e. number of excluded exogenous must be equal or greater than number of
  # endogenous variables in eq j minus 1
  # number of equations is M_j + M*_j + 1 = M
  # number of exogenous variables is K_j + K*_j = K
  # total number of exogenous K = 3
  # exogenous included in equation K_1 = 3
  # endogenous included in equation M = 2
  # i.e. necessary order condition is not satisfied with 3 - 3 < 2 -1

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(raw_equations, exogenous_variables)

  expect_error(
    model_identification(
      sys_eq$character_gamma_matrix,
      sys_eq$character_beta_matrix,
      sys_eq$identities
    ),
    "Model identification error: order"
  )
})

test_that("model_identification throws error for unidentified structure", {
  # System of equations not fullfilling the rank condition
  # circular dependency between manufacturing and service
  raw_equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ service + world_gdp,
    service ~ manufacturing + gdp,
    gdp == 0.5*manufacturing + 0.5*service "

  exogenous_variables <- c("real_interest_rate", "world_gdp")

  sys_eq <- system_of_equations(raw_equations, exogenous_variables)

  expect_error(
    model_identification(
      sys_eq$character_gamma_matrix,
      sys_eq$character_beta_matrix,
      sys_eq$identities
    ),
    "Model identification error: rank"
  )
})

test_that("model_identification without gamma parameters", {
  equations <-
    "manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population"

  exogenous_variables <- c("world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  expect_true(
    model_identification(
      sys_eq$character_gamma_matrix,
      sys_eq$character_beta_matrix,
      sys_eq$identities
    )
  )
})

test_that("gamma_vectorization works", {
  character_gamma_matrix <- matrix(
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

  identity_weights <-
    list(
      gdp = list(
        equation = "gdp==0.5*manufacturing+0.5*service",
        components = list(manufacturing = "theta6_4", service = "theta6_5"),
        weights = list(theta6_4 = 0.5, theta6_5 = 0.5)
      )
    )

  result <- gamma_vectorization(character_gamma_matrix, identity_weights)

  expected_result <- list(
    transformation_matrix = structure(c(
      0, 0, 0, 0, 0, -1, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0
    ), dim = c(36L, 3L)),
    parameters = c("gamma1_6", "gamma2_6", "gamma5_6"),
    constant_vector = structure(c(
      1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1,
      0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, -0.5, -0.5, 1
    ), dim = c(36L, 1L))
  )

  expect_equal(result, expected_result)
})

test_that("gamma_vectorization when no gamma parameters found", {
  character_gamma_matrix <- matrix(
    c(
      1, 0, 0, 0, 0, 0,
      0, 1, 0, 0, 0, 0,
      0, 0, 1, 0, 0, 0,
      0, 0, 0, 1, 0, "-theta6_4",
      0, 0, 0, 0, 1, "-theta6_5",
      0, 0, 0, 0, 0, 1
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

  identity_weights <- list(gdp = c(theta6_4 = 0.5, theta6_5 = 0.5))

  expect_equal(gamma_vectorization(character_gamma_matrix, identity_weights), NA)
})

test_that("adjust_constant_vector replaces correct values", {
  constant_vector <- structure(c(
    "1", "0", "0", "0", "0", "0", "0", "1", "0", "0",
    "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "1", "0",
    "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "theta6_4",
    "theta6_5", "1"
  ), dim = c(36L, 1L))

  identity_weights <-
    list(
      gdp = list(
        equation = "gdp==0.5*manufacturing+0.5*service",
        components = list(manufacturing = "theta6_4", service = "theta6_5"),
        weights = list(theta6_4 = 0.4, theta6_5 = 0.6)
      )
    )

  result <- adjust_constant_vector(constant_vector, identity_weights)

  expected_result <- structure(
    c(
      1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, -0.4, -0.6, 1
    ),
    dim = c(36L, 1L)
  )

  expect_equal(result, expected_result)
})

test_that("beta_vectorization works", {
  character_beta_matrix <- matrix(
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

  result <- beta_vectorization(character_beta_matrix)

  expected_result <- list(
    transformation_matrix = structure(
      c(
        0, 1, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0
      ),
      dim = c(60L, 10L)
    ),
    parameters = c(
      "beta1_2", "beta1_3", "beta2_4", "beta2_8", "beta3_5",
      "beta3_9", "beta4_6", "beta4_9", "beta5_7", "beta5_10"
    ),
    constant_vector = structure(
      c(
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ),
      dim = c(60L, 1L)
    )
  )

  expect_equal(result, expected_result)
})

test_that("vector_to_matrix", {
  transformation_matrix <- matrix(c(1, 0, 0, 0, 0, 0, 1, 0), nrow = 4, ncol = 2)
  parameters <- c(1, 2)
  constant_vector <- c(0, 3, 0, 4)
  nrow <- 2
  ncol <- 2

  result <- vector_to_matrix(
    transformation_matrix, parameters, constant_vector,
    nrow, ncol
  )

  expected_result <- matrix(c(1, 3, 2, 4), nrow = 2, ncol = 2)

  expect_equal(result, expected_result)
})

test_that("get_parameters works for character_gamma_matrix", {
  character_gamma_matrix <- matrix(
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

  result <- get_parameters(character_gamma_matrix, "gamma")
  expected_result <- c("gamma1_6", "gamma2_6", "gamma5_6")
  expect_equal(result, expected_result)

  result <- get_parameters(character_gamma_matrix, "theta")
  expected_result <- c("theta6_4", "theta6_5")
  expect_equal(result, expected_result)
})

test_that("get_parameters works for character_beta_matrix", {
  character_beta_matrix <- matrix(
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

  result <- get_parameters(character_beta_matrix, "beta")
  expected_result <- c(
    "beta1_2", "beta1_3", "beta2_4", "beta2_8", "beta3_5", "beta3_9",
    "beta4_6", "beta4_9", "beta5_7", "beta5_10"
  )
  expect_equal(result, expected_result)
})

test_that("get_parameters returns empty character vector when no match", {
  character_gamma_matrix <- matrix(
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

  result <- get_parameters(character_gamma_matrix, "beta")
  expected_result <- character(0)

  expect_equal(result, expected_result)
})

test_that("validate_full_rank passes silently for a full column rank matrix", {
  x_matrix <- matrix(
    c(1, 1, 1, 1, 2, 4, 6, 8, 5, 3, 1, 9),
    nrow = 4, dimnames = list(NULL, c("constant", "a", "b"))
  )

  expect_true(koma:::validate_full_rank(x_matrix))
})

test_that("validate_full_rank errors and names the dependent columns", {
  # b is an exact linear combination of a and c: b = 0.5*a + 0.5*c
  a <- c(2, 4, 6, 8, 10)
  c <- c(6, 2, 8, 4, 0)
  b <- 0.5 * a + 0.5 * c

  x_matrix <- matrix(
    c(rep(1, 5), a, b, c),
    nrow = 5, dimnames = list(NULL, c("constant", "a", "b", "c"))
  )

  expect_error(
    koma:::validate_full_rank(x_matrix),
    "not full column rank"
  )

  err <- tryCatch(
    koma:::validate_full_rank(x_matrix),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "a")
  expect_match(conditionMessage(err), "b")
  expect_match(conditionMessage(err), "c")
})

test_that("find_dependent_columns identifies exactly the dependent columns", {
  a <- c(2, 4, 6, 8, 10)
  c <- c(6, 2, 8, 4, 0)
  b <- 0.5 * a + 0.5 * c
  unrelated <- c(1, 3, 5, 7, 11)

  x_matrix <- matrix(
    c(rep(1, 5), a, b, c, unrelated),
    nrow = 5,
    dimnames = list(NULL, c("constant", "a", "b", "c", "unrelated"))
  )

  result <- koma:::find_dependent_columns(x_matrix)
  expect_setequal(result, c("a", "b", "c"))
})

test_that("system_of_equations() eagerly catches a lagged identity that is
collinear with its own components", {
  # gdp is an identity with static (literal numeric) weights, and
  # manufacturing/service (its components) lag themselves elsewhere ->
  # gdp.L(1) is guaranteed to be an exact linear combination of
  # manufacturing.L(1) and service.L(1), which is knowable from the
  # equation specification alone, without any data
  equations <-
    "consumption ~ gdp.L(1) + consumption.L(1),
    investment ~ investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  expect_error(
    system_of_equations(equations, exogenous_variables),
    "exactly collinear"
  )
})

test_that("system_of_equations() does not eagerly flag a lagged identity
with dynamic weights", {
  # gdp's weights are dynamic (data-derived nominal shares): they are not
  # numeric yet at system_of_equations() time (only placeholder names like
  # "nom_manufacturing") and may vary over time once resolved from data, so
  # the symbolic check cannot guarantee the dependency and must skip it.
  equations <-
    "consumption ~ gdp.L(1) + consumption.L(1),
    investment ~ investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population,
    gdp == (nom_manufacturing)*manufacturing + (nom_service)*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)
  expect_true(
    all(!sapply(sys_eq$identities$gdp$weights, is.numeric))
  )
})

test_that("validate_full_rank() remains a safety net for collinearity the
symbolic check cannot cover", {
  skip_on_cran()
  # A plain accidental collinearity unrelated to any identity (e.g. the same
  # series declared twice under different names) is invisible to
  # validate_identity_lag_collinearity() - it only reasons about identities -
  # but is still caught numerically by validate_full_rank() at estimate()
  # time.
  equations <-
    "consumption ~ gdp + consumption.L(1) + world_gdp_duplicate,
    investment ~ investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.5*manufacturing + 0.5*service"

  exogenous_variables <- c(
    "real_interest_rate", "world_gdp", "world_gdp_duplicate", "population"
  )
  sys_eq <- system_of_equations(equations, exogenous_variables)

  ts_data <- simulated_data$ts_data
  ts_data$world_gdp_duplicate <- ts_data$world_gdp
  dates <- list(estimation = list(start = c(1977, 1), end = c(2019, 4)))

  expect_error(
    estimate(ts_data, sys_eq, dates),
    "not full column rank"
  )
})
