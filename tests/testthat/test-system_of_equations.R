test_that("system_of_equations", {
  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
   investment ~ gdp + investment.L(1) + real_interest_rate,
   current_account ~ current_account.L(1) + world_gdp,
   manufacturing ~ manufacturing.L(1) + world_gdp,
   service ~ service.L(1) + population + gdp,
   gdp == 0.6*manufacturing + 0.4*service"
  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  result <- system_of_equations(equations, exogenous_variables)

  expect_identical(class(result), "koma_seq")
  expect_length(result, 12)

  expected_eq <-
    c(
      "consumption~constant+gdp+consumption.L(1)+consumption.L(2)",
      "investment~constant+gdp+investment.L(1)+real_interest_rate",
      "current_account~constant+current_account.L(1)+world_gdp",
      "manufacturing~constant+manufacturing.L(1)+world_gdp",
      "service~constant+service.L(1)+population+gdp",
      "gdp==0.6*manufacturing+0.4*service"
    )
  expect_identical(result$equations, expected_eq)

  expected_endogenous_variables <-
    c(
      "consumption", "investment", "current_account", "manufacturing",
      "service", "gdp"
    )
  expect_identical(
    result$endogenous_variables, expected_endogenous_variables
  )

  expect_identical(
    result$stochastic_equations,
    c(
      "consumption", "investment", "current_account", "manufacturing",
      "service"
    )
  )

  expected_identities <-
    list(gdp = list(
      equation = "gdp==0.6*manufacturing+0.4*service",
      components = list(manufacturing = "theta6_4", service = "theta6_5"),
      weights = list(theta6_4 = 0.6, theta6_5 = 0.4),
      matrix = c("gamma", "gamma")
    ))
  expect_identical(
    result$identities, expected_identities
  )

  expected_char_gamma_matrix <- structure(c(
    "1", "0", "0", "0", "0", "-gamma1_6", "0", "1", "0",
    "0", "0", "-gamma2_6", "0", "0", "1", "0", "0", "0", "0", "0",
    "0", "1", "0", "0", "0", "0", "0", "0", "1", "-gamma5_6", "0",
    "0", "0", "-theta6_4", "-theta6_5", "1"
  ), dim = c(6L, 6L), dimnames = list(
    c(
      "consumption", "investment", "current_account", "manufacturing",
      "service", "gdp"
    ), c(
      "consumption", "investment", "current_account",
      "manufacturing", "service", "gdp"
    )
  ))
  expect_identical(
    result$character_gamma_matrix,
    expected_char_gamma_matrix
  )

  expected_character_beta_matrix <- structure(c(
    "constant1", "beta1_2", "beta1_3", "0", "0", "0",
    "0", "0", "0", "0", "constant2", "0", "0", "beta2_4", "0", "0",
    "0", "beta2_8", "0", "0", "constant3", "0", "0", "0", "beta3_5",
    "0", "0", "0", "beta3_9", "0", "constant4", "0", "0", "0", "0",
    "beta4_6", "0", "0", "beta4_9", "0", "constant5", "0", "0", "0",
    "0", "0", "beta5_7", "0", "0", "beta5_10", "0", "0", "0", "0",
    "0", "0", "0", "0", "0", "0"
  ), dim = c(10L, 6L), dimnames = list(
    c(
      "constant", "consumption.L(1)", "consumption.L(2)",
      "investment.L(1)", "current_account.L(1)", "manufacturing.L(1)",
      "service.L(1)", "real_interest_rate", "world_gdp", "population"
    ), c(
      "consumption", "investment",
      "current_account", "manufacturing", "service", "gdp"
    )
  ))
  expect_identical(result$character_beta_matrix, expected_character_beta_matrix)

  expected_predet_variables <-
    c(
      "consumption.L(1)", "consumption.L(2)", "investment.L(1)",
      "current_account.L(1)", "manufacturing.L(1)", "service.L(1)"
    )
  expect_identical(result$predetermined_variables, expected_predet_variables)

  expected_tot_exo_variables <-
    c(
      "constant", "consumption.L(1)", "consumption.L(2)",
      "investment.L(1)", "current_account.L(1)", "manufacturing.L(1)",
      "service.L(1)", "real_interest_rate", "world_gdp", "population"
    )
  expect_identical(
    result$total_exogenous_variables, expected_tot_exo_variables
  )

  expect_identical(result$exogenous_variables, exogenous_variables)

  # no priors supplied
  expect_equal(result$priors, list(list(), list(), list(), list(), list(), list()))

  result
  print(result)
  format(result)
})

test_that("system_of_equation", {
  # returns without error when already processed equations are given as input
  equations <- c(
    "consumption~constant+gdp+consumption.L(1)+consumption.L(2)",
    "investment~constant+gdp+investment.L(1)+real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturing.L(1)+world_gdp",
    "service~constant+service.L(1)+population+gdp",
    "gdp==0.6*manufacturing+0.4*service"
  )
  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  expect_no_error(system_of_equations(equations, exogenous_variables))
})

test_that("system_of_equation throws error when exogenous not declared", {
  # Case 1
  equations <- c(
    "consumption~constant+gdp+consumption.L(1)+consumption.L(2)",
    "investment~constant+gdp+investment.L(1)+real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturing.L(1)+world_gdp",
    "service~constant+service.L(1)+population+gdp",
    "gdp==0.6*manufacturing+0.4*service"
  )
  exogenous_variables <- c("real_interest_rate", "world_gdp")

  expect_error(
    system_of_equations(equations, exogenous_variables),
    "Undeclared exogenous"
  )

  equations <- c(
    "consumption~constant+gdp+consumption.L(1)+consumption.L(2)",
    "investment~constant+gdp+investment.L(1)+real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturing.L(1)+world_gdp",
    "service~constant+service.L(1)+population+gdp",
    "gdp==0.6*manufacturing+0.4*service+inventory"
  )
  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  expect_error(
    system_of_equations(equations, exogenous_variables),
    "Undeclared exogenous"
  )
})

test_that("system_of_equations", {
  equations <- c(
    "consumption~gdp+consumption.L(1:2)+lag(investment, 2:3)",
    "investment~gdp+investment.L(1)+real_interest_rate",
    "current_account~current_account.L(1)+world_gdp",
    "manufacturing~manufacturing.L(1)+world_gdp",
    "service~service.L(1)+population+gdp",
    "gdp==0.6*manufacturing+0.4*service"
  )
  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  result <- system_of_equations(equations, exogenous_variables)

  expected_equations <- c(
    "consumption~constant+gdp+consumption.L(1)+consumption.L(2)+investment.L(2)+investment.L(3)",
    "investment~constant+gdp+investment.L(1)+real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturing.L(1)+world_gdp",
    "service~constant+service.L(1)+population+gdp",
    "gdp==0.6*manufacturing+0.4*service"
  )

  expect_equal(result$equations, expected_equations)
})

test_that("get_variables", {
  equations <- c(
    "consumption~constant+gdp+consumption.L(1)+42*consumption.L(2)",
    "investment~constant+gdp+investment.L(1)-real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "gdp==manufacturing+service"
  )
  result <- get_variables(equations)
  expected_result <- list(
    `consumption~constant+gdp+consumption.L(1)+42*consumption.L(2)` =
      c(
        "consumption",
        "constant", "gdp", "consumption.L(1)", "consumption.L(2)"
      ),
    `investment~constant+gdp+investment.L(1)-real_interest_rate` = c(
      "investment",
      "constant", "gdp", "investment.L(1)", "real_interest_rate"
    ),
    `current_account~constant+current_account.L(1)+world_gdp` = c(
      "current_account",
      "constant", "current_account.L(1)", "world_gdp"
    ),
    `gdp==manufacturing+service` = c(
      "gdp",
      "manufacturing", "service"
    )
  )

  expect_identical(result, expected_result)
})

test_that("system_of_equations with priors", {
  equations <-
    "consumption ~ {0,1000}1 + {0.4,0.1}gdp + {0.1,0.2}service +{0.9,10}consumption.L(1) + {0, 1000}consumption.L(2) + {3,0.001},
   investment ~ gdp + investment.L(1) + real_interest_rate,
   current_account ~ current_account.L(1) + world_gdp,
   manufacturing ~ manufacturing.L(1) + world_gdp,
   service ~ service.L(1) + population + gdp,
   gdp == 0.6*manufacturing + 0.4*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  result <- system_of_equations(equations, exogenous_variables)

  expected_priors <- list(
    list(
      constant = list(0, 1000),
      gdp = list(0.4, 0.1),
      service = list(0.1, 0.2),
      "consumption.L(1)" = list(0.9, 10),
      "consumption.L(2)" = list(0, 1000),
      epsilon = list(3, 0.001)
    ), list(), list(), list(), list(), list()
  )

  expect_equal(result$priors, expected_priors)
})

test_that("system_of_equations with no gamma parameters", {
  equations <-
    "manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population"
  exogenous_variables <- c("world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  expected_stochastic_equations <- c("manufacturing", "service")
  expect_equal(sys_eq$stochastic_equations, expected_stochastic_equations)
})


test_that("system_of_equations, AR(1) model", {
  # Case: AR(1) model
  equations <- "manufacturing ~ manufacturing.L(1) - 1"
  exogenous_variables <- c()

  sys_eq <- system_of_equations(equations, exogenous_variables)

  expected_character_beta_matrix <- structure(
    c("0", "beta1_2"),
    dim = 2:1,
    dimnames = list(c("constant", "manufacturing.L(1)"), "manufacturing")
  )

  expected_character_gamma_matrix <- structure(
    1,
    dim = c(1L, 1L),
    dimnames = list("manufacturing", "manufacturing")
  )

  expect_identical(sys_eq$character_beta_matrix, expected_character_beta_matrix)
  expect_identical(sys_eq$character_gamma_matrix, expected_character_gamma_matrix)
})

test_that("system_of_equations, with equation specific settings", {
  equations <- c(
    "consumption~gdp+consumption.L(1:2)+lag(investment, 2:3),
    investment~gdp+investment.L(1)+real_interest_rate [tau = 0.1, ndraws = 100],
    current_account~current_account.L(1)+world_gdp,
    manufacturing~manufacturing.L(1)+world_gdp,
    service~service.L(1)+population+gdp,
    gdp==0.6*manufacturing+0.4*service"
  )
  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  result <- system_of_equations(equations, exogenous_variables)

  expected_equation_settings <- list(
    consumption = list(),
    investment = list(tau = 0.1, ndraws = 100),
    current_account = list(),
    manufacturing = list(),
    service = list(),
    gdp = list()
  )

  expect_equal(result$equation_settings, expected_equation_settings)
})

test_that("parse_equation, with intercept", {
  expected_result <- "consumption~constant+gdp+consumption.L(1)+consumption.L(2)"

  # implicit intercept
  equation <- "consumption~gdp+consumption.L(1)+consumption.L(2)"
  result <- parse_equation(equation)
  expect_equal(result, expected_result)

  # explicit intercept (1)
  equation <- "consumption~gdp+consumption.L(1)+1+consumption.L(2)"
  result <- parse_equation(equation)
  expect_equal(result, expected_result)

  # explicit intercept (constant)
  equation <- "consumption~gdp+consumption.L(1)+constant+consumption.L(2)"
  result <- parse_equation(equation)
  expect_equal(result, expected_result)

  # explicit leading intercept
  equation <- "consumption~1+gdp+consumption.L(1)+consumption.L(2)"
  result <- parse_equation(equation)
  expect_equal(result, expected_result)

  # explicit trailing intercept
  equation <- "consumption~gdp+consumption.L(1)+consumption.L(2)+1"
  result <- parse_equation(equation)
  expect_equal(result, expected_result)
})

test_that("parse_equation, with interept and priors", {
  expected_result <- "consumption~constant+gdp+consumption.L(1)+consumption.L(2)"

  equation <- "consumption~{0,1}1+{1,0.1}gdp+consumption.L(1)+consumption.L(2)+{3,0.001}"
  result <- parse_equation(equation)
  expect_equal(result, expected_result)
})

test_that("parse_equation, without intercept", {
  expected_result <- "consumption~gdp+consumption.L(1)+consumption.L(2)"

  # without intercept, 0
  equation <- "consumption~gdp+0+consumption.L(1)+consumption.L(2)"
  result <- parse_equation(equation)
  expect_equal(result, expected_result)

  # leading, without intercept, 0
  equation <- "consumption~0+gdp+consumption.L(1)+consumption.L(2)"
  result <- parse_equation(equation)
  expect_equal(result, expected_result)

  # trailing, without intercept, 0
  equation <- "consumption~gdp+consumption.L(1)+consumption.L(2)+0"
  result <- parse_equation(equation)
  expect_equal(result, expected_result)

  # without intercept, -1
  equation <- "consumption~gdp-1+consumption.L(1)+consumption.L(2)"
  result <- parse_equation(equation)
  expect_equal(result, expected_result)

  # leading, without intercept, -1
  equation <- "consumption~-1+gdp+consumption.L(1)+consumption.L(2)"
  result <- parse_equation(equation)
  expect_equal(result, expected_result)

  # trailing, without intercept, -1
  equation <- "consumption~gdp+consumption.L(1)+consumption.L(2)-1"
  result <- parse_equation(equation)
  expect_equal(result, expected_result)
})

test_that("parse_lags", {
  equations <- c(
    "consumption~constant+gdp+consumption.L(1:2)",
    "investment~constant+gdp+lag(investment,1:2)+real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturingL1+world_gdp",
    "service~constant+lag(service,1)+population+gdp",
    "gdp==0.5*manufacturing+0.5*service"
  )
  out <- parse_lags(equations)

  expected_variables <- c(
    "consumption.L(1)",
    "consumption.L(2)",
    "current_account.L(1)",
    "investment.L(1)",
    "investment.L(2)",
    "service.L(1)"
  )
  expected_equations <- c(
    "consumption~constant+gdp+consumption.L(1)+consumption.L(2)",
    "investment~constant+gdp+investment.L(1)+investment.L(2)+real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturingL1+world_gdp",
    "service~constant+service.L(1)+population+gdp",
    "gdp==0.5*manufacturing+0.5*service"
  )

  expect_equal(out$variables, expected_variables)
  expect_equal(out$equations, expected_equations)
})

test_that("parse_lags", {
  equations <- c(
    "consumption~constant+gdp+consumption.L(1)+consumption.L(2)",
    "investment~constant+gdp+investment.L(1)+real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp", "manufacturing~constant+manufacturing.L(1)+world_gdp",
    "service~constant+service.L(1)+population+gdp", "gdp==0.6*manufacturing+0.4*service"
  )

  out <- parse_lags(equations)

  expected_variables <- c(
    "consumption.L(1)",
    "consumption.L(2)",
    "investment.L(1)",
    "current_account.L(1)",
    "manufacturing.L(1)",
    "service.L(1)"
  )
  expected_equations <- c(
    "consumption~constant+gdp+consumption.L(1)+consumption.L(2)",
    "investment~constant+gdp+investment.L(1)+real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturing.L(1)+world_gdp",
    "service~constant+service.L(1)+population+gdp",
    "gdp==0.6*manufacturing+0.4*service"
  )

  expect_equal(out$variables, expected_variables)
  expect_equal(out$equations, expected_equations)
})

test_that("extract_priors", {
  equation <- "consumption~{.1,1000}1+{-.4,.1}gdp+{.9,10}consumption.L(1)+{0,1000}consumption.L(2)+{4,.002}"

  result <- extract_priors(equation)

  expected_result <- list(
    constant = list(0.1, 1000),
    gdp = list(-0.4, 0.1),
    "consumption.L(1)" = list(0.9, 10),
    "consumption.L(2)" = list(0, 1000),
    epsilon = list(4, 0.002)
  )

  expect_equal(result, expected_result)
})

test_that("extract_priors, removes invalid priors", {
  expected_result <- list(
    gdp = list(0.4, 0.1),
    "consumption.L(1)" = list(0.9, 10),
    "consumption.L(2)" = list(0, 1000)
  )

  # invalid prior for 0
  equation <- "consumption~{.1,1000}0+{.4,.1}gdp+{.9,10}consumption.L(1)+{0,1000}consumption.L(2)+{4,.002}"
  expect_warning(extract_priors(equation))
  suppressWarnings(
    result <- extract_priors(equation)
  )
  expect_equal(result, expected_result)

  # error term prior not at end
  equation <- "consumption~{.4,.1}gdp+{.9,10}consumption.L(1)+{4,.002}+{0,1000}consumption.L(2)"
  expect_warning(extract_priors(equation))
  suppressWarnings(
    result <- extract_priors(equation)
  )
  expect_equal(result, expected_result)
})

test_that("extract_priors works when no priors supplied", {
  equation <- "consumption~gdp+consumption.L(1)+consumption.L(2)"

  result <- extract_priors(equation)
  expected_result <- list()
  expect_equal(result, expected_result)
})

test_that("get_endogenous_variables returns the correct endogenous variables", {
  # Test case 1
  equations_1 <- c("x == y", "y == z")
  expected_1 <- c("x", "y")
  result_1 <- get_endogenous_variables(equations_1)
  expect_equal(result_1, expected_1)

  # Test case 2: Identity equation present
  equations_2 <- c(
    "(nom_x)*x == (nom_a)*a + (nom_b)*b",
    "y == 1*c - 1*d",
    "z == e * f"
  )
  expected_2 <- c("x", "y", "z")
  result_2 <- get_endogenous_variables(equations_2)
  expect_equal(result_2, expected_2)
})

test_that("extract_endogenous_variables returns the correct endogenous variables", {
  # --- Test case 1: Simple identity equations ---
  equations_1 <- c(
    "x == y",
    "y == z"
  )
  expected_1 <- c("x", "y")
  result_1 <- extract_endogenous_variables(equations_1)
  expect_equal(result_1, expected_1)

  # --- Test case 2: Identity with weights and injected parameters ---
  equations_2 <- c(
    "(nom_x)*x == (nom_a)*a + (nom_b)*b",
    "y == 1*c - 1*d",
    "z == e * f" # stochastic-looking but still identity
  )
  expected_2 <- c("x", "y", "z")
  result_2 <- extract_endogenous_variables(equations_2)
  expect_equal(result_2, expected_2)

  # --- Test case 3: New stochastic notation with ~ and new lag syntax ---
  equations_3 <- c(
    "consumption ~ constant + gdp + consumption.L(1)",
    "investment ~ constant + gdp + investment.L(1) + real_interest_rate"
  )
  expected_3 <- c("consumption", "investment")
  result_3 <- extract_endogenous_variables(equations_3)
  expect_equal(result_3, expected_3)

  # --- Test case 4: Mixed system with identity and stochastic ---
  equations_4 <- c(
    "gdp == (alpha)*consumption + (1 - alpha)*investment",
    "consumption ~ constant + gdp + consumption.L(1)"
  )
  expected_4 <- c("gdp", "consumption")
  result_4 <- extract_endogenous_variables(equations_4)
  expect_equal(result_4, expected_4)

  # --- Test case 5: Mixed system with stochastic, identity, and injected params ---
  equations_5 <- c(
    "inflation ~ constant + output_gap + inflation.L(1)",
    "output_gap ~ constant + real_interest_rate",
    "nominal_gdp == (price_level) * real_gdp"
  )
  expected_5 <- c("inflation", "output_gap", "nominal_gdp")
  result_5 <- extract_endogenous_variables(equations_5)
  expect_equal(result_5, expected_5)

  # --- Test case 6: Redundant definitions (should return unique vars) ---
  equations_6 <- c(
    "x ~ y + z",
    "x == (param)*a + (1-param)*b"
  )
  expected_6 <- c("x")
  result_6 <- extract_endogenous_variables(equations_6)
  expect_equal(result_6, expected_6)

  # --- Test case 7: Complex identities with multi-level weights ---
  equations_7 <- c(
    "(nom_x)*x == (param1)*(a + b) + (param2)*c",
    "y == (lambda) * z + (1 - lambda) * t"
  )
  expected_7 <- c("x", "y")
  result_7 <- extract_endogenous_variables(equations_7)
  expect_equal(result_7, expected_7)
})

test_that("get_identities", {
  equations <- c(
    "consumption~gdp+consumption.L(1)+consumption.L(2)",
    "investment~gdp+investment.L(1)+real_interest_rate",
    "current_account~current_account.L(1)+world_gdp",
    "manufacturing~manufacturing.L(1)+world_gdp",
    "service~service.L(1)+population+gdp",
    "gdp==(nom_manufacturing/nom_gdp)*manufacturing+(nom_service/nom_gdp)*service+1*current_account.L(1)",
    "real_interest_rate==1*nominal_interest_rate-1*inflation_rate"
  )
  exogenous_variables <- c("nominal_interest_rate", "inflation_rate", "population", "world_gdp")

  character_gamma_matrix <-
    structure(c(
      "1", "0", "0", "0", "0", "-gamma1_6", "0", "0", "1",
      "0", "0", "0", "-gamma2_6", "-gamma2_7", "0", "0", "1", "0",
      "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0",
      "0", "1", "-gamma5_6", "0", "0", "0", "0", "-theta6_4",
      "-theta6_5", "1", "0", "0", "0", "0", "0", "0", "0", "1"
    ), dim = c(7L, 7L), dimnames = list(c(
      "consumption", "investment", "current_account",
      "manufacturing", "service", "gdp", "real_interest_rate"
    ), c(
      "consumption",
      "investment", "current_account", "manufacturing", "service",
      "gdp", "real_interest_rate"
    )))

  character_beta_matrix <-
    structure(c(
      "constant1", "beta1_2", "beta1_3", "0", "0", "0",
      "0", "0", "0", "0", "0", "constant2", "0", "0", "beta2_4", "0",
      "0", "0", "0", "0", "0", "0", "constant3", "0", "0", "0", "beta3_5",
      "0", "0", "0", "0", "0", "beta3_11", "constant4", "0", "0", "0",
      "0", "beta4_6", "0", "0", "0", "0", "beta4_11", "constant5",
      "0", "0", "0", "0", "0", "beta5_7", "0", "0", "beta5_10", "0",
      "0", "0", "0", "0", "theta6_5", "0", "0", "0", "0", "0", "0",
      "0", "0", "0", "0", "0", "0", "0", "theta7_8", "theta7_9", "0",
      "0"
    ), dim = c(11L, 7L), dimnames = list(c(
      "constant", "consumption.L(1)",
      "consumption.L(2)", "investment.L(1)", "current_account.L(1)",
      "manufacturing.L(1)", "service.L(1)", "nominal_interest_rate",
      "inflation_rate", "population", "world_gdp"
    ), c(
      "consumption",
      "investment", "current_account", "manufacturing", "service",
      "gdp", "real_interest_rate"
    )))

  result <- get_identities(
    equations, character_gamma_matrix, character_beta_matrix
  )

  expected_identities <- list(
    gdp = list(
      equation = "gdp==(nom_manufacturing/nom_gdp)*manufacturing+(nom_service/nom_gdp)*service+1*current_account.L(1)",
      components = list(
        manufacturing = "theta6_4",
        service = "theta6_5",
        `current_account.L(1)` = "theta6_5"
      ),
      weights = list(
        theta6_4 = "nom_manufacturing/nom_gdp",
        theta6_5 = "nom_service/nom_gdp",
        theta6_5 = 1
      ),
      matrix = c("gamma", "gamma", "beta")
    ),
    real_interest_rate = list(
      equation =
        "real_interest_rate==1*nominal_interest_rate-1*inflation_rate",
      components = list(
        nominal_interest_rate = "theta7_8", inflation_rate = "theta7_9"
      ),
      weights = list(
        theta7_8 = 1,
        theta7_9 = -1
      ),
      matrix = c("beta", "beta")
    )
  )

  expect_identical(result, expected_identities)
})

test_that("get_identities with lags", {
  equations <- c(
    "consumption~gdp+consumption.L(1)+disposable_income_level.L(1)+consumption_level.L(1)",
    "investment~gdp+investment.L(1)+real_interest_rate",
    "current_account~current_account.L(1)+world_gdp",
    "manufacturing~manufacturing.L(1)+world_gdp",
    "service~service.L(1)+population+gdp",
    "gdp==0.6*manufacturing+0.4*service",
    "disposable_income_level==1*disposable_income+1*disposable_income_level.L(1)",
    "consumption_level==1*consumption+1*consumption_level.L(1)"
  )

  character_beta_matrix <- structure(c(
    "constant1", "beta1_2", "beta1_3", "beta1_4", "0",
    "0", "0", "0", "0", "0", "0", "0", "constant2", "0", "0", "0",
    "beta2_5", "0", "0", "0", "beta2_9", "0", "0", "0", "constant3",
    "0", "0", "0", "0", "beta3_6", "0", "0", "0", "beta3_10", "0",
    "0", "constant4", "0", "0", "0", "0", "0", "beta4_7", "0", "0",
    "beta4_10", "0", "0", "constant5", "0", "0", "0", "0", "0", "0",
    "beta5_8", "0", "0", "beta5_11", "0", "0", "0", "0", "0", "0",
    "0", "0", "0", "0", "0", "0", "0", "0", "0", "theta7_3", "0",
    "0", "0", "0", "0", "0", "0", "0", "theta7_12", "0", "0", "0",
    "theta8_4", "0", "0", "0", "0", "0", "0", "0", "0"
  ), dim = c(12L, 8L), dimnames = list(c(
    "constant", "consumption.L(1)", "disposable_income_level.L(1)",
    "consumption_level.L(1)", "investment.L(1)", "current_account.L(1)",
    "manufacturing.L(1)", "service.L(1)", "real_interest_rate", "world_gdp",
    "population", "disposable_income"
  ), c(
    "consumption", "investment", "current_account",
    "manufacturing", "service", "gdp", "disposable_income_level",
    "consumption_level"
  )))

  character_gamma_matrix <- structure(c(
    "1", "0", "0", "0", "0", "-gamma1_6", "0", "0", "0",
    "1", "0", "0", "0", "-gamma2_6", "0", "0", "0", "0", "1", "0",
    "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0",
    "0", "0", "0", "1", "-gamma5_6", "0", "0", "0", "0", "0", "-theta6_4",
    "-theta6_5", "1", "0", "0", "0", "0", "0", "0", "0", "0", "1",
    "0", "-theta8_1", "0", "0", "0", "0", "0", "0", "1"
  ), dim = c(8L, 8L), dimnames = list(c(
    "consumption", "investment", "current_account",
    "manufacturing", "service", "gdp", "disposable_income_level",
    "consumption_level"
  ), c(
    "consumption", "investment", "current_account",
    "manufacturing", "service", "gdp", "disposable_income_level",
    "consumption_level"
  )))

  result <- get_identities(
    equations, character_gamma_matrix, character_beta_matrix
  )

  expected_identities <- list(
    gdp = list(
      equation = "gdp==0.6*manufacturing+0.4*service",
      components = list(manufacturing = "theta6_4", service = "theta6_5"),
      weights = list(theta6_4 = 0.6, theta6_5 = 0.4),
      matrix = c("gamma", "gamma")
    ),
    disposable_income_level = list(
      equation = "disposable_income_level==1*disposable_income+1*disposable_income_level.L(1)",
      components = list(
        disposable_income = "theta7_12",
        "disposable_income_level.L(1)" = "theta7_3"
      ),
      weights = list(theta7_12 = 1, theta7_3 = 1),
      matrix = c("beta", "beta")
    ),
    consumption_level = list(
      equation = "consumption_level==1*consumption+1*consumption_level.L(1)",
      components = list(
        consumption = "theta8_1",
        "consumption_level.L(1)" = "theta8_4"
      ),
      weights = list(theta8_1 = 1, theta8_4 = 1),
      matrix = c("gamma", "beta")
    )
  )

  expect_identical(result, expected_identities)
})

test_that("get_weights", {
  eq <- "gdp==(nom_manufacturing/nom_gdp)*manufacturing+(nom_service/nom_gdp)*service"
  result <- get_weights(eq)

  expected_result <- list(gdp = list(
    equation = "gdp==(nom_manufacturing/nom_gdp)*manufacturing+(nom_service/nom_gdp)*service",
    components = list(manufacturing = NULL, service = NULL),
    weights = list("nom_manufacturing/nom_gdp", "nom_service/nom_gdp")
  ))

  expect_equal(result, expected_result)
})

test_that("get_weights", {
  eq <- c(
    "gdp==(0.3)*manufacturing+(nom_service/nom_gdp)*service"
  )
  result <- get_weights(eq)

  expected_result <- list(gdp = list(
    equation = "gdp==(0.3)*manufacturing+(nom_service/nom_gdp)*service",
    components = list(manufacturing = NULL, service = NULL),
    weights = list(0.3, "nom_service/nom_gdp")
  ))

  expect_equal(result, expected_result)
})

test_that("get_weights", {
  eq <- c(
    "gdp==0.3*manufacturing+(nom_service/nom_gdp)*service"
  )
  result <- get_weights(eq)

  expected_result <- list(gdp = list(
    equation = "gdp==0.3*manufacturing+(nom_service/nom_gdp)*service",
    components = list(manufacturing = NULL, service = NULL),
    weights = list(0.3, "nom_service/nom_gdp")
  ))

  expect_equal(result, expected_result)
})

test_that("get_weights", {
  eq <- c(
    "real_interest_rate==1*nominal_interest_rate-1*inflation_rate"
  )
  result <- get_weights(eq)

  expected_result <- list(real_interest_rate = list(
    equation = "real_interest_rate==1*nominal_interest_rate-1*inflation_rate",
    components = list(nominal_interest_rate = NULL, inflation_rate = NULL),
    weights = list(1, -1)
  ))

  expect_equal(result, expected_result)
})

test_that("get_weights with lags", {
  eq <- c(
    "x==1*y-1*x.L(1)"
  )
  result <- get_weights(eq)

  expected_result <- list(x = list(
    equation = "x==1*y-1*x.L(1)",
    components = list(y = NULL, "x.L(1)" = NULL),
    weights = list(1, -1)
  ))

  expect_equal(result, expected_result)
})

test_that("get_weight_variables", {
  equations <- c(
    "gdp==0.3*manufacturing+(nom_service/nom_gdp)*service",
    "gdp==(nom_manufacturing*3)*manufacturing+(-3.8+.1)*service"
  )
  result <- get_weight_variables(equations)

  expected_result <- c("nom_gdp", "nom_manufacturing", "nom_service")

  expect_equal(result, expected_result)
})

test_that("parse_lags", {
  equations <- c(
    "a~y+x.L(10:12)+lag(x, 2:3)",
    "b~lag(y,2:4)"
  )

  out <- parse_lags(equations)

  expected_vars <- c(
    "x.L(10)", "x.L(11)", "x.L(12)", "x.L(2)", "x.L(3)",
    "y.L(2)", "y.L(3)", "y.L(4)"
  )
  expect_equal(sort(out$variables), sort(expected_vars))

  expected_equations <- c(
    "a~y+x.L(10)+x.L(11)+x.L(12)+x.L(2)+x.L(3)",
    "b~y.L(2)+y.L(3)+y.L(4)"
  )
  expect_equal(out$equations, expected_equations)
})
test_that("extract_lagged_vars returns the correct lagged variables", {
  # Define patterns for L() and lag()
  # var_pos captures the group that contains the variable name
  # lag_spec_pos captures the group that contains the lag specification
  patterns <- list(
    list(
      regex = "([a-zA-Z0-9_]+)\\.L\\(([^\\)]+)\\)",
      var_pos = 1, lag_spec_pos = 2
    ),
    list(
      regex = "lag\\(([a-zA-Z0-9_\\.]+),\\s*([^)]+)\\)",
      var_pos = 1, lag_spec_pos = 2
    )
  )

  # --- Test case: Simple lags in new syntax ---
  equations <- c(
    "w==a+b",
    "x~x.L(1)",
    "y~y.L(1)+x.L(2)",
    "z~z.L(2)+c.L(1)"
  )
  expected <- c("c.L(1)", "x.L(1)", "x.L(2)", "y.L(1)", "z.L(2)")

  result <- extract_lagged_vars(equations, patterns[[1]])
  expect_equal(sort(result$variables), sort(expected))

  # --- Test case: lag() function notation ---
  equations <- c(
    "x ~ lag(x, 1)",
    "y ~ lag(y, 1) + lag(x, 1)"
  )
  expected <- c("x.L(1)", "y.L(1)")
  result <- extract_lagged_vars(equations, patterns[[2]])
  expect_equal(sort(result$variables), sort(expected))

  # --- Test case: Variables with numbers and lags ---
  equations <- c(
    "x1 ~ x1.L(3)",
    "y2 ~ lag(y2, 1) + lag(x, 1)"
  )
  expected <- c("x.L(1)", "y2.L(1)")
  result <- extract_lagged_vars(equations, patterns[[2]])
  expect_equal(sort(result$variables), sort(expected))

  # --- Test case: Lag ranges x.L(1:3) and lag(x, 2:4) ---
  equations <- c(
    "a ~ y + x.L(10:12) + lag(x, 2:3)",
    "b ~ lag(y, 2:4)"
  )
  expected <- c(
    "x.L(10)", "x.L(11)", "x.L(12)", "x.L(2)", "x.L(3)", "y.L(2)",
    "y.L(3)", "y.L(4)"
  )
  result <- c(
    extract_lagged_vars(equations, patterns[[1]])[["variables"]],
    extract_lagged_vars(equations, patterns[[2]])[["variables"]]
  )
  expect_equal(sort(result), sort(expected))

  # --- Test case: Mixed notation in same equation ---
  equations <- c("c ~ x.L(1) + lag(x, 2) + x.L(3:4) + lag(y, 1:2)")
  expected <- c("x.L(1)", "x.L(2)", "x.L(3)", "x.L(4)", "y.L(1)", "y.L(2)")
  result <- c(
    extract_lagged_vars(equations, patterns[[1]])[["variables"]],
    extract_lagged_vars(equations, patterns[[2]])[["variables"]]
  )
  expect_equal(sort(result), sort(expected))

  # --- Test case: Multiple lags and ranges in x.L() and lag() ---
  equations <- c("u ~ x.L(1:3, 5) + lag(y, 2:4, 6)")
  expected <- c(
    "x.L(1)", "x.L(2)", "x.L(3)", "x.L(5)",
    "y.L(2)", "y.L(3)", "y.L(4)", "y.L(6)"
  )
  result <- c(
    extract_lagged_vars(equations, patterns[[1]])[["variables"]],
    extract_lagged_vars(equations, patterns[[2]])[["variables"]]
  )
  expect_equal(sort(result), sort(expected))
})

test_that("no settings yields empty list", {
  equation <- "y~x1+x2"
  expect_equal(extract_settings(equation), list())
})

test_that("extract_settings, single setting parsed correctly", {
  equation <- "y~x1[x=0.5]"
  expect_equal(extract_settings(equation), list(x = 0.5))
})

test_that("extract_settings, single setting parsed correctly", {
  equation <- "y~x1[x=c(0.5, 3)]"
  expect_equal(extract_settings(equation), list(x = c(0.5, 3)))
})

test_that("extract_settings, multiple settings parsed correctly", {
  equation <- "y~x1[x=0.5,y=2]"
  expect_equal(
    extract_settings(equation),
    list(x = 0.5, y = 2)
  )
})

test_that("extract_settings, trailing whitespace ignored", {
  equation <- "y~x1[x=0.3]   "
  expect_equal(extract_settings(equation), list(x = 0.3))
})

test_that("extract_settings, trailing whitespace ignored", {
  equation <- "[x=0.3]y~x1"
  expect_equal(extract_settings(equation), list(x = 0.3))
})

test_that("extract_settings, lists", {
  equation <- "y~x1[x=list(a=0.3, b=0.4)]"
  expect_equal(extract_settings(equation), list(x = list(a = 0.3, b = 0.4)))
})

test_that("extract_settings, strings", {
  equation <- "y~x1[a='foo',b=1]"
  res <- extract_settings(equation)
  expect_equal(res, list(a = "foo", b = 1))
})

test_that("extract_settings, not parsable throws error", {
  equation <- "y~x1[a=foo,b=1]"
  expect_error(extract_settings(equation))
})
