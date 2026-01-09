test_that("validate_system_of_equations processes valid equations correctly", {
  equations <- c(
    "consumption~constant+gdp+consumption.L(1)+42*consumption.L(2)",
    "investment~constant+gdp+investment.L(1)-real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturing.L(1)+world_gdp",
    "service~constant+service.L(1)+population+gdp",
    "gdp==0.5*manufacturing+0.5*service",
    "real_interest_rate==1*nominal_interest_rate-1*inflation_rate"
  )
  endogenous_variables <- c(
    "consumption", "investment", "current_account", "gdp", "manufacturing",
    "service", "real_interest_rate"
  )
  exogenous_variables <- c(
    "world_gdp", "nominal_interest_rate", "inflation_rate", "population"
  )
  predetermined_variables <- c(
    "consumption.L(1)", "consumption.L(2)", "investment.L(1)", "current_account.L(1)",
    "manufacturing.L(1)", "service.L(1)", "population"
  )

  object <- structure(
    list(
      equations = equations,
      endogenous_variables = endogenous_variables,
      exogenous_variables = exogenous_variables,
      predetermined_variables = predetermined_variables
    ),
    class = "koma_seq"
  )

  expect_no_error(validate_system_of_equations(object))
  expect_no_error(validate_completeness(equations, exogenous_variables))
})

test_that("validate_system_of_equations processes valid equations with priors
correctly", {
  equations <- c(
    "consumption~{0,1000}constant+{0.4,0.1}gdp+{0.9,10}consumption.L(1)+{0,1000}consumption.L(2)+{3,0.001}",
    "investment~constant+gdp+investment.L(1)-real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturing.L(1)+world_gdp",
    "service~constant+service.L(1)+population+gdp",
    "gdp==0.5*manufacturing+0.5*service",
    "real_interest_rate==1*nominal_interest_rate-1*inflation_rate"
  )
  endogenous_variables <- c(
    "consumption", "investment", "current_account", "gdp", "manufacturing",
    "service", "real_interest_rate"
  )
  exogenous_variables <- c(
    "world_gdp", "nominal_interest_rate", "inflation_rate", "population"
  )
  predetermined_variables <- c(
    "consumption.L(1)", "consumption.L(2)", "investment.L(1)", "current_account.L(1)",
    "manufacturing.L(1)", "service.L(1)", "population"
  )

  object <- structure(
    list(
      equations = equations,
      endogenous_variables = endogenous_variables,
      exogenous_variables = exogenous_variables,
      predetermined_variables = predetermined_variables
    ),
    class = "koma_seq"
  )

  expect_no_error(validate_system_of_equations(object))
  expect_no_error(validate_completeness(equations, exogenous_variables))
})

test_that("validate_completeness throws error on undeclared exogenous", {
  equations <- c(
    "consumption~constant+gdp+consumption.L(1)+42*consumption.L(2)",
    "investment~constant+gdp+investment.L(1)-real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturing.L(1)+world_gdp",
    "service~constant+service.L(1)+population+gdp",
    "gdp==manufacturing+service",
    "real_interest_rate==1*nominal_interest_rate-1*inflation_rate"
  )
  exogenous_variables <- c(
    "world_gdp", "nominal_interest_rate"
  )

  # undeclared exogenous: "population", "inflation_rate"
  expect_error(
    validate_completeness(equations, exogenous_variables),
    "Undeclared exogenous variables"
  )
})

test_that("validate_completeness throws error on redundantly specified exogenous", {
  equations <- c(
    "consumption~constant+gdp+consumption.L(1)+42*consumption.L(2)",
    "investment~constant+gdp+investment.L(1)-real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturing.L(1)+world_gdp",
    "service~constant+service.L(1)+population+gdp",
    "gdp==manufacturing+service",
    "real_interest_rate==1*nominal_interest_rate-1*inflation_rate"
  )
  exogenous_variables <- c(
    "world_gdp", "nominal_interest_rate", "population", "inflation_rate",
    "not_in_system"
  )

  # undeclared exogenous: "population", "inflation_rate"
  expect_error(
    validate_completeness(equations, exogenous_variables),
    "Redundant exogenous"
  )
})

test_that("validate_system_of_equations does not throw error with correctly
defined weights", {
  equations <- c(
    "consumption~constant+gdp+consumption.L(1)+consumption.L(2)",
    "investment~constant+gdp+investment.L(1)+real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturing.L(1)+world_gdp",
    "service~constant+service.L(1)+population+gdp",
    "gdp==(nom_manufacturing/nom_gdp)*manufacturing+(nom_service/nom_gdp)*service",
    "real_interest_rate==1*nominal_interest_rate-1*inflation_rate"
  )

  # Vector of exogenous variables
  exogenous_variables <- c(
    "nominal_interest_rate", "inflation_rate",
    "world_gdp", "population"
  )

  endogenous_variables <- c(
    "consumption", "investment", "current_account", "manufacturing",
    "service", "gdp", "real_interest_rate"
  )

  predetermined_variables <- c(
    "consumption.L(1)", "consumption.L(2)", "investment.L(1)", "current_account.L(1)",
    "manufacturing.L(1)", "service.L(1)"
  )

  object <- structure(
    list(
      equations = equations,
      endogenous_variables = endogenous_variables,
      exogenous_variables = exogenous_variables,
      predetermined_variables = predetermined_variables
    ),
    class = "koma_seq"
  )

  expect_no_error(validate_system_of_equations(object))
})

test_that("validate_system_of_equations variable cannot be endogenous aswell as
 exogenous", {
  equations <- c(
    "consumption~constant+gdp+consumption.L(1)+42*consumption.L(2)",
    "investment~constant+gdp+investment.L(1)-real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturing.L(1)+world_gdp",
    "service~constant+service.L(1)+population+gdp",
    "gdp==0.5*manufacturing+0.5*service",
    "real_interest_rate==1*nominal_interest_rate-1*inflation_rate"
  )
  endogenous_variables <- c(
    "consumption", "investment", "current_account", "gdp", "manufacturing",
    "service", "real_interest_rate"
  )
  exogenous_variables <- c(
    "real_interest_rate", "world_gdp", "nominal_interest_rate", "inflation_rate"
  )
  predetermined_variables <- c(
    "consumption.L(1)", "consumption.L(2)", "investment.L(1)", "current_account.L(1)",
    "manufacturing.L(1)", "service.L(1)", "population"
  )

  object <- structure(
    list(
      equations = equations,
      endogenous_variables = endogenous_variables,
      exogenous_variables = exogenous_variables,
      predetermined_variables = predetermined_variables
    ),
    class = "koma_seq"
  )
  expect_error(
    validate_system_of_equations(object),
    "An exogenous variable cannot also be"
  )
})

test_that("validate_system_of_equations throws error when no stochastic equation
in SEM", {
  equations <- c(
    "gdp==manufacturing+service",
    "real_interest_rate==1*nominal_interest_rate-1*inflation_rate"
  )
  endogenous_variables <- c(
    "gdp", "real_interest_rate"
  )
  exogenous_variables <- c(
    "manufacturing", "service", "nominal_interest_rate", "inflation_rate"
  )
  predetermined_variables <- c()

  object <- structure(
    list(
      equations = equations,
      endogenous_variables = endogenous_variables,
      exogenous_variables = exogenous_variables,
      predetermined_variables = predetermined_variables
    ),
    class = "koma_seq"
  )
  expect_error(
    validate_system_of_equations(object),
    "No stochastic equation detected"
  )
})

test_that("validate_system_of_equations throws an error when endogenous or
exogenous variables not unique", {
  equations <- c(
    "consumption~constant+gdp+consumption.L(1)+42*consumption.L(2)",
    "investment~constant+gdp+investment.L(1)-real_interest_rate",
    "current_account~constant+current_account.L(1)+world_gdp",
    "manufacturing~constant+manufacturing.L(1)+world_gdp",
    "service~constant+service.L(1)+population+gdp",
    "gdp==0.5*manufacturing+0.5*service",
    "real_interest_rate==1*nominal_interest_rate-1*inflation_rate"
  ) # Duplicate "consumption"
  endogenous_variables <- c(
    "real_interest_rate", "consumption", "consumption", "investment",
    "current_account", "gdp", "manufacturing", "service"
  )
  exogenous_variables <- c(
    "world_gdp", "nominal_interest_rate", "inflation_rate"
  )
  predetermined_variables <- c(
    "consumption.L(1)", "consumption.L(2)", "investment.L(1)", "current_account.L(1)",
    "manufacturing.L(1)", "service.L(1)", "population"
  )

  object <- structure(
    list(
      equations = equations,
      endogenous_variables = endogenous_variables,
      exogenous_variables = exogenous_variables,
      predetermined_variables = predetermined_variables
    ),
    class = "koma_seq"
  )
  expect_error(
    validate_system_of_equations(object),
    "Declared endogenous variables are not unique."
  )

  endogenous_variables <- c(
    "consumption", "investment", "current_account", "gdp",
    "manufacturing", "service"
  )
  # Duplicate "world_gdp"
  exogenous_variables <- c(
    "real_interest_rate", "world_gdp", "world_gdp", "nominal_interest_rate",
    "inflation_rate"
  )

  object <- structure(
    list(
      equations = equations,
      endogenous_variables = endogenous_variables,
      exogenous_variables = exogenous_variables,
      predetermined_variables = predetermined_variables
    ),
    class = "koma_seq"
  )
  expect_error(
    validate_system_of_equations(object),
    "Declared exogenous variables are not unique."
  )
})

test_that("validate_system_of_equations throws error for duplicate dependent
variables", {
  equations <- c(
    "consumption~constant+gdp",
    "consumption~constant+investment"
  )
  endogenous_variables <- c("consumption", "consumption")
  exogenous_variables <- c("gdp", "investment")
  predetermined_variables <- c()

  object <- structure(
    list(
      equations = equations,
      endogenous_variables = endogenous_variables,
      exogenous_variables = exogenous_variables,
      predetermined_variables = predetermined_variables
    ),
    class = "koma_seq"
  )
  expect_error(
    validate_system_of_equations(object),
    "Declared endogenous variables are not unique"
  )
})

# Validate priors
test_that("validate_priors", {
  equation <- "consumption~{0,1000}constant+{0.4,0.1}gdp+{0.9,10}consumption.L(1)+{0,1000}consumption.L(2)+{3,0.001}"
  expect_no_error(validate_priors(equation))

  # no priors specified
  equation <- "consumption~constant+gdp+consumption.L(1)+consumption.L(2)"
  expect_no_error(validate_priors(equation))

  # missing prior before constant
  equation <- "consumption~constant+{0.4,0.1}gdp+{0.9,10}consumption.L(1)+{0,1000}consumption.L(2)+{3,0.001}"
  expect_no_error(validate_priors(equation))

  # misspecified prior (numbers seperated by ;)
  equation <- "consumption~{0;1000}constant+{0.4,0.1}gdp+{0.9,10}consumption.L(1)+{0,1000}consumption.L(2)+{3,0.001}"
  expect_error(validate_priors(equation))

  # misspecified prior (no seperation of mean and variance)
  equation <- "consumption~constant+{0.4,0.1}gdp+{0.9,10}consumption.L(1)+{01000}consumption.L(2)+{3,0.001}"
  expect_error(validate_priors(equation))

  # two misspecified priors
  equation <- "consumption~{0;1000}constant+{0.40.1}gdp+{0.9,10}consumption.L(1)+{0,1000}consumption.L(2)+{3,0.001}"
  expect_error(validate_priors(equation))

  equation <- "{1,3}consumption~{0,1}constant+{0.4,0.1}gdp+{0.9,10}consumption.L(1)+{0,1000}consumption.L(2)+{3,0.001}"
  expect_error(validate_priors(equation))
})

# Validate single equations
test_that("validate_equation processes valid R variable names correctly", {
  equation <- c("consumption123~constant+0.5*manufacturing+gdp")
  expect_true(validate_equation(equation))
})

test_that("validate_equation processes valid equation with priors correctly", {
  equation <- "consumption~constant+gdp+consumption.L(1)+consumption.L(2)"
  expect_true(validate_equation(equation))
})

test_that("validate_equation throws error for invalid structure", {
  equation <- c("consumption=constant+gdp") # Missing '=='
  expect_error(validate_equation(equation), "Invalid equation structure")
})

test_that("validate_equation throws error for invalid left variable name", {
  equation <- c("consumption 1~constant+gdp") # Space in variable name
  expect_error(validate_equation(equation))
})

test_that("validate_equation throws error for duplicate regressors", {
  equation <- c("consumption~constant+gdp+gdp+consumption.L(1)")
  expect_error(validate_equation(equation))
})

test_that("validate_equation throws error for left variable that is numeric", {
  equation <- c("123~constant+gdp")
  expect_error(validate_equation(equation))
})

test_that("validate_equation throws error for variable name with special
characters", {
  equation <- c("consumption@123~constant+gdp")
  expect_error(validate_equation(equation))
})

test_that("validate_equation throws error for invalid right-side number-only
component", {
  equation <- c("consumption~constant+123")
  expect_error(validate_equation(equation))
})

test_that("validate_equation throws error for right-side multiplication with
non-numeric first component", {
  equation <- c("consumption~constant-gdp*5")
  expect_error(validate_equation(equation))
})

test_that("validate_equation for weight definition", {
  equation <- "gdp==(nom_manufacturing/nom_gdp)*manufacturing+(nom_service/nom_gdp)*service"
  expect_true(validate_equation(equation))
})

test_that("validate_equation throws error for right-side multiplication with
both components as numbers", {
  equation <- c("consumption==constant-10*5")
  expect_error(validate_equation(equation))
})

test_that("validate_equation throws error for invalid right-side operator", {
  equation <- c("consumption==constant-10/5")
  expect_error(validate_equation(equation))
})

test_that("validate_equation throws error for invalid right-side single
component variable", {
  equation <- c("consumption~constant+123gdp")
  expect_error(validate_equation(equation))
})

test_that("validate_equation throws error for right-side variables with invalid
first component during multiplication", {
  equation <- c("consumption~constant+gdp*income")
  expect_error(validate_equation(equation))
})

test_that("validate_equation throws error for right-side variables with invalid
second component during multiplication", {
  equation <- c("consumption~constant+2*123income")
  expect_error(validate_equation(equation))
})

test_that("validate_equation throws error for right-side variables with too
many components separated by '*'", {
  equation <- c("consumption~constant+2*income*gdp")
  expect_error(validate_equation(equation))
})
