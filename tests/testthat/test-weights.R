test_that("get_seq_weights", {
  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == (nom_manufacturing/nom_gdp)*manufacturing + (nom_service/nom_gdp)*service,
    real_interest_rate == 1*nominal_interest_rate - 1*inflation_rate"

  # Vector of exogenous variables
  exogenous_variables <- c(
    "nominal_interest_rate", "inflation_rate",
    "world_gdp", "population"
  )

  sys_eq <- system_of_equations(equations, exogenous_variables)

  identities <- sys_eq$identities

  weight_manufacturing <- 0.7
  weight_service <- 1 - weight_manufacturing
  ts_data <- list(
    nom_gdp = stats::ts(1:30, start = 1990, frequency = 4),
    nom_manufacturing = stats::ts((1:30) * weight_manufacturing,
      start = 1990, frequency = 4
    ),
    nom_service = stats::ts(1:30 * weight_service,
      start = 1990, frequency = 4
    )
  )

  seq_weights <- get_seq_weights(ts_data, identities)

  expect_equal(seq_weights$gdp$weights$theta_gamma6_4, weight_manufacturing)
  expect_equal(seq_weights$gdp$weights$theta_gamma6_5, weight_service)
})

# Nominal series whose annual shares are `shares`, one per year from `start`.
nominal_with_shares <- function(shares, start = 1995, extra_quarters = 0) {
  n <- 4 * length(shares) + extra_quarters
  share <- c(rep(shares, each = 4), rep(0.5, extra_quarters))
  total <- stats::ts(rep(100, n), start = start, frequency = 4)
  list(
    nom_gdp = total,
    nom_manufacturing = total * share,
    nom_service = total * (1 - share)
  )
}

identity_weights <- function() {
  sys_eq <- system_of_equations(
    "manufacturing ~ manufacturing.L(1),
    service ~ service.L(1),
    gdp == (nom_manufacturing/nom_gdp)*manufacturing + (nom_service/nom_gdp)*service"
  )
  sys_eq$identities
}

test_that("get_seq_weights uses the share of the last year in the data", {
  ts_data <- nominal_with_shares(c(0.6, 0.7, 0.8))

  result <- get_seq_weights(ts_data, identity_weights())

  expect_equal(result$gdp$weights$theta_gamma3_1, 0.8)
  expect_equal(result$gdp$weights$theta_gamma3_2, 0.2)
})

test_that("get_seq_weights ignores an incomplete last year", {
  ts_data <- nominal_with_shares(c(0.6, 0.7, 0.8), extra_quarters = 2)

  result <- get_seq_weights(ts_data, identity_weights())

  expect_equal(result$gdp$weights$theta_gamma3_1, 0.8)
})

test_that("get_seq_weights ignores a last year with missing values", {
  ts_data <- nominal_with_shares(c(0.6, 0.7, 0.8))
  ts_data$nom_service[12] <- NA

  result <- get_seq_weights(ts_data, identity_weights())

  expect_equal(result$gdp$weights$theta_gamma3_1, 0.7)
  expect_equal(result$gdp$weights$theta_gamma3_2, 0.3)
})

test_that("get_seq_weights uses the last year complete for all series", {
  ts_data <- nominal_with_shares(c(0.6, 0.7, 0.8))
  ts_data$nom_service <- stats::window(ts_data$nom_service, end = c(1997, 3))

  result <- get_seq_weights(ts_data, identity_weights())

  expect_equal(result$gdp$weights$theta_gamma3_1, 0.7)
  expect_equal(result$gdp$weights$theta_gamma3_2, 0.3)
})

test_that("calculate_eq_weights computes fixed weights correctly", {
  weight_manufacturing <- 0.7
  weight_service <- 1 - weight_manufacturing
  ts_data <- list(
    nom_gdp = stats::ts(1:30, start = 1990, frequency = 4),
    nom_manufacturing = stats::ts((1:30) * weight_manufacturing,
      start = 1990, frequency = 4
    ),
    nom_service = stats::ts(1:30 * weight_service,
      start = 1990, frequency = 4
    )
  )

  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == (nom_manufacturing/nom_gdp)*manufacturing + (nom_service/nom_gdp)*service,
    real_interest_rate == 1*nominal_interest_rate - 1*inflation_rate"

  # Vector of exogenous variables
  exogenous_variables <- c(
    "nominal_interest_rate", "inflation_rate",
    "world_gdp", "population"
  )

  sys_eq <- system_of_equations(equations, exogenous_variables)

  identities <- sys_eq$identities
  iden <- identities[1] # gdp

  result <- calculate_eq_weights(ts_data, iden)

  expect_equal(result$gdp$manufacturing[1], weight_manufacturing)
  expect_equal(result$gdp$service[1], weight_service)
  expect_equal(names(result), "gdp")

  # 1990 Q1 to 1997 Q2: complete years 1990 to 1996 weight 1991 to 1997
  expect_equal(stats::tsp(result$gdp$manufacturing)[1:2], c(1991, 1997))
  expect_length(result$gdp$service, 7)
})

test_that("calculate_eq_weights computes dynamic weights", {
  weight_comp1_1990 <- 0.7
  weight_comp2_1990 <- 1 - weight_comp1_1990
  weight_comp1_1991 <- 0.3
  weight_comp2_1991 <- 1 - weight_comp1_1991
  weight_comp1_1992 <- 0.5
  weight_comp2_1992 <- 1 - weight_comp1_1992

  comp1_1990 <- stats::ts((1:4) * weight_comp1_1990,
    start = 1990, frequency = 4
  )
  comp2_1990 <- stats::ts(1:4 * weight_comp2_1990,
    start = 1990, frequency = 4
  )
  comp1_1991 <- stats::ts((5:8) * weight_comp1_1991,
    start = 1991, frequency = 4
  )
  comp2_1991 <- stats::ts((5:8) * weight_comp2_1991,
    start = 1990, frequency = 4
  )
  comp1_1992 <- stats::ts((9:12) * weight_comp1_1992,
    start = 1992, frequency = 4
  )
  comp2_1992 <- stats::ts((9:12) * weight_comp2_1992,
    start = 1992, frequency = 4
  )

  nominal_comp1 <- stats::ts(c(comp1_1990, comp1_1991, comp1_1992),
    start(comp1_1990),
    frequency = frequency(comp1_1990)
  )
  nominal_comp2 <- stats::ts(c(comp2_1990, comp2_1991, comp2_1992),
    start(comp2_1990),
    frequency = frequency(comp2_1990)
  )
  ts_data <- list(
    nom_gdp = stats::ts(1:12, start = 1990, frequency = 4),
    nom_manufacturing = nominal_comp1,
    nom_service = nominal_comp2
  )

  iden <- list(
    gdp = list(
      equation = "gdp==(nom_manufacturing/nom_gdp)*manufacturing+(nom_service/nom_gdp)*service",
      components = list(manufacturing = "theta6_4", service = "theta6_5"),
      weights = list(theta6_4 = "nom_manufacturing/nom_gdp", theta6_5 = "nom_service/nom_gdp")
    )
  )

  result <- calculate_eq_weights(ts_data, iden)

  expect_equal(result$gdp$manufacturing[1], weight_comp1_1990)
  expect_equal(result$gdp$manufacturing[2], weight_comp1_1991)
  expect_equal(result$gdp$manufacturing[3], weight_comp1_1992)
  expect_equal(result$gdp$service[1], weight_comp2_1990)
  expect_equal(result$gdp$service[2], weight_comp2_1991)
  expect_equal(result$gdp$service[3], weight_comp2_1992)
  expect_equal(names(result), "gdp")

  expect_length(result$gdp$manufacturing, 3)
  expect_length(result$gdp$service, 3)
})

test_that("calculate_eq_weights with negative weights", {
  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == (nom_consumption/nom_gdp)*consumption + (nom_investment/nom_gdp)*investment + (nom_exports/nom_gdp)*exports - (nom_imports/nom_gdp)*imports,
    real_interest_rate == 1*nominal_interest_rate - 1*inflation_rate"

  # Vector of exogenous variables
  exogenous_variables <- c(
    "nominal_interest_rate", "inflation_rate",
    "world_gdp", "population", "imports", "exports"
  )

  sys_eq <- system_of_equations(equations, exogenous_variables)

  identities <- sys_eq$identities
  iden <- identities[1] # gdp

  # GDP == C + I + X - M
  # weights have to add up to 1
  weight_consumption <- 0.6
  weight_investment <- 0.3
  weight_exports <- 0.6
  weight_imports <- 0.5

  ts_data <- list(
    nom_gdp = stats::ts(1:30, start = 1990, frequency = 4),
    nom_consumption = stats::ts((1:30) * weight_consumption,
      start = 1990, frequency = 4
    ),
    nom_investment = stats::ts(1:30 * weight_investment,
      start = 1990, frequency = 4
    ),
    nom_exports = stats::ts((1:30) * weight_exports,
      start = 1990, frequency = 4
    ),
    nom_imports = stats::ts(1:30 * weight_imports,
      start = 1990, frequency = 4
    )
  )

  result <- calculate_eq_weights(ts_data, iden)

  expect_equal(result$gdp$consumption[1], weight_consumption)
  expect_equal(result$gdp$investment[1], weight_investment)
  expect_equal(result$gdp$exports[1], weight_exports)
  expect_equal(result$gdp$imports[1], -weight_imports)
  expect_equal(names(result), "gdp")
})

test_that("calculate_eq_weights with weights", {
  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == (nom_consumption/nom_gdp)*consumption + (nom_investment/nom_gdp)*investment + (nom_exports/nom_gdp)*exports - (nom_imports/nom_gdp)*imports + 1*inventory,
    real_interest_rate == 1*nominal_interest_rate - 1*inflation_rate"

  # Vector of exogenous variables
  exogenous_variables <- c(
    "nominal_interest_rate", "inflation_rate",
    "world_gdp", "population", "imports", "exports", "inventory"
  )

  sys_eq <- system_of_equations(equations, exogenous_variables)

  identities <- sys_eq$identities
  iden <- identities[1] # gdp

  # GDP == C + I + X - M
  # weights have to add up to 1
  weight_consumption <- 0.6
  weight_investment <- 0.3
  weight_exports <- 0.6
  weight_imports <- 0.5

  tslist <- list(
    nom_gdp = stats::ts(1:30, start = 1990, frequency = 4),
    nom_consumption = stats::ts((1:30) * weight_consumption,
      start = 1990, frequency = 4
    ),
    nom_investment = stats::ts(1:30 * weight_investment,
      start = 1990, frequency = 4
    ),
    nom_exports = stats::ts((1:30) * weight_exports,
      start = 1990, frequency = 4
    ),
    nom_imports = stats::ts(1:30 * weight_imports,
      start = 1990, frequency = 4
    )
  )

  result <- calculate_eq_weights(tslist, iden)

  expect_equal(result$gdp$consumption[1], weight_consumption)
  expect_equal(result$gdp$investment[1], weight_investment)
  expect_equal(result$gdp$exports[1], weight_exports)
  expect_equal(result$gdp$imports[1], -weight_imports)
  expect_equal(names(result), "gdp")
})

test_that("calculate_eq_weights returns error if the names of aggregate
value and components are not contained in tslist", {
  time_series_1 <- stats::ts(rnorm(100), start = c(1981, 1), frequency = 12)
  time_series_2 <- stats::ts(rnorm(100), start = c(1981, 1), frequency = 12)
  time_series_3 <- stats::ts(rnorm(100), start = c(1981, 1), frequency = 12)

  # Create a named list
  ts_data <- list(
    nominal_comp1 = time_series_1,
    nominal_comp2 = time_series_2,
    nominal_c = time_series_3
  )

  iden <- list(
    agg_val = list(
      components = list(comp1 = "theta6_4", comp2 = "theta6_5"),
      weights = list(
        theta6_4 = "nominal_comp1/nom_agg_val",
        theta6_5 = "nominal_comp2/nom_agg_val"
      )
    )
  )

  expect_error(
    calculate_eq_weights(ts_data, iden),
    "The following time series are missing from 'ts_data': nom_agg_val"
  )
})

test_that("update_identity_weights", {
  manufacturing_weight <- 0.4
  service_weight <- 0.6
  weights <- list(gdp = list(
    manufacturing = manufacturing_weight, service = service_weight
  ))

  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == (nom_manufacturing/nom_gdp)*manufacturing + (nom_service/nom_gdp)*service + 1*inventory,
    real_interest_rate == 1*nominal_interest_rate - 1*inflation_rate"

  # Vector of exogenous variables
  exogenous_variables <- c(
    "nominal_interest_rate", "inflation_rate",
    "world_gdp", "population", "inventory"
  )

  sys_eq <- system_of_equations(equations, exogenous_variables)

  identities <- sys_eq$identities

  result <- update_identity_weights(weights, identities)

  expect_identical(result$gdp$weights$theta_gamma6_4, manufacturing_weight)
  expect_identical(result$gdp$weights$theta_gamma6_5, service_weight)
  expect_identical(result$gdp$weights$theta_beta6_12, 1)
})

test_that("dynamic identity weights retain endogenous and exogenous values", {
  # b and x1 occupy row 2 in their respective coefficient matrices.
  sys_eq <- system_of_equations(
    "a ~ x1, b ~ a, y == (nom_b)*b + (nom_x1)*x1",
    exogenous_variables = "x1"
  )
  calculated_weights <- list(y = list(b = c(0.2, 0.3), x1 = c(0.8, 0.7)))

  identities <- update_identity_weights(calculated_weights, sys_eq$identities)

  expect_identical(unname(identities$y$weights), list(0.3, 0.7))
})

test_that("update_identity_weights throws error when NA", {
  weights <- list(gdp = list(manufacturing = 0.4, service = NA))

  identities <- list(
    gdp = list(
      equation = "gdp==(nom_manufacturing/nom_gdp)*manufacturing+(nom_service/nom_gdp)*service+1*inventory",
      components = list(
        manufacturing = "theta6_4", service = "theta6_5",
        inventory = "theta6_14"
      ), weights = list(
        theta6_4 = "nom_manufacturing/nom_gdp",
        theta6_5 = "nom_service/nom_gdp",
        theta6_14 = 1
      )
    ),
    real_interest_rate = list(
      equation = "real_interest_rate==1*nominal_interest_rate-1*inflation_rate",
      components = list(
        nominal_interest_rate = "theta7_8",
        inflation_rate = "theta7_9"
      ), weights = list(
        theta7_8 = 1,
        theta7_9 = -1
      )
    )
  )

  expect_error(
    update_identity_weights(weights, identities),
    "In weight calculation"
  )
})
