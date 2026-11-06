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

test_that("get_seq_weights keeps the weight expressions", {
  ts_data <- nominal_with_shares(c(0.6, 0.7, 0.8))

  result <- get_seq_weights(ts_data, identity_weights())

  expect_equal(
    result$gdp$expressions,
    list(
      manufacturing = "nom_manufacturing/nom_gdp",
      service = "nom_service/nom_gdp"
    )
  )
})

test_that("get_seq_weights keeps no expressions for numeric weights", {
  sys_eq <- system_of_equations(
    "a ~ a.L(1), b ~ b.L(1), y == 0.5*a + 0.5*b"
  )

  result <- get_seq_weights(list(), sys_eq$identities)

  expect_null(result$y$expressions)
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

test_that("get_seq_weights uses the share of the chosen year", {
  ts_data <- nominal_with_shares(c(0.6, 0.7, 0.8))

  result <- get_seq_weights(ts_data, identity_weights(), period = 1996)

  expect_equal(result$gdp$weights$theta_gamma3_1, 0.7)
  expect_equal(result$gdp$weights$theta_gamma3_2, 0.3)
})

test_that("get_seq_weights uses the share of the chosen quarter", {
  ts_data <- nominal_with_shares(c(0.6, 0.7, 0.8))
  # 1996 Q2
  ts_data$nom_manufacturing[6] <- 90
  ts_data$nom_service[6] <- 10

  result <- get_seq_weights(ts_data, identity_weights(), period = c(1996, 2))

  expect_equal(result$gdp$weights$theta_gamma3_1, 0.9)
  expect_equal(result$gdp$weights$theta_gamma3_2, 0.1)
})

test_that("get_seq_weights errors if the chosen period is not complete", {
  ts_data <- nominal_with_shares(c(0.6, 0.7, 0.8), extra_quarters = 2)
  ts_data$nom_service <- stats::window(ts_data$nom_service, start = c(1996, 3))

  expect_error(
    get_seq_weights(ts_data, identity_weights(), period = 1998),
    "1998"
  )
  expect_error(
    get_seq_weights(ts_data, identity_weights(), period = 1996),
    "1996"
  )
  expect_error(
    get_seq_weights(ts_data, identity_weights(), period = c(1996, 2)),
    "1996 Q2"
  )
  expect_error(
    get_seq_weights(ts_data, identity_weights(), period = 2010),
    "2010"
  )
})

test_that("get_seq_weights checks the chosen period", {
  ts_data <- nominal_with_shares(c(0.6, 0.7, 0.8))

  expect_error(
    get_seq_weights(ts_data, identity_weights(), period = "1996"),
    "dates\\$weights"
  )
  expect_error(
    get_seq_weights(ts_data, identity_weights(), period = c(1996, 5)),
    "dates\\$weights"
  )
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
  expect_length(result$gdp$service, 1)
})

test_that("calculate_eq_weights uses the share of the chosen year", {
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

  result_1990 <- calculate_eq_weights(ts_data, iden, period = 1990)
  result_1991 <- calculate_eq_weights(ts_data, iden, period = 1991)
  result <- calculate_eq_weights(ts_data, iden)

  expect_equal(result_1990$gdp$manufacturing, weight_comp1_1990)
  expect_equal(result_1990$gdp$service, weight_comp2_1990)
  expect_equal(result_1991$gdp$manufacturing, weight_comp1_1991)
  expect_equal(result_1991$gdp$service, weight_comp2_1991)
  # Without a period, the last year
  expect_equal(result$gdp$manufacturing, weight_comp1_1992)
  expect_equal(result$gdp$service, weight_comp2_1992)
  expect_equal(names(result), "gdp")
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

test_that("calculate_eq_weights gives a year the scale of its periods", {
  ts_data <- list(
    nom_manufacturing = stats::ts(rep(c(100, 200, 300), each = 4),
      start = 1990, frequency = 4
    )
  )
  iden <- list(gdp = list(
    components = list(manufacturing = "theta_gamma3_1"),
    weights = list(theta_gamma3_1 = "nom_manufacturing/1000")
  ))

  year <- calculate_eq_weights(ts_data, iden, period = 1991)
  quarter <- calculate_eq_weights(ts_data, iden, period = c(1991, 4))

  expect_equal(year$gdp$manufacturing, 0.2)
  expect_equal(year, quarter)
})

test_that("calculate_eq_weights evaluates numbers in weight expressions", {
  ts_data <- nominal_with_shares(c(0.6, 0.7, 0.8))
  iden <- list(gdp = list(
    components = list(manufacturing = "theta_gamma3_1"),
    weights = list(theta_gamma3_1 = "nom_manufacturing/nom_gdp/2")
  ))

  result <- calculate_eq_weights(ts_data, iden)

  expect_equal(result$gdp$manufacturing, 0.4)
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

# Component volumes and annual nominal shares from quarterly prices and
# quantities.
chain_link_inputs <- function(prices, quantities, signs, start = c(1990, 1)) {
  year <- start[1] + (seq_len(nrow(quantities)) + start[2] - 2) %/% 4
  annual_nominal <- rowsum(prices * quantities, year)
  totals <- drop(annual_nominal %*% signs)
  share_start <- as.numeric(rownames(annual_nominal)[1])
  components <- colnames(quantities)
  levels <- lapply(components, function(comp) {
    stats::ts(quantities[, comp], start = start, frequency = 4)
  })
  shares <- lapply(seq_along(components), function(i) {
    stats::ts(signs[i] * annual_nominal[, i] / totals,
      start = share_start, frequency = 1
    )
  })
  names(levels) <- names(shares) <- components
  list(levels = levels, shares = shares)
}

# Textbook annual overlap: value each quarter at the average prices of the
# previous year and link it to the previous year's average at those prices.
annual_overlap <- function(prices, quantities, signs) {
  year <- 1990 + (seq_len(nrow(quantities)) - 1) %/% 4
  annual_price <- rowsum(prices * quantities, year) / rowsum(quantities, year)
  annual_quantity <- rowsum(quantities, year) / 4
  out <- c()
  base <- 100
  for (y in unique(year)[-1]) {
    previous_price <- signs * annual_price[as.character(y - 1), ]
    at_previous_prices <- drop(quantities[year == y, ] %*% previous_price)
    previous_average <- sum(previous_price * annual_quantity[as.character(y - 1), ])
    level <- base * at_previous_prices / previous_average
    out <- c(out, level)
    base <- mean(level)
  }
  stats::ts(out, start = c(1991, 1), frequency = 4)
}

# k random walks of length n, column j around scale[j]
random_walk <- function(n, k, scale) {
  walks <- exp(apply(matrix(rnorm(n * k, sd = 0.02), n, k), 2, cumsum))
  sweep(walks, 2, rep_len(scale, k), `*`)
}

test_that("chain_link matches annual overlap at previous-year prices", {
  set.seed(1)
  quantities <- random_walk(24, 3, c(100, 60, 30))
  prices <- random_walk(24, 3, 1)
  colnames(quantities) <- colnames(prices) <- c("consumption", "exports", "imports")
  signs <- c(1, 1, -1)
  inputs <- chain_link_inputs(prices, quantities, signs)

  result <- chain_link(inputs$levels, inputs$shares)

  expect_equal(result, annual_overlap(prices, quantities, signs))
})

test_that("chain_link with constant prices grows like the fixed-price sum", {
  set.seed(2)
  quantities <- random_walk(20, 2, c(100, 50))
  colnames(quantities) <- c("manufacturing", "service")
  prices <- matrix(c(1.2, 0.8), nrow = 20, ncol = 2, byrow = TRUE)
  inputs <- chain_link_inputs(prices, quantities, c(1, 1))

  result <- chain_link(inputs$levels, inputs$shares)

  fixed_price_sum <- stats::window(
    stats::ts(drop(quantities %*% c(1.2, 0.8)), start = 1990, frequency = 4),
    start = 1991
  )
  expect_equal(
    as.numeric(result / stats::lag(result, -1)),
    as.numeric(fixed_price_sum / stats::lag(fixed_price_sum, -1))
  )
})

test_that("chain_link of a single component follows the component", {
  component <- stats::ts(100 * 1.01^(0:15), start = 1990, frequency = 4)
  shares <- stats::ts(rep(1, 3), start = 1990, frequency = 1)

  result <- chain_link(list(a = component), list(a = shares))

  expected <- 100 * stats::window(component, start = 1991) / mean(component[1:4])
  expect_equal(result, expected)
})

test_that("chain_link does not depend on the scale of component levels", {
  set.seed(3)
  quantities <- random_walk(16, 2, c(100, 50))
  prices <- random_walk(16, 2, 1)
  colnames(quantities) <- colnames(prices) <- c("a", "b")
  inputs <- chain_link_inputs(prices, quantities, c(1, 1))
  rescaled <- inputs$levels
  rescaled$a <- 1000 * rescaled$a

  expect_equal(
    chain_link(rescaled, inputs$shares),
    chain_link(inputs$levels, inputs$shares)
  )
})

test_that("chain_link does not require shares that add up to 1", {
  set.seed(4)
  quantities <- random_walk(16, 2, c(100, 50))
  prices <- random_walk(16, 2, 1)
  colnames(quantities) <- colnames(prices) <- c("a", "b")
  inputs <- chain_link_inputs(prices, quantities, c(1, 1))
  scaled_shares <- lapply(inputs$shares, function(x) 0.9 * x)

  expect_equal(
    chain_link(inputs$levels, scaled_shares),
    chain_link(inputs$levels, inputs$shares)
  )
})

test_that("chain_link starts after the first complete year", {
  set.seed(5)
  # 1990 Q2 to 1993 Q3: 1990 and 1993 are incomplete.
  quantities <- random_walk(14, 2, c(100, 50))
  prices <- random_walk(14, 2, 1)
  colnames(quantities) <- colnames(prices) <- c("a", "b")
  inputs <- chain_link_inputs(prices, quantities, c(1, 1), start = c(1990, 2))

  result <- chain_link(inputs$levels, inputs$shares)

  expect_equal(stats::start(result), c(1992, 1))
  expect_equal(stats::end(result), c(1993, 3))
})

test_that("chain_link stops when components and shares differ", {
  levels <- list(a = stats::ts(1:8, start = 1990, frequency = 4))
  shares <- list(b = stats::ts(1, start = 1991, frequency = 1))

  expect_error(chain_link(levels, shares), "must match")
})

growth <- function(x) x / stats::lag(x, -1) - 1

# Prices that follow a random walk until the end of `base_year` and stay at
# that year's average prices afterwards, as in a forecast.
prices_fixed_after <- function(quantities, base_year, start = 1990) {
  prices <- random_walk(nrow(quantities), ncol(quantities), 1)
  year <- start + (seq_len(nrow(quantities)) - 1) %/% 4
  in_base <- year == base_year
  base_price <- colSums(prices[in_base, ] * quantities[in_base, ]) /
    colSums(quantities[in_base, ])
  prices[year > base_year, ] <- rep(base_price, each = sum(year > base_year))
  prices
}

test_that("fixed_price_weights aggregate like chain-linking with base-year prices", {
  set.seed(8)
  quantities <- random_walk(24, 3, c(100, 60, 30))
  colnames(quantities) <- c("consumption", "exports", "imports")
  prices <- prices_fixed_after(quantities, base_year = 1992)
  colnames(prices) <- colnames(quantities)
  inputs <- chain_link_inputs(prices, quantities, c(1, 1, -1))
  expected <- growth(chain_link(inputs$levels, inputs$shares))

  history <- lapply(inputs$levels, stats::window, end = c(1992, 4))
  prices <- base_prices(history, inputs$shares, base_year = 1992)

  # From the second forecast quarter on, including the next first quarter
  for (h in seq(1993.25, 1995.75, by = 0.25)) {
    previous <- vapply(inputs$levels, function(x) {
      as.numeric(stats::window(x, h - 0.25, h - 0.25))
    }, numeric(1))
    growth_h <- vapply(inputs$levels, function(x) {
      as.numeric(stats::window(growth(x), h, h))
    }, numeric(1))
    weights <- fixed_price_weights(previous, prices)

    expect_equal(sum(weights), 1)
    expect_equal(
      sum(weights * (1 + growth_h)) - 1,
      as.numeric(stats::window(expected, h, h))
    )
  }
})

test_that("weight_series lists each series in the expressions once", {
  expressions <- list(a = "n_a/n_y", m = "-0.5*n_m/n_y")

  expect_equal(weight_series(expressions), c("n_a", "n_y", "n_m"))
  expect_equal(weight_series(list(a = "0.5")), character(0))
})

test_that("evaluate_weight evaluates on the given values only", {
  values <- list(n_a = c(2, 3), n_y = c(4, 6))

  expect_equal(evaluate_weight("-0.5*n_a/n_y", values), c(-0.25, -0.25))
  n_m <- 1
  expect_error(evaluate_weight("n_a/n_m", values), "n_m")
})

test_that("link_quarter_weights are fixed_price_weights without a price change", {
  last_levels <- c(a = 110, m = 40)
  mean_levels <- c(a = 100, m = 35)
  prices <- c(a = 1.2, m = -0.8)

  weights <- link_quarter_weights(last_levels, mean_levels, prices, prices)

  expect_equal(weights, fixed_price_weights(last_levels, prices))
  expect_equal(sum(weights), 1)
})

test_that("link_quarter_weights do not add up to 1 after a price change", {
  last_levels <- c(a = 110, m = 40)
  mean_levels <- c(a = 100, m = 35)

  weights <- link_quarter_weights(
    last_levels, mean_levels,
    prices = c(a = 1.2, m = -0.8), previous_prices = c(a = 1, m = -1)
  )

  link <- (100 - 35) / (110 - 40)
  expect_equal(weights, c(a = 1.2 * 110, m = -0.8 * 40) / (1.2 * 100 - 0.8 * 35) * link)
  expect_false(isTRUE(all.equal(sum(weights), 1)))
})

test_that("complete_year_means are NA for incomplete years", {
  # 1990 Q2 to 1992 Q3: only 1991 is complete
  x <- stats::ts(1:10, start = c(1990, 2), frequency = 4)

  means <- complete_year_means(x)

  expect_equal(names(means), c("1990", "1991", "1992"))
  expect_equal(as.numeric(means), c(NA, mean(4:7), NA))
})

test_that("share_of_year looks up the share of each year", {
  share <- stats::ts(c(0.6, 0.7, 0.8), start = 1990, frequency = 1)

  expect_equal(share_of_year(share, c(1991, 1991, 1990, 1993)), c(0.7, 0.7, 0.6, NA))
})

test_that("base_prices errors without shares and levels of the base year", {
  levels <- list(a = stats::ts(1:8, start = 1990, frequency = 4))
  shares <- list(a = stats::ts(1, start = 1990, frequency = 1))

  expect_error(base_prices(levels, shares, base_year = 1991), "1991")
})

# Real components and nominal series as koma_ts, and the identity
# y == (n_a/n_y)*a - (n_m/n_y)*m with its weights computed from the data.
chain_link_data <- function(prices, quantities, start = c(1990, 1)) {
  real <- function(x) {
    ets(x, start = start, frequency = 4, series_type = "level", method = "diff_log")
  }
  nominal <- prices * quantities
  ts_data <- list(
    a = real(quantities[, 1]),
    m = real(quantities[, 2]),
    n_a = real(nominal[, 1]),
    n_m = real(nominal[, 2]),
    n_y = real(nominal[, 1] - nominal[, 2])
  )
  sys_eq <- system_of_equations(
    "a ~ a.L(1), m ~ m.L(1), y == (n_a/n_y)*a - (n_m/n_y)*m"
  )
  identities <- get_seq_weights(level(ts_data), sys_eq$identities)
  list(ts_data = ts_data, iden = identities$y)
}

test_that("chain_link_start links the first quarter after the base year", {
  set.seed(10)
  quantities <- random_walk(20, 2, c(100, 30))
  prices <- prices_fixed_after(quantities, base_year = 1993)
  data <- chain_link_data(prices, quantities)
  levels <- lapply(data$ts_data[c("a", "m")], function(x) stats::ts(c(x),
    start = stats::start(x), frequency = 4
  ))
  shares <- annual_shares(data$ts_data, data$iden$expressions)
  expected <- growth(chain_link(levels, shares))

  history <- lapply(data$ts_data, stats::window, end = c(1993, 4))
  start <- chain_link_start(history, data$iden, forecast_start = 1994)

  growth_link <- vapply(levels, function(x) {
    as.numeric(stats::window(growth(x), c(1994, 1), c(1994, 1)))
  }, numeric(1))
  expect_equal(start$base_year, 1993)
  expect_true(abs(start$link) > 1e-6)
  expect_equal(
    sum(start$weights * 100 * growth_link) + start$link,
    100 * as.numeric(stats::window(expected, c(1994, 1), c(1994, 1)))
  )
})

test_that("chain_link_start uses base-year prices within the year", {
  set.seed(11)
  quantities <- random_walk(14, 2, c(100, 30))
  prices <- random_walk(14, 2, 1)
  data <- chain_link_data(prices, quantities)
  levels <- lapply(data$ts_data[c("a", "m")], function(x) stats::ts(c(x),
    start = stats::start(x), frequency = 4
  ))
  shares <- annual_shares(data$ts_data, data$iden$expressions)

  # data until 1993 Q2: the base year is 1992
  start <- chain_link_start(data$ts_data, data$iden, forecast_start = 1993.5)

  previous <- vapply(levels, function(x) as.numeric(utils::tail(x, 1)), numeric(1))
  expected <- fixed_price_weights(previous, base_prices(levels, shares, 1992))
  expect_equal(start$weights, expected)
  expect_equal(start$link, 0)
})

test_that("setup_varying_weights prepares the identities with weights from data", {
  set.seed(16)
  # Forecast from 1993 Q1: a link quarter
  quantities <- random_walk(16, 2, c(100, 30))
  prices <- random_walk(16, 2, 1)
  data <- chain_link_data(prices, quantities)
  sys_eq <- system_of_equations(paste(
    "a ~ a.L(1), m ~ m.L(1), y == (n_a/n_y)*a - (n_m/n_y)*m,",
    "z == (n_a/n_y)*a + 0.2*m, w == 0.5*a + 0.5*m"
  ))
  identities <- get_seq_weights(level(data$ts_data), sys_eq$identities)
  attr(data$ts_data$m, "method") <- "percentage"

  varying <- setup_varying_weights(data$ts_data, identities, forecast_start = 1993)

  expect_named(varying, c("y", "z"))
  for (lhs in c("y", "z")) {
    expect_equal(
      varying[[lhs]][names(varying[[lhs]]) != "methods"],
      chain_link_start(data$ts_data, identities[[lhs]], forecast_start = 1993)
    )
  }
  expect_true(abs(varying$y$link) > 1e-6)
  expect_equal(varying$y$methods, c(a = "diff_log", m = "percentage"))
  expect_equal(varying$z$methods, c(a = "diff_log"))
})

test_that("next_chain_weights continue the weights at fixed prices", {
  set.seed(13)
  quantities <- random_walk(3, 2, c(100, 30))
  prices <- c(1.2, 0.8)
  weights <- fixed_price_weights(quantities[1, ], prices)

  expected <- 0.9 * fixed_price_weights(quantities[2, ], prices)
  growth <- quantities[2, ] / quantities[1, ]
  expect_equal(next_chain_weights(0.9 * weights, growth, 0.9), expected)
})

test_that("growth_factor follows the levels of the method", {
  r <- c(0.5, -1.2, 2)
  for (method in c("percentage", "diff_log")) {
    levels <- level(ets(r, start = c(2020, 1), frequency = 4,
      series_type = "rate", method = method
    ))
    expect_equal(
      vapply(r, growth_factor, numeric(1), method),
      as.numeric(levels / stats::lag(levels, -1))
    )
  }
})

test_that("annual_shares evaluate the weight expressions per year", {
  set.seed(12)
  quantities <- random_walk(12, 2, c(100, 30))
  prices <- random_walk(12, 2, 1)
  data <- chain_link_data(prices, quantities)

  shares <- annual_shares(data$ts_data, data$iden$expressions)

  nominal <- unname(rowsum(prices * quantities, rep(1990:1992, each = 4)))
  expect_equal(as.numeric(shares$a), nominal[, 1] / (nominal[, 1] - nominal[, 2]))
  expect_equal(as.numeric(shares$m), -nominal[, 2] / (nominal[, 1] - nominal[, 2]))
  expect_equal(stats::start(shares$a), c(1990, 1))
})

test_that("annual_shares evaluate numbers in weight expressions", {
  set.seed(15)
  quantities <- random_walk(12, 2, c(100, 30))
  prices <- random_walk(12, 2, 1)
  data <- chain_link_data(prices, quantities)

  halved <- annual_shares(data$ts_data, list(a = "n_a/n_y/2"))
  shares <- annual_shares(data$ts_data, data$iden$expressions)

  expect_equal(halved$a, 0.5 * shares$a)
})

test_that("annual_shares evaluate the expressions on annual averages", {
  set.seed(17)
  quantities <- random_walk(12, 2, c(100, 30))
  prices <- random_walk(12, 2, 1)
  data <- chain_link_data(prices, quantities)

  shares <- annual_shares(data$ts_data, list(a = "n_a/1000"))

  averages <- tapply(prices[, 1] * quantities[, 1], rep(1990:1992, each = 4), mean)
  expect_equal(as.numeric(shares$a), as.numeric(averages) / 1000)
})

test_that("annual_shares convert rates to levels", {
  set.seed(14)
  quantities <- random_walk(12, 2, c(100, 30))
  prices <- random_walk(12, 2, 1)
  data <- chain_link_data(prices, quantities)

  as_rates <- rate(data$ts_data)

  expect_equal(
    annual_shares(as_rates, data$iden$expressions),
    annual_shares(data$ts_data, data$iden$expressions)
  )
})

test_that("setup_varying_weights explains a component with a custom method", {
  set.seed(13)
  quantities <- random_walk(16, 2, c(100, 30))
  prices <- random_walk(16, 2, 1)
  data <- chain_link_data(prices, quantities)
  attr(data$ts_data$a, "method") <- expression(x * 10)

  expect_error(
    setup_varying_weights(data$ts_data, list(y = data$iden), forecast_start = 1994),
    "percentage"
  )
})

test_that("chain_link_start needs components with percentage or diff_log", {
  set.seed(13)
  quantities <- random_walk(16, 2, c(100, 30))
  prices <- random_walk(16, 2, 1)
  data <- chain_link_data(prices, quantities)

  for (method in list("none", expression(x * 10))) {
    unsupported <- data
    unsupported$ts_data$a <- rate(unsupported$ts_data$a)
    attr(unsupported$ts_data$a, "method") <- method

    expect_error(
      chain_link_start(unsupported$ts_data, unsupported$iden, forecast_start = 1994),
      "percentage"
    )
  }
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
