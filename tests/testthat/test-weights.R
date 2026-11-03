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

random_walk <- function(n, k, scale) {
  scale * exp(apply(matrix(rnorm(n * k, sd = 0.02), n, k), 2, cumsum))
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

test_that("chain_link_weights aggregate the growth of the chain-linked volume", {
  set.seed(6)
  quantities <- random_walk(24, 3, c(100, 60, 30))
  prices <- random_walk(24, 3, 1)
  colnames(quantities) <- colnames(prices) <- c("consumption", "exports", "imports")
  inputs <- chain_link_inputs(prices, quantities, c(1, 1, -1))

  weights <- chain_link_weights(inputs$levels, inputs$shares)

  # The weights of the last period are for the period after the levels.
  expected <- stats::window(
    growth(chain_link(inputs$levels, inputs$shares)),
    start = stats::start(weights$consumption)
  )
  periods <- stats::tsp(expected)
  weighted <- Reduce(`+`, lapply(names(weights), function(comp) {
    w <- stats::window(weights[[comp]], periods[1], periods[2])
    w * (1 + stats::window(growth(inputs$levels[[comp]]), periods[1], periods[2]))
  })) - 1
  expect_equal(weighted, expected)
})

test_that("chain_link_weights add up to 1 except in the first quarter", {
  set.seed(7)
  quantities <- random_walk(20, 2, c(100, 50))
  prices <- random_walk(20, 2, 1)
  colnames(quantities) <- colnames(prices) <- c("a", "b")
  inputs <- chain_link_inputs(prices, quantities, c(1, 1))

  weights <- chain_link_weights(inputs$levels, inputs$shares)

  total <- weights$a + weights$b
  first_quarter <- stats::cycle(total) == 1
  expect_equal(as.numeric(total[!first_quarter]), rep(1, sum(!first_quarter)))
  expect_true(all(abs(total[first_quarter] - 1) > 1e-6))
})

test_that("chain_link_weights are the shares when volumes are constant", {
  levels <- list(
    a = stats::ts(rep(10, 12), start = 1990, frequency = 4),
    b = stats::ts(rep(5, 12), start = 1990, frequency = 4)
  )
  shares <- list(
    a = stats::ts(c(0.6, 0.7, 0.8), start = 1990, frequency = 1),
    b = stats::ts(c(0.4, 0.3, 0.2), start = 1990, frequency = 1)
  )

  weights <- chain_link_weights(levels, shares)

  # From the first year with a complete previous year of the aggregate, to
  # the period after the levels.
  expect_equal(stats::tsp(weights$a), c(1992, 1993, 4))
  expect_equal(as.numeric(weights$a), c(0.7, 0.7, 0.7, 0.7, 0.8))
  expect_equal(as.numeric(weights$b), c(0.3, 0.3, 0.3, 0.3, 0.2))
})

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

test_that("chain_link_weights give the first forecast quarter", {
  set.seed(9)
  quantities <- random_walk(24, 2, c(100, 50))
  colnames(quantities) <- c("a", "b")
  prices <- prices_fixed_after(quantities, base_year = 1992)
  colnames(prices) <- colnames(quantities)
  inputs <- chain_link_inputs(prices, quantities, c(1, 1))
  expected <- growth(chain_link(inputs$levels, inputs$shares))

  history <- lapply(inputs$levels, stats::window, end = c(1992, 4))
  weights <- chain_link_weights(history, inputs$shares)
  link <- vapply(weights, function(w) as.numeric(utils::tail(w, 1)), numeric(1))
  growth_link <- vapply(inputs$levels, function(x) {
    as.numeric(stats::window(growth(x), c(1993, 1), c(1993, 1)))
  }, numeric(1))

  expect_equal(stats::end(weights$a), c(1993, 1))
  expect_equal(
    sum(link * (1 + growth_link)) - 1,
    as.numeric(stats::window(expected, c(1993, 1), c(1993, 1)))
  )
})

test_that("base_prices errors without shares and levels of the base year", {
  levels <- list(a = stats::ts(1:8, start = 1990, frequency = 4))
  shares <- list(a = stats::ts(1, start = 1990, frequency = 1))

  expect_error(base_prices(levels, shares, base_year = 1991), "1991")
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
