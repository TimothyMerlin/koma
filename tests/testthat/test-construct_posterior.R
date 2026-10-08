test_that("construct_posterior constructs median estimate matrices correctly", {
  system_of_equations <- list(
    endogenous_variables = NULL,
    total_exogenous_variables = NULL,
    identities = NULL,
    character_gamma_matrix = list(gamma_matrix = NULL)
  )
  system_of_equations$equations <-
    simulated_data$equations
  system_of_equations$endogenous_variables <-
    simulated_data$endogenous_variables
  system_of_equations$total_exogenous_variables <-
    simulated_data$total_exogenous_variables
  system_of_equations$character_gamma_matrix <-
    simulated_data$character_gamma_matrix
  system_of_equations$character_beta_matrix <-
    simulated_data$character_beta_matrix

  system_of_equations$identities <- list(
    gdp = list(
      equation = "gdp==manufacturing+service",
      components = list(
        manufacturing = "theta6_4", service = "theta6_5"
      ),
      weights = list(
        theta6_4 = 0.5,
        theta6_5 = 0.5
      ),
      matrix = c("gamma", "gamma")
    )
  )

  estimate <- extract_estimates_from_draws(
    system_of_equations, simulated_data$estimates
  )

  result <- construct_posterior(
    system_of_equations, estimate, find_phi_positions(system_of_equations)
  )

  expect_type(result, "list")
  expect_named(
    result,
    c("gamma_matrix", "beta_matrix", "phi_matrix", "sigma_matrix", "omega_matrix")
  )

  expect_equal(dim(result$gamma_matrix), c(6, 6))
  expect_equal(dim(result$beta_matrix), c(10, 6))
  expect_equal(dim(result$phi_matrix[[1]]), c(6, 6))
  expect_equal(dim(result$sigma_matrix), c(6, 6))
  expect_equal(dim(result$omega_matrix), c(6, 6))

  # check if identity weights are correctly set
  expect_equal(result$gamma_matrix[4, 6], -0.5)
  expect_equal(result$gamma_matrix[5, 6], -0.5)
})

test_that("construct_posterior posterior matrices are not complete", {
  # From the character matrices we know where to expect values in the gamma and
  # beta matrices, e.g. the weights have to be added to the posterior matrices
  # at the correct position.
  # If these expectations are somehow not met, the function has
  # to throw an error.

  system_of_equations <- list(
    endogenous_variables = NULL,
    total_exogenous_variables = NULL,
    identity_equations = NULL,
    identities = NULL,
    character_gamma_matrix = list(gamma_matrix = NULL)
  )
  system_of_equations$equations <-
    simulated_data$equations
  system_of_equations$endogenous_variables <-
    simulated_data$endogenous_variables
  system_of_equations$total_exogenous_variables <-
    simulated_data$total_exogenous_variables
  system_of_equations$character_gamma_matrix <-
    simulated_data$character_gamma_matrix
  system_of_equations$character_beta_matrix <-
    simulated_data$character_beta_matrix

  system_of_equations$identities <- list(
    gdp = list(
      equation = "gdp==manufacturing+service",
      components = list(
        manufacturing = "theta6_4", service = "theta6_5"
      ),
      weights = list(
        theta6_4 = 0,
        theta6_5 = 0
      ),
      matrix = c("gamma", "gamma")
    )
  )

  estimate <- extract_estimates_from_draws(
    system_of_equations, simulated_data$estimates
  )

  expect_error(
    construct_posterior(
      system_of_equations, estimate, find_phi_positions(system_of_equations)
    ),
    "column 6 and row 4"
  )
})

test_that("construct_posterior validates sigma_matrix before using it", {
  system_of_equations <- simulated_data[c(
    "equations", "endogenous_variables", "total_exogenous_variables",
    "character_gamma_matrix", "character_beta_matrix"
  )]
  system_of_equations$identities <- list(
    gdp = list(
      equation = "gdp==manufacturing+service",
      components = list(
        manufacturing = "theta6_4", service = "theta6_5"
      ),
      weights = list(
        theta6_4 = 0.5,
        theta6_5 = 0.5
      ),
      matrix = c("gamma", "gamma")
    )
  )
  estimate <- extract_estimates_from_draws(
    system_of_equations, simulated_data$estimates
  )
  phi_positions <- find_phi_positions(system_of_equations)

  with_sigma <- function(sigma_matrix) {
    estimate$sigma_matrix <- sigma_matrix
    construct_posterior(system_of_equations, estimate, phi_positions)
  }
  sigma_matrix <- estimate$sigma_matrix

  expect_error(with_sigma(diag(sigma_matrix)), "must be a matrix")
  expect_error(with_sigma(sigma_matrix[, 1:5]), "must be square")
  expect_error(with_sigma(sigma_matrix[1:5, 1:5]), "incompatible")

  # gdp is an identity and must not have a variance
  sigma_matrix[6, 6] <- 1
  expect_error(with_sigma(sigma_matrix), "zero variance")
})

test_that("construct_posterior returns estimates for draw jx", {
  system_of_equations <- list(
    endogenous_variables = NULL,
    total_exogenous_variables = NULL,
    identity_equations = NULL,
    identities = NULL,
    character_gamma_matrix = list(gamma_matrix = NULL)
  )
  system_of_equations$equations <-
    simulated_data$equations
  system_of_equations$endogenous_variables <-
    simulated_data$endogenous_variables
  system_of_equations$total_exogenous_variables <-
    simulated_data$total_exogenous_variables
  system_of_equations$character_gamma_matrix <-
    simulated_data$character_gamma_matrix
  system_of_equations$character_beta_matrix <-
    simulated_data$character_beta_matrix

  system_of_equations$identities <- list(
    gdp = list(
      equation = "gdp==manufacturing+service",
      components = list(
        manufacturing = "theta6_4", service = "theta6_5"
      ),
      weights = list(
        theta6_4 = 0.5,
        theta6_5 = 0.5
      ),
      matrix = c("gamma", "gamma")
    )
  )

  jx <- 1

  estimates <- list(
    consumption = list(
      gamma_jw = list(structure(-0.5, dim = c(1L, 1L))),
      beta_jw = list(c(1.2, 0.5, 0.2)),
      omega_tilde_jw = list(structure(c(0.5, NA, NA, NA), dim = c(2L, 2L)))
    ),
    investment = list(
      gamma_jw = list(structure(-1.6, dim = c(1L, 1L))),
      beta_jw = list(c(2.3, 0.4, 0.3)),
      omega_tilde_jw = list(structure(c(0.9, NA, NA, NA), dim = c(2L, 2L)))
    ),
    current_account = list(
      gamma_jw = list(structure(0, dim = c(1L, 1L))),
      beta_jw = list(c(1.5, -0.5, 0.5)),
      omega_tilde_jw = list(structure(0.1, dim = c(1L, 1L)))
    ),
    manufacturing = list(
      gamma_jw = list(structure(0, dim = c(1L, 1L))),
      beta_jw = list(c(0.6, 0.1, 0.25)),
      omega_tilde_jw = list(structure(0.4, dim = c(1L, 1L)))
    ),
    service = list(
      gamma_jw = list(structure(-0.22, dim = c(1L, 1L))),
      beta_jw = list(c(0.3, 0.2, 0.3)),
      omega_tilde_jw = list(structure(c(0.6, NA, NA, NA), dim = c(2L, 2L)))
    )
  )

  # Case density forecast
  # Construct posterior of draw jx
  estimate <- extract_estimates_from_draws(
    system_of_equations, estimates, jx
  )

  result <- construct_posterior(
    system_of_equations, estimate, find_phi_positions(system_of_equations)
  )

  expected_gamma_matrix <-
    structure(c(
      1, 0, 0, 0, 0, 0.5, 0, 1, 0, 0, 0, 1.6, 0, 0, 1,
      0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0.22, 0, 0, 0, -0.5, -0.5, 1
    ), dim = c(6L, 6L), dimnames = list(c(
      "consumption", "investment",
      "current_account", "manufacturing", "service", "gdp"
    ), c(
      "consumption",
      "investment", "current_account", "manufacturing", "service",
      "gdp"
    )))

  expected_beta_matrix <-
    structure(c(
      1.2, 0.5, 0.2, 0, 0, 0, 0, 0, 0, 0, 2.3, 0, 0, 0.4,
      0, 0, 0, 0.3, 0, 0, 1.5, 0, 0, 0, -0.5, 0, 0, 0, 0.5, 0, 0.6,
      0, 0, 0, 0, 0.1, 0, 0, 0.25, 0, 0.3, 0, 0, 0, 0, 0, 0.2, 0, 0,
      0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    ), dim = c(10L, 6L), dimnames = list(
      c(
        "constant", "consumption.L(1)", "consumption.L(2)", "investment.L(1)",
        "current_account.L(1)", "manufacturing.L(1)", "service.L(1)",
        "real_interest_rate", "world_gdp", "population"
      ), c(
        "consumption", "investment",
        "current_account", "manufacturing", "service", "gdp"
      )
    ))

  expected_sigma_matrix <-
    structure(c(
      0.5, 0, 0, 0, 0, 0, 0, 0.9, 0, 0, 0, 0, 0, 0, 0.1,
      0, 0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0, 0, 0, 0.6, 0, 0, 0, 0, 0,
      0, 0
    ), dim = c(6L, 6L))

  expected_phi_matrix <-
    list(`1` = structure(c(
      0.5, 0, 0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0,
      0, 0, -0.5, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.2, 0,
      0, 0, 0, 0, 0, 0
    ), dim = c(6L, 6L)), `2` = structure(c(
      0.2, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    ), dim = c(6L, 6L)))


  expect_identical(result$gamma_matrix, expected_gamma_matrix)
  expect_identical(result$beta_matrix, expected_beta_matrix)
  expect_identical(unname(result$sigma_matrix), expected_sigma_matrix)
  expect_identical(result$phi_matrix, expected_phi_matrix)
})

test_that("mixed dynamic identity weights produce numeric model coefficients", {
  sys_eq <- system_of_equations(
    "a ~ x1, b ~ a, y == (nom_b)*b + (nom_x1)*x1",
    exogenous_variables = "x1"
  )
  identities <- update_identity_weights(
    list(y = list(b = 0.3, x1 = 0.7)),
    sys_eq$identities
  )
  gamma_matrix <- diag(3)
  dimnames(gamma_matrix) <- dimnames(sys_eq$character_gamma_matrix)
  beta_matrix <- matrix(
    0, nrow = nrow(sys_eq$character_beta_matrix),
    ncol = ncol(sys_eq$character_beta_matrix),
    dimnames = dimnames(sys_eq$character_beta_matrix)
  )

  coefficients <- update_estimates_with_weights(
    identities, gamma_matrix, beta_matrix
  )

  expect_identical(coefficients$gamma_matrix["b", "y"], -0.3)
  expect_true(is.numeric(coefficients$beta_matrix))
  expect_identical(coefficients$beta_matrix["x1", "y"], 0.7)
})

test_that("update_estimates_with_weights", {
  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == (nom_manufacturing/nom_gdp)*manufacturing + (nom_service/nom_gdp)*service + 1*current_account.L(1),
    real_interest_rate == 1*nominal_interest_rate - 1*inflation_rate"

  # Vector of exogenous variables
  exogenous_variables <- c(
    "nominal_interest_rate", "inflation_rate", "world_gdp", "population"
  )

  sys_eq <- system_of_equations(equations, exogenous_variables)

  gamma_matrix <- diag(ncol(sys_eq$character_gamma_matrix))
  colnames(gamma_matrix) <- colnames(sys_eq$character_gamma_matrix)
  rownames(gamma_matrix) <- rownames(sys_eq$character_gamma_matrix)
  beta_matrix <- matrix(
    0,
    nrow(sys_eq$character_beta_matrix),
    ncol(sys_eq$character_beta_matrix)
  )
  colnames(beta_matrix) <- colnames(sys_eq$character_beta_matrix)
  rownames(beta_matrix) <- rownames(sys_eq$character_beta_matrix)

  identities <- list(
    gdp = list(
      equation = "gdp==(nom_manufacturing/nom_gdp)*manufacturing+(nom_service/nom_gdp)*service+1*current_account.L(1)",
      components = list(
        manufacturing = "theta6_4",
        service = "theta6_5",
        `current_account.L(1)` = "theta6_5"
      ),
      weights = list(
        theta6_4 = 0.7,
        theta6_5 = 0.3,
        theta6_5 = 1
      ),
      matrix = list("gamma", "gamma", "beta")
    ),
    real_interest_rate = list(
      equation =
        "real_interest_rate==1*nominal_interest_rate+(-1)*inflation_rate",
      components = list(
        nominal_interest_rate = "theta7_8", inflation_rate = "theta7_9"
      ),
      weights = list(
        theta7_8 = 1,
        theta7_9 = -1
      ),
      matrix = list("beta", "beta")
    )
  )
  result <-
    update_estimates_with_weights(identities, gamma_matrix, beta_matrix)

  expected_gamma_matrix <- structure(c(
    1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
    -0.7, -0.3, 1, 0, 0, 0, 0, 0, 0, 0, 1
  ), dim = c(7L, 7L), dimnames = list(
    c(
      "consumption", "investment", "current_account", "manufacturing",
      "service", "gdp", "real_interest_rate"
    ), c(
      "consumption",
      "investment", "current_account", "manufacturing", "service",
      "gdp", "real_interest_rate"
    )
  ))

  expected_beta_matrix <- structure(c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0
  ), dim = c(
    11L,
    7L
  ), dimnames = list(c(
    "constant", "consumption.L(1)", "consumption.L(2)",
    "investment.L(1)", "current_account.L(1)", "manufacturing.L(1)",
    "service.L(1)", "nominal_interest_rate", "inflation_rate", "world_gdp",
    "population"
  ), c(
    "consumption", "investment", "current_account", "manufacturing",
    "service", "gdp", "real_interest_rate"
  )))

  expect_identical(result$gamma_matrix, expected_gamma_matrix)
  expect_identical(result$beta_matrix, expected_beta_matrix)
})

test_that("update_matrix_with_weights fails if weights are undefined", {
  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == (nom_manufacturing/nom_gdp)*manufacturing + (nom_service/nom_gdp)*service"

  # Vector of exogenous variables
  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(
    equations, exogenous_variables
  )

  gamma_matrix <- diag(ncol(sys_eq$character_gamma_matrix))
  colnames(gamma_matrix) <- colnames(sys_eq$character_gamma_matrix)
  rownames(gamma_matrix) <- rownames(sys_eq$character_gamma_matrix)
  beta_matrix <- matrix(
    0,
    nrow(sys_eq$character_beta_matrix),
    ncol(sys_eq$character_beta_matrix)
  )
  colnames(beta_matrix) <- colnames(sys_eq$character_beta_matrix)
  rownames(beta_matrix) <- rownames(sys_eq$character_beta_matrix)

  identities <- list(
    gdp = list(
      equation = "gdp==(nom_manufacturing/nom_gdp)*manufacturing+(nom_service/nom_gdp)*service",
      components = list(
        manufacturing = "theta6_4", service = "theta6_5"
      ),
      weights = list(
        theta6_4 = NULL,
        theta6_5 = 0.3
      ),
      matrix = list("beta", "beta")
    )
  )
  expect_error(
    update_estimates_with_weights(identities, gamma_matrix, beta_matrix),
    "gdp"
  )

  identities <- list(
    gdp = list(
      equation = "gdp==(nom_manufacturing/nom_gdp)*manufacturing+(nom_service/nom_gdp)*service",
      components = list(
        manufacturing = "theta6_4", service = "theta6_5"
      ),
      weights = list(
        theta6_4 = NULL,
        theta6_5 = 0.3
      )
    ),
    ngdp = list(
      equation = "ngdp==(nom_manufacturing/nom_gdp)*manufacturing+(nom_service/nom_gdp)*service",
      components = list(
        manufacturing = "theta6_4", service = "theta6_5"
      ),
      weights = list(
        theta6_4 = NULL,
        theta6_5 = 0.3
      )
    )
  )
})

test_that("construct_phi correctly returns phi matrix", {
  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == (nom_manufacturing/nom_gdp)*manufacturing + (nom_service/nom_gdp)*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  beta_matrix <- matrix(
    c(
      1.2, 2.3, 1.5, 0.6, 0.3, 0,
      0.5, 0, 0, 0, 0, 0,
      0.2, 0, 0, 0, 0, 0,
      0, 0.4, 0, 0, 0, 0,
      0, 0, -0.5, 0, 0, 0,
      0, 0, 0, 0.1, 0, 0,
      0, 0, 0, 0, 0.2, 0,
      0, 0.3, 0, 0, 0, 0,
      0, 0, 0.5, 0.25, 0, 0,
      0, 0, 0, 0, 0.3, 0
    ),
    nrow = 10, ncol = 6, byrow = TRUE, dimnames = list(
      c(
        "constant", "consumption.L(1)", "consumption.L(2)", "investment.L(1)",
        "current_account.L(1)", "manufacturing.L(1)", "service.L(1)",
        "real_interest_rate", "world_gdp", "population"
      ),
      c(
        "consumption", "investment", "current_account", "manufacturing",
        "service", "gdp"
      )
    )
  )

  expected_phi <- list(`1` = structure(c(
    0.5, 0, 0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0,
    0, 0, -0.5, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.2, 0,
    0, 0, 0, 0, 0, 0
  ), dim = c(6L, 6L)), `2` = structure(c(
    0.2, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ), dim = c(6L, 6L)))

  result <- construct_phi(find_phi_positions(sys_eq), beta_matrix)

  expect_identical(result, expected_phi)
})

# Phi matrices from a beta matrix that numbers its cells column by column, so
# every coefficient can be told apart.
phi_from_numbered_beta <- function(equations) {
  sys_eq <- system_of_equations(equations, "x")
  character_beta_matrix <- sys_eq$character_beta_matrix
  beta_matrix <- matrix(
    as.numeric(seq_along(character_beta_matrix)),
    nrow = nrow(character_beta_matrix),
    dimnames = dimnames(character_beta_matrix)
  )
  construct_phi(find_phi_positions(sys_eq), beta_matrix)
}

test_that("construct_phi finds a lag that an earlier equation does not use", {
  # beta rows: constant, a.L(1), a.L(2), x
  result <- phi_from_numbered_beta("a ~ a.L(1) + x, b ~ a.L(2) + x")

  expect_identical(result, list(
    `1` = matrix(c(2, 0, 0, 0), 2, 2),
    `2` = matrix(c(0, 0, 7, 0), 2, 2)
  ))
})

test_that("construct_phi tells apart variables that share a prefix", {
  # beta rows: constant, gdp.L(1), gdp_ch.L(1), x
  result <- phi_from_numbered_beta(
    "gdp ~ gdp.L(1) + x, gdp_ch ~ gdp_ch.L(1) + x"
  )

  expect_identical(result, list(`1` = matrix(c(2, 0, 0, 7), 2, 2)))
})

test_that("construct_phi handles digits in names and lags above 9", {
  # beta rows: constant, m2.L(1), y.L(12), x
  result <- phi_from_numbered_beta("m2 ~ m2.L(1) + x, y ~ y.L(12) + x")

  zero <- matrix(0, 2, 2)
  expected <- c(
    list(matrix(c(2, 0, 0, 0), 2, 2)),
    rep(list(zero), 10),
    list(matrix(c(0, 0, 0, 7), 2, 2))
  )
  names(expected) <- as.character(1:12)

  expect_identical(result, expected)
})

test_that("construct_phi returns the lags in order", {
  # beta rows: constant, a.L(2), b.L(1), x
  result <- phi_from_numbered_beta("a ~ a.L(2) + x, b ~ b.L(1) + x")

  expect_identical(result, list(
    `1` = matrix(c(0, 0, 0, 7), 2, 2),
    `2` = matrix(c(2, 0, 0, 0), 2, 2)
  ))
})

test_that("find_phi_positions needs the character beta matrix", {
  sys_eq <- system_of_equations("a ~ a.L(1) + x, b ~ a.L(2) + x", "x")
  sys_eq$character_beta_matrix <- NULL

  expect_error(find_phi_positions(sys_eq), "character_beta_matrix")
})

test_that("construct_phi with lagged identity", {
  equations <-
    "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp.L(1),
    gdp == (nom_manufacturing/nom_gdp)*manufacturing + (nom_service/nom_gdp)*service"

  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

  sys_eq <- system_of_equations(equations, exogenous_variables)

  beta_matrix <- matrix(
    c(
      1.2, 2.3, 1.5, 0.6, 0.3, 0,
      0.5, 0, 0, 0, 0, 0,
      0.2, 0, 0, 0, 0, 0,
      0, 0.4, 0, 0, 0, 0,
      0, 0, -0.5, 0, 0, 0,
      0, 0, 0, 0.1, 0, 0,
      0, 0, 0, 0, 0.2, 0,
      0, 0, 0, 0, 0.05, 0,
      0, 0.3, 0, 0, 0, 0,
      0, 0, 0.5, 0.25, 0, 0,
      0, 0, 0, 0, 0.3, 0
    ),
    nrow = 11, ncol = 6, byrow = TRUE, dimnames = list(
      c(
        "constant", "consumption.L(1)", "consumption.L(2)", "investment.L(1)",
        "current_account.L(1)", "service.L(1)", "manufacturing.L(1)", "gdp.L(1)",
        "real_interest_rate", "world_gdp", "population"
      ),
      c(
        "consumption", "investment", "current_account", "manufacturing",
        "service", "gdp"
      )
    )
  )

  expected_phi <- list(`1` = structure(c(
    0.5, 0, 0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0,
    0, 0, -0.5, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.2, 0.05,
    0, 0, 0, 0, 0, 0
  ), dim = c(6L, 6L)), `2` = structure(c(
    0.2, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ), dim = c(6L, 6L)))

  result <- construct_phi(find_phi_positions(sys_eq), beta_matrix)

  expect_identical(result, expected_phi)
})
