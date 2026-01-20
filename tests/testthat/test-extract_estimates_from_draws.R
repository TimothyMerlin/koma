test_that("extract_estimates_... returns median or mean estimates", {
  # if jx, i.e. the draw index, is not set
  sys_eq <- list(
    endogenous_variables = NULL,
    total_exogenous_variables = NULL,
    identity_equations = NULL,
    identities = NULL,
    character_gamma_matrix = list(gamma_matrix = NULL)
  )
  sys_eq$equations <-
    simulated_data$equations
  sys_eq$endogenous_variables <-
    simulated_data$endogenous_variables
  sys_eq$total_exogenous_variables <-
    simulated_data$total_exogenous_variables
  sys_eq$character_gamma_matrix <-
    simulated_data$character_gamma_matrix
  sys_eq$character_beta_matrix <-
    simulated_data$character_beta_matrix

  sys_eq$identities <- list(
    gdp = list(
      equation = "gdp==manufacturing+service",
      components = list(
        manufacturing = "theta6_4", service = "theta6_5"
      ),
      weights = list(
        theta6_4 = 0.5,
        theta6_5 = 0.5
      )
    )
  )

  estimates <- simulated_data$estimates
  ## Case 1: Returns mean estimates
  result <- extract_estimates_from_draws(
    sys_eq, estimates
  )

  expected_gamma_matrix <- structure(
    c(
      1, 0, 0, 0, 0, 0.356162420823345, 0, 1, 0, 0, 0,
      1.30860809068447, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
      0, 1, 0.800892315414132, 0, 0, 0, 0, 0, 1
    ),
    dim = c(6L, 6L), dimnames = list(
      c(
        "consumption", "investment", "current_account", "manufacturing",
        "service", "gdp"
      ), c(
        "consumption", "investment", "current_account",
        "manufacturing", "service", "gdp"
      )
    )
  )

  expected_beta_matrix <- structure(
    c(
      1.29555201117066, 0.493640051114065, 0.198451632513463,
      0, 0, 0, 0, 0, 0, 0, 2.35706500298874, 0, 0, 0.425800031143992,
      0, 0, 0, 0.357807411087837, 0, 0, 1.53894277991306, 0, 0, 0,
      -0.519185374109944, 0, 0, 0, 0.520924405307531, 0, 0.611418739845237,
      0, 0, 0, 0, 0.0228662878484722, 0, 0, 0.330210117528094, 0, 0.0140231663078636,
      0, 0, 0, 0, 0, 0.142844446226057, 0, 0, 0.0817021534919335, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0
    ),
    dim = c(10L, 6L),
    dimnames = list(
      c(
        "constant", "consumption.L(1)", "consumption.L(2)", "investment.L(1)",
        "current_account.L(1)", "manufacturing.L(1)", "service.L(1)", "real_interest_rate",
        "world_gdp", "population"
      ), c(
        "consumption", "investment",
        "current_account", "manufacturing", "service", "gdp"
      )
    )
  )

  expected_sigma_matrix <- diag(c(
    structure(
      c(
        0.593903535536046, 1.08156143798434, 0.0867852365774461,
        0.449002137976062, 0.338599060353756, 0
      ),
      dim = c(1L, 6L)
    )
  ))

  expect_equal(result$gamma_matrix, expected_gamma_matrix)
  expect_equal(result$beta_matrix, expected_beta_matrix)
  expect_equal(result$sigma_matrix, expected_sigma_matrix)

  # Case 2: Returns median estimates
  result <- extract_estimates_from_draws(
    sys_eq, simulated_data$estimates,
    central_tendency = "median"
  )

  expected_gamma_matrix <- structure(
    c(
      1, 0, 0, 0, 0, 0.346839608272198, 0, 1, 0, 0, 0,
      1.29552523237189, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
      0, 1, 0.793020504530468, 0, 0, 0, 0, 0, 1
    ),
    dim = c(6L, 6L),
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

  expected_beta_matrix <- structure(
    c(
      1.28728023126327, 0.493465501033715, 0.197603396719306,
      0, 0, 0, 0, 0, 0, 0, 2.34706509476837, 0, 0, 0.425340562631935,
      0, 0, 0, 0.356596789574403, 0, 0, 1.53870974142649, 0, 0, 0,
      -0.519565667402154, 0, 0, 0, 0.520594984880779, 0, 0.610221183268151,
      0, 0, 0, 0, 0.0228420577882731, 0, 0, 0.328814635228246, 0, 0.0129771628678928,
      0, 0, 0, 0, 0, 0.14173990114163, 0, 0, 0.0805609787873833, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0
    ),
    dim = c(10L, 6L),
    dimnames = list(
      c(
        "constant", "consumption.L(1)", "consumption.L(2)", "investment.L(1)",
        "current_account.L(1)", "manufacturing.L(1)", "service.L(1)",
        "real_interest_rate", "world_gdp", "population"
      ), c(
        "consumption",
        "investment", "current_account", "manufacturing", "service",
        "gdp"
      )
    )
  )

  expected_sigma_matrix <- diag(c(structure(
    c(
      0.588795874201523, 1.06672034759151, 0.0862706544652745,
      0.446339836441364, 0.330434206917935, 0
    ),
    dim = c(1L, 6L)
  )))

  expect_equal(result$gamma_matrix, expected_gamma_matrix)
  expect_equal(result$beta_matrix, expected_beta_matrix)
  expect_equal(result$sigma_matrix, expected_sigma_matrix)
})

test_that("extract_estimates_... returns estimates for draw jx", {
  sys_eq <- list(
    endogenous_variables = NULL,
    total_exogenous_variables = NULL,
    identity_equations = NULL,
    identities = NULL,
    character_gamma_matrix = list(gamma_matrix = NULL)
  )
  sys_eq$equations <-
    simulated_data$equations
  sys_eq$endogenous_variables <-
    simulated_data$endogenous_variables
  sys_eq$total_exogenous_variables <-
    simulated_data$total_exogenous_variables
  sys_eq$character_gamma_matrix <-
    simulated_data$character_gamma_matrix
  sys_eq$character_beta_matrix <-
    simulated_data$character_beta_matrix

  sys_eq$identities <- list(
    gdp = list(
      equation = "gdp==manufacturing+service",
      components = list(
        manufacturing = "theta6_4", service = "theta6_5"
      ),
      weights = list(
        theta6_4 = 0.5,
        theta6_5 = 0.5
      )
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

  result <- extract_estimates_from_draws(sys_eq, estimates, jx)

  # We expect gamma matrix without weights, instead there are 0s where the
  # weights should be.
  expected_gamma_matrix <-
    structure(c(
      1, 0, 0, 0, 0, 0.5, 0, 1, 0, 0, 0, 1.6, 0, 0, 1,
      0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0.22, 0, 0, 0, 0,
      0, 1
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

  expect_identical(result$gamma_matrix, expected_gamma_matrix)
  expect_identical(result$beta_matrix, expected_beta_matrix)
  expect_identical(result$sigma_matrix, expected_sigma_matrix)
})
