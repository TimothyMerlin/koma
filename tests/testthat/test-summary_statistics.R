test_that("summary_statistics works", {
  # equation: consumption = constant + consumption.L(1) + consumption.L(2) + gdp # nolint
  endogenous_variables <- "consumption"
  estimates <- simulated_data$estimates
  system_of_equations <- simulated_data$sys_eq

  ## Case 1: Default - Returns results as an ASCI table.
  result <- summary_statistics(
    endogenous_variables,
    estimates,
    system_of_equations
  )

  expected_result <- list(consumption = list(coef.names = c(
    "constant", "consumption.L(1)",
    "consumption.L(2)", "gdp"
  ), coef = c(
    constant = 1.31322216139783,
    `consumption.L(1)` = 0.484287348595653,
    `consumption.L(2)` = 0.203404281058472,
    gdp = -0.367543792811032
  ), ci.low = c(0.766708163554736, 0.375690517877449,
    0.0920661493549338,
    `5%` = -0.671805581196672
  ), ci.up = c(1.8641199520759,
    0.591091226099493, 0.308526113032514,
    `95%` = -0.0865327358691923
  ), pvalues = numeric(0), model.name = "consumption"))

  expect_equal(result, expected_result, tolerance = 1e-1)

  ## Case 2: Returns median as central tendency measure
  result <- summary_statistics(
    endogenous_variables,
    estimates,
    system_of_equations,
    central_tendency = "median"
  )

  expected_result <- list(consumption = list(coef.names = c(
    "constant", "consumption.L(1)",
    "consumption.L(2)", "gdp"
  ), coef = c(
    constant = 1.30198086868547,
    `consumption.L(1)` = 0.482808139140976,
    `consumption.L(2)` = 0.203018478345194,
    gdp = -0.358204333963515
  ), ci.low = c(0.766708163554736, 0.375690517877449,
    0.0920661493549338,
    `5%` = -0.671805581196672
  ), ci.up = c(1.8641199520759,
    0.591091226099493, 0.308526113032514,
    `95%` = -0.0865327358691923
  ), pvalues = numeric(0), model.name = "consumption"))

  expect_equal(result, expected_result, tolerance = 1e-1)

  ## Case 3: Change confidence interval
  result <- summary_statistics(
    endogenous_variables,
    estimates,
    system_of_equations,
    ci_low = 1,
    ci_up = 99
  )

  expected_result <- list(consumption = list(coef.names = c(
    "constant", "consumption.L(1)",
    "consumption.L(2)", "gdp"
  ), coef = c(
    constant = 1.31322216139783,
    `consumption.L(1)` = 0.484287348595653,
    `consumption.L(2)` = 0.203404281058472,
    gdp = -0.367543792811032
  ), ci.low = c(0.546664449167103, 0.32921604415603,
    0.0404815218094497,
    `1%` = -0.857507373914667
  ), ci.up = c(2.04526696125366,
    0.628059082263212, 0.350866423143984,
    `99%` = 0.0687091729120636
  ), pvalues = numeric(0), model.name = "consumption"))

  expect_equal(result, expected_result, tolerance = 1e-1)
})

test_that("summary_statistics equation without endogenous variables", {
  # equation: current_account = constant + current_account.L(1) + world_gdp # nolint
  endogenous_variables <- "current_account"
  estimates <- simulated_data$estimates
  system_of_equations <- simulated_data$sys_eq

  result <- summary_statistics(
    endogenous_variables,
    estimates,
    system_of_equations
  )

  expected_result <- list(current_account = list(
    coef.names = c(
      "constant", "current_account.L(1)",
      "world_gdp"
    ), coef = c(
      constant = 1.53894277991306, `current_account.L(1)` = -0.519185374109944,
      world_gdp = 0.520924405307531
    ), ci.low = c(
      1.47305792647342,
      -0.573028861836006, 0.486965688727344
    ), ci.up = c(
      1.60224811029465,
      -0.462318499023928, 0.55490196878073
    ), pvalues = numeric(0),
    model.name = "current_account"
  ))
  expect_equal(result, expected_result)
})

test_that("summary_statistics works for multiple variables", {
  endogenous_variables <- c("consumption", "investment")
  estimates <- simulated_data$estimates
  system_of_equations <- simulated_data$sys_eq

  result <- summary_statistics(
    endogenous_variables,
    estimates,
    system_of_equations
  )

  expected_result <- list(consumption = list(coef.names = c(
    "constant", "consumption.L(1)",
    "consumption.L(2)", "gdp"
  ), coef = c(
    constant = 1.29555201117066,
    `consumption.L(1)` = 0.493640051114065, `consumption.L(2)` = 0.198451632513463,
    gdp = -0.356162420823345
  ), ci.low = c(0.763411504413967, 0.379775410010346,
    0.0871539159456293,
    `5%` = -0.659881904565181
  ), ci.up = c(1.83612284904621,
    0.608410836528798, 0.307837464941855,
    `95%` = -0.0756522273055594
  ), pvalues = numeric(0), model.name = "consumption"), investment = list(
    coef.names = c(
      "constant", "investment.L(1)", "real_interest_rate",
      "gdp"
    ), coef = c(
      constant = 2.35706500298874, `investment.L(1)` = 0.425800031143992,
      real_interest_rate = 0.357807411087837, gdp = -1.30860809068447
    ), ci.low = c(1.957238031167, 0.3508699279918, 0.242559246156005,
      `5%` = -1.73482270679573
    ), ci.up = c(2.79200976043857, 0.499433516080141,
      0.474945636597825,
      `95%` = -0.914963458563659
    ), pvalues = numeric(0),
    model.name = "investment"
  ))

  expect_equal(result, expected_result)
})

test_that("summary_statistics throws error", {
  # Case endogenous variable not contained in system.
  endogenous_variables <- "x"
  estimates <- simulated_data$estimates
  system_of_equations <- simulated_data$sys_eq

  expect_error(summary_statistics(
    endogenous_variables,
    estimates,
    system_of_equations
  ), "x")


  # Case endogenous variables missing in estimates
  endogenous_variables <- "consumption"
  estimates$consumption <- NULL

  expect_error(summary_statistics(
    endogenous_variables,
    estimates,
    system_of_equations
  ), "consumption")
})
