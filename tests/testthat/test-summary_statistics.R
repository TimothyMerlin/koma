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
      constant = 1.53928461251418, `current_account.L(1)` = -0.518768432127631,
      world_gdp = 0.520809474662241
    ), ci.low = c(
      1.46993236929141,
      -0.572270239900139, 0.486965688727344
    ), ci.up = c(
      1.60308022223233,
      -0.465555830190864, 0.554532804746407
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
    constant = 1.2973694834906,
    `consumption.L(1)` = 0.492748770087562, `consumption.L(2)` = 0.198912926268343,
    gdp = -0.356162420833674
  ), ci.low = c(0.765434375884618, 0.383233571927267,
    0.0876699532538459,
    `5%` = -0.659881904975434
  ), ci.up = c(1.8295569302224,
    0.596044135914781, 0.309787548305748,
    `95%` = -0.0756522269465259
  ), pvalues = numeric(0), model.name = "consumption"), investment = list(
    coef.names = c(
      "constant", "investment.L(1)", "real_interest_rate",
      "gdp"
    ), coef = c(
      constant = 2.35821006497356, `investment.L(1)` = 0.425185609249721,
      real_interest_rate = 0.358011851071844, gdp = -1.30860809067019
    ), ci.low = c(1.95528358867594, 0.351167542741454, 0.239954874322291,
      `5%` = -1.73482270604464
    ), ci.up = c(2.78838428386522, 0.499433516035207,
      0.475483516957269,
      `95%` = -0.91496345922988
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
