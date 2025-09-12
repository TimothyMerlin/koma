test_that("generate_sample_data returns the correct y and x matrices", {
  # set seed to generate the same multivariate distribution
  set.seed(3)

  # set up arguments of function to be tested
  sample_size <- 150
  sample_start <- c(1976, 1)
  burnin <- 50
  exogenous_variables <- c("real_interest_rate", "world_gdp", "population")
  predetermined_variables <- c(
    "consumption.L(1)", "consumption.L(2)",
    "investment.L(1)", "current_account.L(1)",
    "manufacturing.L(1)", "service.L(1)"
  )
  endogenous_variables <- c(
    "consumption",
    "investment",
    "current_account",
    "manufacturing",
    "service",
    "gdp"
  )

  # set up parameters
  gamma_matrix <- matrix(
    c(
      1, 0, 0, 0, 0, 0,
      0, 1, 0, 0, 0, 0,
      0, 0, 1, 0, 0, 0,
      0, 0, 0, 1, 0, 0.5,
      0, 0, 0, 0, 1, 0.5,
      0.5, 1.6, 0, 0, 0.22, 1
    ),
    nrow = 6, ncol = 6, byrow = TRUE
  )
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
    nrow = 10, ncol = 6, byrow = TRUE
  )
  sigma_matrix <- diag(c(0.5, 0.9, 0.1, 0.4, 0.6, 0))

  generated_data <- generate_sample_data(
    sample_size,
    sample_start,
    burnin,
    gamma_matrix,
    beta_matrix,
    sigma_matrix,
    endogenous_variables,
    exogenous_variables,
    predetermined_variables
  )

  # we only test the last six observations to keep data small
  expected_x_matrix_145_150 <- structure(c(
    1, 1, 1, 1, 1, 1, 4.93061214425343, 4.49775163345672,
    5.38009929369495, 5.28431508802181, 5.89180222389651, 3.53198440212836,
    4.56585967699961, 4.93061214425343, 4.49775163345672, 5.38009929369495,
    5.28431508802181, 5.89180222389651, 4.91740878474051, 5.24757929178648,
    3.67177736359915, 5.56895281915473, 6.8175507293137, 5.41747333056895,
    0.207209319171355, 2.04006484630929, -0.175133089884501, 1.72391059283339,
    -0.034401166204305, 1.70548170284277, 0.216807612367463, 0.545185694438725,
    0.444549626625126, 0.322209137280523, -0.330362168226146, 1.48626291336813,
    0.83533993208071, 1.40256860562495, 0.471493301299334, 0.505508104402123,
    1.81078347187344, -1.7693349996185, 1.01208654795973, -0.579270779925403,
    -0.788640019088879, -0.0384044689109903, 1.52762863677198,
    0.111037002660126, 0.853299700548911, -0.380950067294774,
    -0.084708837901136, 0.172339523868407, 1.75658725906422, 0.352126004196338,
    0.34868487171889, -0.407383219978553, -1.01599701589685,
    -0.0694116202925416, -0.453397945160182, -2.14771506938501
  ), dim = c(6L, 10L), dimnames = list(NULL, c(
    "constant", "consumption.L(1)",
    "consumption.L(2)", "investment.L(1)", "current_account.L(1)", "manufacturing.L(1)",
    "service.L(1)", "real_interest_rate", "world_gdp", "population"
  )))

  expected_y_matrix_145_150 <- structure(c(
    4.49775163345672, 5.38009929369495, 5.28431508802181,
    5.89180222389651, 3.53198440212836, 4.44645377197101, 5.24757929178648,
    3.67177736359915, 5.56895281915473, 6.8175507293137, 5.41747333056895,
    6.15737145556916, 2.04006484630929, -0.175133089884501, 1.72391059283339,
    -0.034401166204305, 1.70548170284277, 0.929300546350757, 0.545185694438725,
    0.444549626625126, 0.322209137280523, -0.330362168226146, 1.48626291336813,
    1.6147895459246, 1.40256860562495, 0.471493301299334, 0.505508104402123,
    1.81078347187344, -1.7693349996185, 0.292150434179864, -0.973877150031836,
    -0.45802146396223, -0.413858620841322, -0.740210651823645,
    0.141536043125185, -0.953469990052231
  ), dim = c(6L, 6L), dimnames = list(NULL, c(
    "consumption",
    "investment", "current_account", "manufacturing", "service",
    "gdp"
  )))

  expect_equal(generated_data$x_matrix[145:150, ], expected_x_matrix_145_150)
  expect_equal(generated_data$y_matrix[145:150, ], expected_y_matrix_145_150)
})

test_that("get_lagged_values correctly updates predetermined matrix", {
  predetermined_matrix <- matrix(0, nrow = 5, ncol = 2)
  colnames(predetermined_matrix) <- c("construction.L(1)", "construction.L(2)")

  y_matrix <- matrix(1:10, nrow = 5, ncol = 2)
  colnames(y_matrix) <- c("construction", "gdp")

  # We update the predetermined values for the row 3
  tx <- 3
  updated_matrix <- get_lagged_values(predetermined_matrix, y_matrix, tx)

  expected_matrix <- matrix(c(0, 0, 0, 3, 0, 0, 0, 0, 0, 3), nrow = 5, ncol = 2)
  colnames(expected_matrix) <- c("construction.L(1)", "construction.L(2)")
  expect_identical(updated_matrix, expected_matrix)
})
