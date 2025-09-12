#' Economic Data Simulation for KoMa Package
#'
#' @description
#' This script generates a simulated economic dataset based on a
#' simultaneous equations model. The simulated data serves two primary
#' roles within the KoMa package:
#' 1. As a concise sample dataset for users to explore package functionalities.
#' 2. For internal package unit tests to ensure code integrity.
#'
#' @details
#' The script sets up the following:
#' 1. System of Equations: Includes GDP, consumption, investment, etc.
#' 2. Endogenous and Exogenous Variables: e.g., real_interest_rate, world_gdp.
#' 3. Lagged Variables: Variables like consumption.L(1), consumption.L(2), etc.
#' 4. Gamma and Beta Matrices: Matrices for model specifications.
#' 5. Simulation: Uses the reduced form of the model to generate data.
#'
#' @output
#' - simulated_data: A list containing matrices, variables, and estimates.
#'
#' @usage
#' To use, run `devtools::load_all()` and then execute the script.
#' Make sure to set your seed for reproducibility; the default is set.seed(7).
#'
#' @seealso R/data.R

devtools::load_all()

seed <- 7

## Write down system of equations
# Important note:
# 1. Left-hand side of equations have to match endogenous variables vector
# 2. "epsilon" flags the stochastic equations. Thus, every equation without
#    an "epsilon" will be treated as an identity equation
# 3. Lagged variables can be introduced by using XLx for variable X and lag
#    Lx with x L=1,2,...
# 4. All stochastic equations have an intercept (conx*onet) for x=1,2,...

raw_equations <-
  "consumption ~ gdp + consumption.L(1) + consumption.L(2),
    investment ~ gdp + investment.L(1) + real_interest_rate,
    current_account ~ current_account.L(1) + world_gdp,
    manufacturing ~ manufacturing.L(1) + world_gdp,
    service ~ service.L(1) + population + gdp,
    gdp == 0.4*manufacturing + 0.6*service"

# Vector of exogenous variables
exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

sys_eq <- system_of_equations(raw_equations, exogenous_variables)

equations <- sys_eq$equations
endogenous_variables <- sys_eq$endogenous_variables
stochastic_equations <- sys_eq$stochastic_equations
identities <- sys_eq$identities
predetermined_variables <- sys_eq$predetermined_variables
total_exogenous_variables <- sys_eq$total_exogenous_variables
character_gamma_matrix <- sys_eq$character_gamma_matrix
character_beta_matrix <- sys_eq$character_beta_matrix

#### Simulation
identity_weights <- list(gdp = c(theta6_4 = 0.5, theta6_5 = 0.5))

gamma_matrix <- matrix(
  c(
    1, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0,
    0, 0, 0, 1, 0, 0.5,
    0, 0, 0, 0, 1, 0.5,
    0.5, 1.6, 0, 0, 0.22, 1
  ),
  nrow = 6, ncol = 6, byrow = TRUE, dimnames = list(
    c(
      "consumption", "investment", "current_account", "manufacturing",
      "service", "gdp"
    ), c(
      "consumption", "investment", "current_account",
      "manufacturing", "service", "gdp"
    )
  )
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
# Fix sigma_matrix matrix
sigma_matrix <- diag(c(0.5, 0.9, 0.1, 0.4, 0.6, 0))
colnames(sigma_matrix) <- c(
  "consumption", "investment", "current_account", "manufacturing",
  "service", "gdp"
)
rownames(sigma_matrix) <- c(
  "consumption", "investment", "current_account",
  "manufacturing", "service", "gdp"
)

# Compute Y and X matrix (data)
# Use reduced form of the model to compute Y
# Y = X*B*Gamma^(-1) + U*Gamma^{-1} # nolint: commented_code_linter.
sample_size <- 200 # sample size
sample_start <- c(1976, 1)
burnin <- 50

generated_data <- withr::with_seed(
  seed,
  generate_sample_data(
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
)

ts_data <- lapply(
  generated_data$ts_data,
  function(x) {
    as_ets(x,
      series_type = "rate", method = "percentage", value_type = "real"
    )
  }
)

# Estimate coefficients
ix <- which(endogenous_variables %in% names(identities))

##### Fix environment variables for test
## Gibbs sampler specifications
the$gibbs_sampler <- list(
  consumption = set_gibbs_spec(),
  investment = set_gibbs_spec(),
  current_account = set_gibbs_spec(),
  manufacturing = set_gibbs_spec(),
  service = set_gibbs_spec()
)
set_gibbs_settings(settings = NULL, sys_eq$equation_settings)

estimates <- vector(
  "list",
  length(endogenous_variables[-ix])
)
names(estimates) <- endogenous_variables[-ix]

for (jx in seq_along(endogenous_variables[-ix])) {
  estimates[[jx]] <- withr::with_seed(
    seed,
    draw_parameters_j(
      generated_data$y_matrix,
      generated_data$x_matrix,
      character_gamma_matrix,
      character_beta_matrix,
      jx
    )
  )
}

# Saving data
simulated_data <- list()
simulated_data$raw_equations <- raw_equations
simulated_data$equations <- equations
simulated_data$exogenous_variables <- exogenous_variables
simulated_data$predetermined_variables <- predetermined_variables
simulated_data$endogenous_variables <- endogenous_variables
simulated_data$identities <- identities
simulated_data$identity_weights <- identity_weights
simulated_data$total_exogenous_variables <- total_exogenous_variables
simulated_data$character_gamma_matrix <- character_gamma_matrix
simulated_data$character_beta_matrix <- character_beta_matrix
simulated_data$y_matrix <- generated_data$y_matrix
simulated_data$x_matrix <- generated_data$x_matrix
simulated_data$ts_data <- ts_data
simulated_data$beta_matrix <- beta_matrix
simulated_data$gamma_matrix <- gamma_matrix
simulated_data$sigma_matrix <- sigma_matrix
simulated_data$estimates <- estimates
simulated_data$the <- the
simulated_data$sys_eq <- sys_eq

usethis::use_data(simulated_data, overwrite = TRUE, internal = TRUE)
