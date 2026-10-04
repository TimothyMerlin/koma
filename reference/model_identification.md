# Identify Model Parameters from Character Matrices

This function calculates the gamma and beta matrices based on the given
character gamma and beta matrices, along with identity weights. It
checks the identification of all stochastic equations and verifies the
order and rank conditions for model identification.

## Usage

``` r
model_identification(
  character_gamma_matrix,
  character_beta_matrix,
  identity_weights,
  call = rlang::caller_env()
)
```

## Arguments

- character_gamma_matrix:

  A character matrix representing the gamma structure of the model.

- character_beta_matrix:

  A character matrix representing the beta structure of the model.

- identity_weights:

  A list of identity weights to help construct constant vectors.

- call:

  The environment from which the error is called. Defaults to
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html),
  and is used to provide context in case of an error.

## Value

NULL. This function is used for side effects, stopping execution if
model identification conditions are not met.

## Examples

``` r
equations <- "consumption ~ gdp + consumption.L(1) + consumption.L(2),
investment ~ gdp + investment.L(1) + real_interest_rate,
current_account ~ current_account.L(1) + world_gdp,
manufacturing ~ manufacturing.L(1) + world_gdp,
service ~ service.L(1) + population + gdp,
gdp == 0.5*manufacturing + 0.5*service"

exogenous_variables <- c("real_interest_rate", "world_gdp", "population")
sys_eq <- system_of_equations(equations, exogenous_variables)

model_identification(
  sys_eq$character_gamma_matrix,
  sys_eq$character_beta_matrix,
  sys_eq$identities
)
#> [1] TRUE
```
