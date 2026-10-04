# Generate Data for the Simultaneous Equations Model

Compute Y and X matrix (data) Use reduced form of the model to compute Y
\\Y = X B \Gamma^{-1} + U \Gamma^{-1}\\

## Usage

``` r
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
```

## Arguments

- sample_size:

  The number of observations.

- sample_start:

  A vector of format `c(YEAR, QUARTER)` representing

- burnin:

  The number of observations to discard to mitigate the dependency on
  starting values.

- gamma_matrix:

  A matrix that defines the relationships between the variables.

- beta_matrix:

  A matrix that defines the coefficients of the equation.

- sigma_matrix:

  A matrix that defines the standard deviation of the error terms.

- endogenous_variables:

  A vector of endogenous variables.

- exogenous_variables:

  A vector of exogenous variables.

- predetermined_variables:

  A vector of predetermined variables.

## Value

A list containing three elements:

- y_matrix: The dependent variable matrix with newly generated data.

- x_matrix: The matrix of exogenous and predetermined variables with
  newly generated data.

- ts_data: A list containing time series in growth rates.

## Details

It creates a sample of data where the relationship between the variables
is determined by the defined parameters.

## Examples

``` r
gamma_matrix <- matrix(1, nrow = 1)
beta_matrix <- matrix(c(0.2, 0.5, 0.3), nrow = 3)
sigma_matrix <- matrix(0.01, nrow = 1)

sample <- generate_sample_data(
  sample_size = 12,
  sample_start = c(2000, 1),
  burnin = 4,
  gamma_matrix = gamma_matrix,
  beta_matrix = beta_matrix,
  sigma_matrix = sigma_matrix,
  endogenous_variables = "y",
  exogenous_variables = "x",
  predetermined_variables = "y.L(1)"
)
names(sample)
#> [1] "y_matrix" "x_matrix" "ts_data" 
```
