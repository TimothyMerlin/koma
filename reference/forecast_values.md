# Compute Forecast Based on Companion Matrix and Reduced Form

This function calculates the forecast for a given set of parameters and
data. It computes baseline forecasts and applies certain restrictions if
specified.

## Usage

``` r
forecast_values(
  posterior,
  companion_matrix,
  reduced_form,
  y_matrix,
  forecast_x_matrix,
  horizon,
  freq,
  start_forecast,
  endogenous_variables,
  restrictions,
  identities,
  conditional_innov_method = "projection",
  stochastic = FALSE
)
```

## Arguments

- posterior:

  A list of posterior system matrices from
  [`construct_posterior()`](https://timothymerlin.github.io/koma/reference/construct_posterior.md)
  (e.g., `gamma_matrix`, `sigma_matrix`).

- companion_matrix:

  A list containing components of the companion matrix.

- reduced_form:

  A list containing the reduced form components.

- y_matrix:

  Matrix of the dependent variable time series data.

- forecast_x_matrix:

  A matrix with forecasting data for exogenous variables.

- horizon:

  Forecasting horizon, specifying the number of periods.

- freq:

  Frequency of the time series data.

- start_forecast:

  A vector of format `c(YEAR, QUARTER)` representing the start date of
  the forecast.

- endogenous_variables:

  A character vector containing the names of endogenous variables.

- restrictions:

  List of model constraints. Default is empty.

- stochastic:

  Logical; when `TRUE`, returns a stochastic forecast draw.

## Value

A matrix with the base forecast of Y.
