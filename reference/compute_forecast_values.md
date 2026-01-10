# Compute Forecast Based on Companion Matrix and Reduced Form

This function calculates the forecast for a given set of parameters and
data. It computes baseline forecasts and applies certain restrictions if
specified.

## Usage

``` r
compute_forecast_values(
  companion_matrix,
  reduced_form,
  y_matrix,
  forecast_x_matrix,
  horizon,
  freq,
  start_forecast,
  endogenous_variables,
  restrictions
)
```

## Arguments

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

## Value

A matrix with the base forecast of Y.
