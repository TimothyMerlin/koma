# Calculate the Forecast Error

This function calculates the the desired forecast errors for the desired
variables given the true values and the forecasts

## Usage

``` r
calculate_error(forecasts, realized, variables)
```

## Arguments

- forecasts:

  A mets-object containing the forecasts.

- realized:

  A mets-object containing the true values.

- variables:

  A character vector of name(s) of the stochastic endogenous variable
  for which the forecast error(s) should be calculated.

## Value

List of errors
