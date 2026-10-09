# Shorten the Forecast Horizon to the Available Exogenous Data

If the exogenous variables end before the forecast end date, the horizon
is shortened to the number of periods with complete exogenous data, and
a warning names the variables that end early. Missing values that are
followed by data, or that start in the first forecast period, cannot be
handled by shortening the horizon and raise an error.

## Usage

``` r
shorten_forecast_horizon(horizon, forecast_x_matrix)
```

## Arguments

- horizon:

  Forecasting horizon, specifying the number of periods.

- forecast_x_matrix:

  A matrix with forecasting data for exogenous variables.

## Value

The (possibly shortened) forecast horizon.
