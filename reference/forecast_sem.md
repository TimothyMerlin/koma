# Generate Forecasts for a System of Equations

This function performs forecasting based on the provided system of
equations, estimates, and other parameters. It supports both density and
point forecasting.

## Usage

``` r
forecast_sem(
  sys_eq,
  estimates,
  restrictions,
  y_matrix,
  forecast_x_matrix,
  horizon,
  freq,
  forecast_dates,
  approximate,
  probs,
  conditional_innov_method = "projection"
)
```

## Arguments

- sys_eq:

  A `koma_seq` object
  ([system_of_equations](https://timothymerlin.github.io/koma/reference/system_of_equations.md))
  containing details about the system of equations used in the model.

- estimates:

  List of parameter estimates for the system.

- restrictions:

  List of model constraints. Default is empty.

- y_matrix:

  Matrix of the dependent variable time series data.

- forecast_x_matrix:

  A matrix with forecasting data for exogenous variables.

- horizon:

  Forecasting horizon, specifying the number of periods.

- freq:

  Frequency of the time series data.

- forecast_dates:

  List containing 'start' and 'end' dates for forecast.

## Value

A list containing the forecast values.
