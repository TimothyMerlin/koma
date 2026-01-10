# Generate a Forecast for a Single Draw

This function computes a forecast for a single draw of the parameter
estimates, supporting both point forecasts (mean or median) and density
forecasts. It constructs the posterior distribution, companion matrix,
and reduced-form representation of the system before computing
forecasts.

## Usage

``` r
forecast_single_draw(
  sys_eq,
  estimates,
  jx,
  y_matrix,
  forecast_x_matrix,
  horizon,
  freq,
  forecast_dates,
  restrictions,
  state,
  central_tendency = NULL
)
```

## Arguments

- sys_eq:

  A `koma_seq` object
  ([system_of_equations](https://timothymerlin.github.io/koma/reference/system_of_equations.md))
  containing details about the system of equations used in the model.

- estimates:

  List of parameter estimates for the system.

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

- restrictions:

  List of model constraints. Default is empty.

- state:

  An environment used to share mutable state between function calls,
  particularly for issuing warnings only once during the forecasting
  process.
