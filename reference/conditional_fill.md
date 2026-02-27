# Conditional Fill and Forecast for Time Series Data

This function fills missing observations and extends the time series
data up to and including the current quarter. The function conditionally
forecasts based on the estimates and realized observations.

## Usage

``` r
conditional_fill(ts_data, sys_eq, dates, estimates, fill_method)
```

## Arguments

- ts_data:

  Time-series data set for the estimation.

- sys_eq:

  A `koma_seq` object
  ([system_of_equations](https://timothymerlin.github.io/koma/reference/system_of_equations.md))
  containing details about the system of equations used in the model.

- dates:

  Key-value list for date ranges in various model operations.

- estimates:

  Optional. A `koma_estimate` object (see
  [`estimate`](https://timothymerlin.github.io/koma/reference/estimate.md))
  containing the estimates of the previously estimated simultaneous
  equations model. Use this parameter when some equations of the system
  need to be re-estimated.

- fill_method:

  Character string indicating which central tendency measure ("mean" or
  "median") to use when filling ragged edges.

## Value

An extended time series list containing the filled and forecasted time
series up to the current quarter.
