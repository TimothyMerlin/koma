# Construct Balanced Data

Construct Balanced Data

## Usage

``` r
construct_balanced_data(
  ts_data,
  endogenous_variables,
  total_exogenous_variables,
  start,
  end,
  state = NULL
)
```

## Arguments

- ts_data:

  Time series data.

- endogenous_variables:

  Endogenous variables.

- total_exogenous_variables:

  Exogenous and / or predetermined variables to include in x matrix.

- start:

  Start date for truncation.

- end:

  End date for truncation.

- state:

  An environment used to share mutable state between function calls,
  particularly for issuing warnings only once during the forecasting
  process.

## Value

A list containing the truncated time series data, y_matrix, x_matrix the
number of observations, the date of last observation and the frequency.
