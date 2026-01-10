# Calculate Dynamic Weights for Identity Equations

Computes the weights required for identity equations using time series
data. The weights represent how components collectively sum up to or are
related to an aggregate value over the specified time period.

## Usage

``` r
calculate_eq_weights(ts_data, iden, start, end)
```

## Arguments

- ts_data:

  A named list of [`stats::ts`](https://rdrr.io/r/stats/ts.html)
  objects. It should contain the time series data for both the aggregate
  value and its components. Ensure that `ts_data` contains the time
  series listed in `iden`.

- start:

  A numeric value representing the start date for the time series data
  subset. This value should be in the numeric format used by the
  function `dates_to_num`.

- end:

  A numeric value representing the end date for the time series data
  subset, in the same numeric format as `start`.

## Value

A list of [`stats::ts`](https://rdrr.io/r/stats/ts.html) objects
containing the calculated weights for the given aggregate value and its
components. The top-level name of the list is the aggregate value, and
its sub-level names correspond to the component names.
