# Get System of Equations Weights

Calculates dynamic weights for a system of equations and updates the
identities with these weights. The function processes a list of time
series data (`ts_data`), a list of identities (`identities`), and a set
of dates (`dates`) to compute and update weights dynamically.

## Usage

``` r
get_seq_weights(ts_data, identities, dates)
```

## Arguments

- ts_data:

  A named list of [`stats::ts`](https://rdrr.io/r/stats/ts.html)
  objects. This list should contain the time series data for both the
  aggregate values and their components.

- identities:

  A named list of identities, each containing components and their
  respective weights. The list should include elements with "lhs_weight"
  in their names.

- dates:

  A list with start and end dates for dynamic weight calculations. The
  dates list should have a sub-list `dynamic_weights` with `start` and
  `end` entries, each being a numeric representation of a date, it
  should be in the format produced by `dates_to_num`.

## Value

The function updates the `identities` list with the calculated dynamic
weights and returns nothing explicitly.
