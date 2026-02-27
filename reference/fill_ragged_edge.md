# Fill Ragged Edges in Time Series Data

This function fills in the ragged edges in a time series data set using
a system of equations model. It iteratively detects edges, estimates the
model, and fills the unobserved series using a one-step ahead
conditional forecast until the time series is balanced.

## Usage

``` r
fill_ragged_edge(ts_data, sys_eq, exogenous_variables, dates, fill_method)
```

## Arguments

- ts_data:

  Time-series data set for the estimation.

- sys_eq:

  A `koma_seq` object
  ([system_of_equations](https://timothymerlin.github.io/koma/reference/system_of_equations.md))
  containing details about the system of equations used in the model.

- exogenous_variables:

  A character vector of exogenous variables.

- dates:

  Key-value list for date ranges in various model operations.

- fill_method:

  Character string indicating which central tendency measure ("mean" or
  "median") to use when filling ragged edges.

## Value

A list containing the updated time series data.
