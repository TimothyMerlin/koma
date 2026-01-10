# Set Restrictions for Observed Variables in Time Series Data

This function generates a list of restrictions for specified variables
in a time series data frame, based on their values between specified
start and end dates.

## Usage

``` r
set_restrictions(ts_data, variables_to_restrict, start, end)
```

## Arguments

- ts_data:

  A list containing the time series data.

- variables_to_restrict:

  A vector of strings specifying the names of the variables in `ts_data`
  that should be restricted.

- start:

  The starting date for the restrictions.

- end:

  The ending date for the restrictions.

## Value

A named list of restrictions, where the names are the names of the
variables in `variables_to_restrict`. Each element is a list containing
`horizon`, a sequence from 1 to the length of `value_at_date`, and
`value`, the truncated values of the variable between `start` and `end`.
