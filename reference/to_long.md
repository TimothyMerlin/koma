# Convert Time Series from Wide to Long Format

This function returns the data in long format.

## Usage

``` r
to_long(mts, start)
```

## Arguments

- mts:

  A multivariate time series object.

- start:

  A numeric value representing the forecast start date.

## Value

A data frame in long format containing the original data along with the
sample status, dates, and frame identifiers.
