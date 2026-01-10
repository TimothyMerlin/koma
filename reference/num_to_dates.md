# Convert Numeric to Date Format

This function converts a numeric representation of a date back to a
vector or list of vectors with year and period (e.g., quarter or month).
The function supports input as a single numeric value, or a list of
numeric values. The numeric representation should match the format
produced by `dates_to_num`.

## Usage

``` r
num_to_dates(x, frequency, ...)
```

## Arguments

- x:

  A numeric value or list of numeric values representing the date(s). If
  `x` is a single numeric value, it should be in the format produced by
  `dates_to_num`.

- frequency:

  A numeric value indicating the number of periods in a year (e.g., 1
  for years, 4 for quarters, 12 for months).

- ...:

  Additional arguments passed to other methods.

## Value

A numeric vector of length 2 (year, period) or a list of such vectors.
