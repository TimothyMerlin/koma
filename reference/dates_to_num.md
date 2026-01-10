# Convert Date to Numeric Format

This function converts a vector, list, or 2-element numeric vector of
years and quarters to a numeric representation. The format is
`year + (quarter - 1) / frequency`, where the `frequency` is the number
of periods in a year (e.g., 4 for quarters).

## Usage

``` r
dates_to_num(x, frequency, ...)
```

## Arguments

- x:

  A numeric vector of length 2 (year, quarter), or a list of such
  vectors. For a single year, pass just the year as a scalar.

- frequency:

  A numeric value indicating the number of periods in a year (e.g., 4
  for quarters, 12 for months).

- ...:

  Additional arguments passed to other methods.

## Value

A numeric representation of the date(s).
