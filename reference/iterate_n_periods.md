# Add Periods to Numeric Date Representation

This function iterates a numeric date representation by a given number
of periods (e.g., years, quarters or months). It accepts a date in
either vector format (year, period) or its numeric representation and
adds the specified number of periods to it.

## Usage

``` r
iterate_n_periods(x, n, frequency)
```

## Arguments

- x:

  A numeric value or a 2-element vector (year, period). The vector
  should represent a year and a period (e.g., quarter or month).

- n:

  The number of periods to add. This can be positive or negative.

- frequency:

  A numeric value indicating the number of periods in a year (e.g., 1,
  for years, 4 for quarters, 12 for months).

## Value

A numeric representation of the date after adding the periods.
