# Convert Dates to String Format

This function converts dates represented as a two-element numeric vector
(year, period), Date, POSIXct, or a list of these, to a character. For
quarterly data (frequency = 4), returns "YYYY Qn". For monthly data
(frequency = 12), returns "YYYY-MM". Otherwise, returns "YYYY-MM-DD".

## Usage

``` r
dates_to_str(x, frequency, ...)
```

## Arguments

- x:

  A numeric vector of length 2 (year, period), a Date, POSIXct, or a
  list of these.

- frequency:

  An integer number of periods per year (e.g., 4 = quarters, 12 =
  months).

- ...:

  Additional arguments passed to methods.

## Value

A character string or a list of character strings.
