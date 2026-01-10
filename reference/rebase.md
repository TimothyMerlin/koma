# Rebase Time Series Data Relative to a Base Period

Calculates a rebased time series using a given date range as the base
period. The base period average is set to 100, and all other values are
scaled accordingly.

## Usage

``` r
rebase(x, start, end, ...)
```

## Arguments

- x:

  An ets object.

- start:

  the time of the first observation. Either a single number or a vector
  of two numbers (the second of which is an integer), which specify a
  natural time unit and a (1-based) number of samples into the time
  unit. See the examples for the use of the second form.

- end:

  the time of the last observation, specified in the same way as
  `start`.

- ...:

  arguments passed to methods (unused for the default method).

## Value

An ets object with the level computed.
