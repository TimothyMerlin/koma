# Format ts Time Labels

For quarterly data (frequency = 4), returns "YYYY Qn". For monthly data
(frequency = 12), returns "YYYY-MM". Otherwise, falls back to the
numeric time values.

## Usage

``` r
format_ts_time(x)
```

## Arguments

- x:

  A `ts` object.

## Value

A character vector of formatted time labels.
