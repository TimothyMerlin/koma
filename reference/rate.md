# Compute the rate of change for a time series

Required attributes of the input time series are:

- `series_type` - "rate" or "level"

- `method` - "percentage", "diff_log", "none", or an expression

## Usage

``` r
rate(x, ...)
```

## Arguments

- x:

  A time series object. Supported classes are `ts` and `ets` (a subclass
  of `ts`).

- ...:

  arguments passed to methods (unused for the default method).

## Value

An `ets` object.

## Examples

``` r
x <- ets(1:10, series_type = "level", method = "diff_log")
rate(x)
#> rate, diff_log, c(1, 1)
#> Time Series:
#> Start = 2 
#> End = 10 
#> Frequency = 1 
#> [1] 69.31472 40.54651 28.76821 22.31436 18.23216 15.41507 13.35314 11.77830
#> [9] 10.53605
```
