# Compute the level for a time series

Required attributes of the input time series are:

- `series_type` - "rate" or "level"

- `method` - "percentage", "diff_log", "none", or an expression

## Usage

``` r
level(x, ...)
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
x <- ets(c(0.3, 0.1, 0.2, -0.1), series_type = "rate", method = "percentage")
level(x)
#> level, percentage
#> Time Series:
#> Start = 0 
#> End = 4 
#> Frequency = 1 
#> [1] 100.0000 100.3000 100.4003 100.6011 100.5005
```
