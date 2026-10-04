# Concatenate two time series

Concatenate two time series

## Usage

``` r
concat(x, y, ...)

# S3 method for class 'ts'
concat(x, y, ...)

# S3 method for class 'list'
concat(x, y, ...)

# S3 method for class 'mts'
concat(x, y, ...)
```

## Arguments

- x:

  A `koma_ts` object.

- y:

  A `koma_ts` object to be concatenated to x.

- ...:

  arguments passed to methods (unused for the default method).

## Value

A `koma_ts` object containing `x` followed by `y`.

## Examples

``` r
x <- as_ets(ts(c(100, 101, 102), start = c(2020, 1), frequency = 4))
y <- as_ets(ts(c(103, 104), start = c(2020, 4), frequency = 4))
concat(x, y)
#> <koma_ts>
#> series:
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2020  100  101  102  103
#> 2021  104               
```
