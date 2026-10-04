# Convert a list object with ets koma_ts objects to a koma_ts multivariate time series (mets)

Convert a list object with ets koma_ts objects to a koma_ts multivariate
time series (mets)

## Usage

``` r
as_mets(x, ...)

# S3 method for class 'list'
as_mets(x, ...)
```

## Arguments

- x:

  An object to be converted.

- ...:

  Additional arguments.

## Value

A koma_ts multivariate time series object.

## Examples

``` r
x <- list(
  y = as_ets(ts(1:8, start = c(2020, 1), frequency = 4)),
  z = as_ets(ts(11:18, start = c(2020, 1), frequency = 4))
)
as_mets(x)
#> <koma_ts>
#> series:
#>         y  z
#> 2020 Q1 1 11
#> 2020 Q2 2 12
#> 2020 Q3 3 13
#> 2020 Q4 4 14
#> 2021 Q1 5 15
#> 2021 Q2 6 16
#> 2021 Q3 7 17
#> 2021 Q4 8 18
```
