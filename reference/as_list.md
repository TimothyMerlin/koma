# Convert an mets koma_ts object to a list

Convert an mets koma_ts object to a list

## Usage

``` r
as_list(x, ...)

# S3 method for class 'mts'
as_list(x, ...)

# S3 method for class 'list'
as_list(x, ...)
```

## Arguments

- x:

  An object to be converted.

- ...:

  Additional arguments.

## Value

A list with ets koma_ts objects.

## Examples

``` r
x <- as_mets(list(
  y = as_ets(ts(1:8, start = c(2020, 1), frequency = 4)),
  z = as_ets(ts(11:18, start = c(2020, 1), frequency = 4))
))
as_list(x)
#> $y
#> <koma_ts>
#> series:
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2020    1    2    3    4
#> 2021    5    6    7    8
#> 
#> $z
#> <koma_ts>
#> series:
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2020   11   12   13   14
#> 2021   15   16   17   18
#> 
```
