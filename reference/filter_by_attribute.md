# Filter a koma_ts object by attribute value

Filter a koma_ts object by attribute value

## Usage

``` r
filter_by_attribute(x, attribute, value, var = NULL, ...)

# S3 method for class 'mts'
filter_by_attribute(x, attribute, value, var = NULL, ...)

# S3 method for class 'list'
filter_by_attribute(x, attribute, value, var = NULL, ...)
```

## Arguments

- x:

  A koma_ts object or a list of koma_ts objects.

- attribute:

  The attribute name to filter by.

- value:

  The attribute value to match.

- var:

  An optional variable to filter by.

- ...:

  arguments passed to methods (unused for the default method).

## Value

A koma_ts object or list with the matching series.

## Examples

``` r
x <- as_ets(
  ts(1:8, start = c(2020, 1), frequency = 4),
  series_type = "level",
  method = "diff_log"
)
filter_by_attribute(
  list(x = x),
  attribute = "series_type",
  value = "level"
)
#> $x
#> <koma_ts>
#> attributes:
#>   series_type:  chr "level"
#>   method:  chr "diff_log"
#> 
#> series:
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2020    1    2    3    4
#> 2021    5    6    7    8
#> 
```
