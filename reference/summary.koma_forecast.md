# Summary for koma_forecast Objects

Prints a summary table for forecast horizons, including available
central tendencies (mean/median) and quantiles.

## Usage

``` r
# S3 method for class 'koma_forecast'
summary(object, ..., variables = NULL, horizon = NULL, digits = 3)
```

## Arguments

- object:

  A `koma_forecast` object.

- ...:

  Unused.

- variables:

  Optional character vector to filter variables.

- horizon:

  Optional numeric or character vector selecting forecast horizon.

- digits:

  Number of digits to round numeric values. Default is 3.

## Value

Invisibly returns `object`.
