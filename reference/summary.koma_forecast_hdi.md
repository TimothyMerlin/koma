# Summary for koma_forecast_hdi Objects

Prints HDI intervals for forecast horizons.

## Usage

``` r
# S3 method for class 'koma_forecast_hdi'
summary(object, ..., variables = NULL, horizon = NULL, digits = 3)
```

## Arguments

- object:

  A `koma_forecast_hdi` object.

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
