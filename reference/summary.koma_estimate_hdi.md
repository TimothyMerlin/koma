# Summary for koma_estimate_hdi Objects

Prints HDI intervals for coefficients in a table per equation.

## Usage

``` r
# S3 method for class 'koma_estimate_hdi'
summary(object, ..., variables = NULL, params = NULL, digits = 3)
```

## Arguments

- object:

  A `koma_estimate_hdi` object.

- ...:

  Unused.

- variables:

  Optional character vector to filter variables.

- params:

  Optional character vector to filter parameters (e.g., "beta", "gamma",
  "sigma").

- digits:

  Number of digits to round numeric values. Default is 3.

## Value

Invisibly returns `object`.
