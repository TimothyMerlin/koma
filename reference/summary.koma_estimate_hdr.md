# Summary for koma_estimate_hdr Objects

Prints HDR intervals for coefficients in a table per equation.

## Usage

``` r
# S3 method for class 'koma_estimate_hdr'
summary(object, ..., variables = NULL, params = NULL, digits = 3)
```

## Arguments

- object:

  A `koma_estimate_hdr` object.

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
