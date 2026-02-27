# Highest Density Intervals for koma_estimate Objects

Computes highest density intervals (HDIs) for coefficient draws from a
`koma_estimate` object.

## Usage

``` r
# S3 method for class 'koma_estimate'
hdi(x, variables = NULL, probs = c(0.5, 0.99), include_sigma = FALSE, ...)
```

## Arguments

- x:

  A `koma_estimate` object.

- variables:

  Optional character vector of endogenous variables to include. Defaults
  to all variables in `object$estimates`.

- probs:

  Numeric vector of target mass levels. Values in \\(0, 1\]\\ or \\\[0,
  100\]\\ are accepted. Default is `c(0.5, 0.99)`.

- include_sigma:

  Logical. If TRUE, compute HDIs for the error variance parameter
  (omega). Default is FALSE.

- ...:

  Unused.

## Value

A list with class `c("koma_estimate_hdi", "koma_hdi")` containing HDI
intervals per coefficient.
