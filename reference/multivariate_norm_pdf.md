# Evaluate Multivariate Normal Density at x

Evaluate Multivariate Normal Density at x

## Usage

``` r
multivariate_norm_pdf(x, mu, sigma, log = FALSE)
```

## Arguments

- x:

  A vector at which to evaluate the density.

- mu:

  A vector giving the means of the variables.

- sigma:

  A positive-definite symmetric matrix specifying the covariance matrix
  of the variables.

- log:

  If `TRUE`, the log density is returned. It is computed directly, so it
  stays finite where the density itself underflows to zero.

## Value

The density of the multivariate normal distribution at `x`, or its
logarithm if `log = TRUE`.
