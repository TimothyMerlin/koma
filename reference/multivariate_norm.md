# Simulate from a Multivariate Normal Distribution

Produces one or more samples from the specified multivariate normal
distribution.

## Usage

``` r
multivariate_norm(n = 1, mu, sigma, tol = 1e-06, empirical = FALSE)
```

## Arguments

- n:

  The number of samples required.

- mu:

  A vector giving the means of the variables.

- sigma:

  A positive-definite symmetric matrix specifying the covariance matrix
  of the variables.

- tol:

  Tolerance (relative to the largest variance) for numerical lack of
  positive-definiteness in `sigma`.

- empirical:

  Logical. If true, mu and sigma specify the empirical not population
  mean and covariance matrix.

## Value

If `n = 1` a vector of the same length as `mu`, otherwise an `n` by
`length(mu)` matrix with one sample in each row.

## Details

The matrix decomposition is done vai `eigen`; although a Choleski
decomposition might be faster, the eigendecomposition is stabler.

## References

Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S.
Fourth Edition. Springer, New York. ISBN 0-387-95457-0

## See also

[`rnorm()`](https://rdrr.io/r/stats/Normal.html)

## Examples

``` r
#'
if (FALSE) { # \dontrun{
sigma <- matrix(c(10, 3, 3, 2), 2, 2)
sigma
var(multivariate_norm(n = 1000, rep(0, 2), sigma))
var(multivariate_norm(n = 1000, rep(0, 2), sigma, empirical = TRUE))
} # }
```
