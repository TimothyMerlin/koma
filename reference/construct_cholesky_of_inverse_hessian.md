# Cholesky factor of the inverse Hessian

Computes the Cholesky factor \\L\\ of the inverse of the Hessian
\\M^{-1}\\ of the target function at its optimum, used to draw the
candidate gamma in the MH algorithm.

## Usage

``` r
construct_cholesky_of_inverse_hessian(hessian)
```

## Arguments

- hessian:

  The Hessian of the target function at its optimum, as returned by
  [`stats::optim()`](https://rdrr.io/r/stats/optim.html).

## Value

The lower triangular Cholesky factor of the inverse Hessian. Stops with
an error if the Hessian is not positive definite, i.e. the target has no
proper optimum to start the sampler from.
