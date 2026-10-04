# Validate that x_matrix has full column rank

Detects exact linear dependence between columns of `x_matrix`, the \\(T
\times k)\\ matrix of observations on exogenous and predetermined
variables used during estimation. Such dependence can arise, for
example, when a lagged identity variable (e.g. `gdp.L(1)`, where
`gdp == 0.5*manufacturing + 0.5*service`) is used alongside lags of its
own identity components elsewhere in the system: since an identity holds
exactly (no error term), its lag is then an exact linear combination of
other columns already in `x_matrix`. Left uncaught, this surfaces later
as an opaque `"computationally singular"` error from
[`solve()`](https://rdrr.io/r/base/solve.html) deep inside the Gibbs
sampler.

## Usage

``` r
validate_full_rank(x_matrix, tol = 1e-07, call = rlang::caller_env())
```

## Arguments

- x_matrix:

  A \\(T \times k)\\ matrix \\X\\ of observations on \\k\\ exogenous
  variables.

- tol:

  Numerical tolerance for detecting rank deficiency, passed to
  [`base::qr()`](https://rdrr.io/r/base/qr.html) and used (relative to
  the largest singular value) to identify the columns responsible.

- call:

  The environment from which the error is called.
