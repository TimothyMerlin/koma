# Identify columns involved in an exact linear dependency

Uses the singular value decomposition of `x_matrix` to find near-zero
singular values (rank-deficient directions) and reports the columns with
non-negligible loadings on the corresponding right singular vectors,
i.e. the columns actually involved in the dependency.

## Usage

``` r
find_dependent_columns(x_matrix, tol = 1e-07)
```

## Arguments

- x_matrix:

  A \\(T \times k)\\ matrix \\X\\ of observations on \\k\\ exogenous
  variables.

- tol:

  Numerical tolerance for detecting rank deficiency, passed to
  [`base::qr()`](https://rdrr.io/r/base/qr.html) and used (relative to
  the largest singular value) to identify the columns responsible.

## Value

A character vector of column names involved in at least one exact linear
dependency.
