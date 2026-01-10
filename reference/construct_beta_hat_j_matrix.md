# Construct \\\hat{\beta_j}\\

\\\hat{\beta_j} = (x_b'x_b)^{-1} x_b' Z_j\\ with \\x_b\\ being the
predetermined variables and constant of equation \\j\\ and \\Z_j = y_j -
Y_j \* \gamma_j\\

## Usage

``` r
construct_beta_hat_j_matrix(x_matrix, z_matrix_j, character_beta_matrix, jx)
```

## Arguments

- x_matrix:

  A \\(T \times k)\\ matrix \\X\\ of observations on \\k\\ exogenous
  variables.

- z_matrix_j:

  A \\Z_j = y_j - Y_j \* \gamma_j\\ matrix.

- character_beta_matrix:

  A matrix \\\beta\\ that holds the coefficients in character form for
  all equations. The dimensions of the matrix are \\(k \times n)\\,
  where \\k\\ is the number of exogenous variables and \\n\\ the number
  of equations.

- jx:

  The index of equation \\j\\.

## Value

\\\hat{\beta_j}\\ with dimensions \\k \times 1\\.
