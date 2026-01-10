# Construct \\\hat{\Theta}\_j\\

Computes the OLS estimate for \\\hat{\Theta}\_j\\, which is a \\(k
\times (1 + n_j))\\ matrix, where \\n_j\\ is the number of endogenous
variables in equation \\j\\ and k is the number of exogenous variables.

## Usage

``` r
construct_theta_hat_j(x_matrix, z_matrix_j)
```

## Arguments

- x_matrix:

  A \\(T \times k)\\ matrix \\X\\ of observations on \\k\\ exogenous
  variables.

- z_matrix_j:

  A \\Z_j = y_j - Y_j \* \gamma_j\\ matrix.

## Value

\\\hat{\Theta}\_j\\ with dimensions \\(k \times (1 + n_j))\\.

## Details

\\\hat{\Theta}\_j = (X'X)^{-1}X' Z_j\\
