# Construct reduced form coefficients \\\Pi_0\\

Computes the estimated \\\Pi_0\\, which is a \\(k \times n_j)\\ matrix
of reduced form coefficients, where \\n_j\\ is the number of endogenous
variables in equation \\j\\.

## Usage

``` r
construct_pi_hat_0(x_matrix, z_matrix_j)
```

## Arguments

- x_matrix:

  A \\(T \times k)\\ matrix \\X\\ of observations on \\k\\ exogenous
  variables.

- z_matrix_j:

  A \\Z_j = y_j - Y_j \* \gamma_j\\ matrix.

## Value

\\\hat{\Pi_0}\\ with dimensions \\(k \times n_j)\\.
