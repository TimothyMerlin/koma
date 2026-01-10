# Construct \\\bar{\Theta}\_j\\

Computes the posterior mean for \\\hat{\Theta}\_j\\ with informative
priors, which is a \\(k \times (1 + n_j))\\ matrix, where \\n_j\\ is the
number of endogenous variables in equation \\j\\ and k is the number of
exogenous variables.

## Usage

``` r
construct_theta_bar_j(x_matrix, z_matrix_j, priors_j, omega_tilde_jw)
```

## Arguments

- x_matrix:

  A \\(T \times k)\\ matrix \\X\\ of observations on \\k\\ exogenous
  variables.

- z_matrix_j:

  A \\Z_j = y_j - Y_j \* \gamma_j\\ matrix.

- omega_tilde_jw:

  A variance-covariance matrix \\\tilde{\Omega}\_j = A'\_j \Omega_j
  A_j\\ for row \\j\\.

## Value

\\\hat{\Theta}\_j\\ with dimensions \\(k \times (1 + n_j))\\.

## Details

\\ \bar{\Theta}\_j = \overline{\Xi} \left( kron(\tilde{\Omega},X'X)
\hat{\theta} + \underline{\Xi}^{-1} \underline{\theta} \right) \\
