# Draw Omega from inverse Wishart distribution for equation j

`draw_omega_j` draws the variance-covariance matrix
\\\tilde{\Omega}\_j\\ for each row of \\\[ u_j, V_j \]\\.

## Usage

``` r
draw_omega_j(
  y_matrix,
  x_matrix,
  character_gamma_matrix,
  character_beta_matrix,
  jx,
  gamma_parameters_j
)
```

## Arguments

- y_matrix:

  A \\(T \times n)\\ matrix \\Y\\, where \\T\\ is the number of
  observations and \\n\\ the number of equations, i.e. endogenous
  variables.

- x_matrix:

  A \\(T \times k)\\ matrix \\X\\ of observations on \\k\\ exogenous
  variables.

- character_gamma_matrix:

  A matrix \\\Gamma\\ that holds the coefficients in character form for
  all equations. The dimensions of the matrix are \\(T \times n)\\,
  where \\T\\ is the number of observations and \\n\\ the number of
  equations.

- character_beta_matrix:

  A matrix \\\beta\\ that holds the coefficients in character form for
  all equations. The dimensions of the matrix are \\(k \times n)\\,
  where \\k\\ is the number of exogenous variables and \\n\\ the number
  of equations.

- jx:

  The index of equation \\j\\.

## Value

List containing \\{\tilde{\Omega}}\_j^{(w)}\\ as `omega_tilde_jw` and
\\\Omega_j^{(w)}\\ as `omega_jw`.
