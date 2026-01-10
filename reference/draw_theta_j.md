# Draw Theta from multivariate normal distribution for equation j

Draw Theta from multivariate normal distribution for equation j

## Usage

``` r
draw_theta_j(
  y_matrix,
  x_matrix,
  character_gamma_matrix,
  character_beta_matrix,
  jx,
  gamma_parameters_j,
  omega_tilde_jw
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

- gamma_parameters_j:

  A \\(n_j \times 1)\\ matrix with the parameters of the gamma matrix,
  where \\n_j\\ is the number of endogenous variables in equation \\j\\.

## Value

List containing theta_jw and beta_jw
