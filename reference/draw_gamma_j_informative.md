# Draw gamma parameters for equation j

`draw_gamma_j_informative` draws the \\\gamma\\ parameters from a given
posterior distribution for an equation \\j\\, in the Metropolis-Hastings
algorithm.

## Usage

``` r
draw_gamma_j_informative(
  y_matrix,
  x_matrix,
  character_gamma_matrix,
  character_beta_matrix,
  jx,
  gamma_jw,
  tau,
  cholesky_of_inverse_hessian,
  omega_jw,
  theta_jw,
  priors_j
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

- gamma_jw:

  A \\(n_j \times 1)\\ vector with the parameters of the gamma matrix,
  where \\n_j\\ is the number of endogenous variables in equation \\j\\.

- tau:

  A tuning scalar \\\tau\\ to adjust the acceptance rate.

- cholesky_of_inverse_hessian:

  The Cholesky factor \\L\\ of the inverse Hessian matrix \\M^{-1}\\
  used to generate candidate draws.

- omega_jw:

  \\{\Omega}\_j^{(w)}\\

- theta_jw:

  A \\(k \times nj+1)\\, where \\n_j\\ is the number of endogenous
  variables in equation \\j\\.

## Value

A \\(n_j \times 1)\\ matrix with the either accepted candidate or
previous gamma parameters. Returns 0 if there are no endogenous
variables in equation \\j\\.
