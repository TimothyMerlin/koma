# Compute initial omega for the jth equation

`initial_omega_j` computes OLS quantity for initial omega_j using
initialized gamma_j'

## Usage

``` r
initial_omega_j(
  y_matrix,
  x_matrix,
  character_gamma_matrix,
  character_beta_matrix,
  jx,
  gamma_jw,
  xtx,
  xbtxb
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

- xtx:

  Precomputed \\x_matrix'x_matrix\\. This is invariant across Gibbs
  draws, so it is computed once instead of on every call.

- xbtxb:

  Precomputed \\x_b'x_b\\, where \\x_b\\ is \\x_matrix\\ restricted to
  the columns kept for equation \\j\\. Same rationale as `xtx`.

## Value

The function returns the evaluation of the target function, which is
used to decide whether to accept or reject proposed states in the MH
algorithm. Returns NA if there are no gamma parameters.
