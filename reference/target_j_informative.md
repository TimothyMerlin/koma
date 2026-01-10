# Compute the target function for the jth equation for informative priors

`target_j_informative` calculates the target function used in the
Metropolis-Hastings (MH) algorithm for a given equation \\j\\. The
target function is used in the MH algorithm to accept or reject proposed
new states in the Markov chain.

## Usage

``` r
target_j_informative(
  y_matrix,
  x_matrix,
  character_gamma_matrix,
  character_beta_matrix,
  jx,
  gamma_jw,
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

- omega_jw:

  \\{\Omega}\_j^{(w)}\\

- theta_jw:

  A \\(k \times nj+1)\\, where \\n_j\\ is the number of endogenous
  variables in equation \\j\\.

## Value

The function returns the evaluation of the target function, which is
used to decide whether to accept or reject proposed states in the MH
algorithm. Returns NA if there are no gamma parameters.
