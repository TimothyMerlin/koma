# Initialize the sampler

`initialize_sampler_informative` initializes the sampler for the
Metropolis-Hastings algorithm. It obtains the initial \\gamma\\
parameters by numerically maximizing the target function. It then
calculates the Cholesky factor \\L\\ of the inverse of the Hessian
\\M^{-1}\\ of the target function. This Cholesky factor, along with the
gamma parameter for equation \\j\\, is returned. The Cholesky factor is
used to draw the candidate gamma in the MH algorithm.

## Usage

``` r
initialize_sampler_informative(
  y_matrix,
  x_matrix,
  character_gamma_matrix,
  character_beta_matrix,
  jx
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

A list containing the initial parameters for gamma (`gamma_jw`) and the
Cholesky factor of the inverse of the Hessian
(`cholesky_of_inverse_hessian`) if `number_endogenous_in_j` is greater
than 0. If not, only the `gamma_jw` parameter is returned as 0.
