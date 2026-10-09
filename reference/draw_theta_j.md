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
  omega_tilde_jw,
  xtx,
  inverse_xtx = solve(xtx),
  theta_permutation,
  equation_data
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

- xtx:

  Precomputed \\x_matrix'x_matrix\\. This is invariant across Gibbs
  draws, so it is computed once instead of on every call.

- inverse_xtx:

  Precomputed inverse of \\x_matrix'x_matrix\\. Same rationale as `xtx`.

- theta_permutation:

  Precomputed order of the elements of theta, as returned by
  [`construct_theta_permutation()`](https://timothymerlin.github.io/koma/reference/construct_theta_permutation.md).
  Same rationale as `xtx`.

- equation_data:

  Fixed equation subsets and counts returned by
  [`construct_equation_data()`](https://timothymerlin.github.io/koma/reference/construct_equation_data.md).
  The samplers compute this once per equation.

## Value

List containing theta_jw and beta_jw
