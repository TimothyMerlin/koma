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
  gamma_parameters_j,
  xtx,
  xbtxb,
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

- xtx:

  Precomputed \\x_matrix'x_matrix\\. This is invariant across Gibbs
  draws, so it is computed once instead of on every call.

- xbtxb:

  Precomputed \\x_b'x_b\\, where \\x_b\\ is \\x_matrix\\ restricted to
  the columns kept for equation \\j\\. Same rationale as `xtx`.

- equation_data:

  Fixed equation subsets and counts returned by
  [`construct_equation_data()`](https://timothymerlin.github.io/koma/reference/construct_equation_data.md).
  The samplers compute this once per equation.

## Value

List containing \\{\tilde{\Omega}}\_j^{(w)}\\ as `omega_tilde_jw` and
\\\Omega_j^{(w)}\\ as `omega_jw`.
