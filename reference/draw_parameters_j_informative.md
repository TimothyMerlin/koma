# Draw Parameters for equation j

`draw_parameters_j_informative` simulates from the posterior of the
model's parameters for each equation \\j\\ separately using an
informative prior. The posterior is simulated using a
Metropolis-within-Gibbs sampling procedure.

## Usage

``` r
draw_parameters_j_informative(
  y_matrix,
  x_matrix,
  character_gamma_matrix,
  character_beta_matrix,
  jx,
  gibbs_sampler,
  priors
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

- gibbs_sampler:

  An object of class `gibbs_sampler` that holds an equations gibbs
  settings.

- priors:

  The priors for \\\theta\\ in equation \\j\\.

## Value

A list containing matrices for the saved draws of parameters and
additional diagnostic information.

## Details

The sampler works as follows:

1.  Initialize sampler: \\delta\_{\gamma}^{(0)}\\ and \\\Omega^{(0)}\\

2.  Conditional on \\delta\_{\gamma}^{(w-1)}\\ and \\\Omega^{(w-1)}\\
    and the data draw \\delta\_{\theta}\\.

3.  Conditional on \\delta\_{\gamma}^{(w-1)}\\,
    \\delta\_{\theta}^{(w)}\\ draw \\\Omega^{(w)}\\.

4.  Conditional on \\delta\_{\theta}^{(w)}\\, \\Omega^{(w)}\\ draw
    \\\delta\_{\gamma}^{(w)}\\.

5.  Go back to step 2.
