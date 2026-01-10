# Construct priors for a single equation

`construct_priors_j` generates prior hyperparameters for equation `jx`
based on user-specified priors and the model's coefficient matrices.

## Usage

``` r
construct_priors_j(priors, character_gamma_matrix, character_beta_matrix, jx)
```

## Arguments

- priors:

  The priors for \\\theta\\ in equation \\j\\.

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

A list with elements:

- `theta_mean`: prior means for exogenous coefficients and intercept.

- `theta_vcv`: prior variance-covariance matrix for exogenous terms.

- `omega_df`: degrees of freedom for error covariance prior.

- `omega_scale`: scale matrix for error covariance prior.

- `gamma_mean`: prior means for endogenous coefficients (if set).

- `gamma_vcv`: prior variance-covariance for endogenous terms.
