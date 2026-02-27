# Update Gamma and Beta Matrices with Identity Weights

This function injects identity weights into the structural coefficient
matrices. For each identity component, it finds the target entry encoded
in the theta name (e.g., "theta6_4") and replaces the corresponding
value in \\\Gamma\\ or \\B\\.

## Usage

``` r
update_estimates_with_weights(identities, gamma_matrix, beta_matrix)
```

## Arguments

- identities:

  A list of identity equations.

- gamma_matrix:

  Initial \\\Gamma\\ matrix (structural coefficients).

- beta_matrix:

  Initial \\B\\ matrix (exogenous coefficients).

## Value

A list containing:

- `gamma_matrix`: \\\Gamma\\ updated with identity weights.

- `beta_matrix`: \\B\\ updated with identity weights.

## See also

[`construct_posterior`](https://timothymerlin.github.io/koma/reference/construct_posterior.md)
for a function that uses this function's outputs.
