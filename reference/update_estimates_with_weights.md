# Update Gamma and Beta Matrices with Identity Weights

The function takes in the identities and initial matrices for gamma and
beta, and updates the values in the matrices based on the identity
weights.

## Usage

``` r
update_estimates_with_weights(identities, gamma_matrix, beta_matrix)
```

## Arguments

- identities:

  A list of identity equations.

- gamma_matrix:

  Initial gamma matrix derived from median estimates.

- beta_matrix:

  Initial beta matrix derived from median estimates.

## Value

A list containing:

- `gamma_matrix`: Gamma matrix updated with identity weights.

- `beta_matrix`: Beta matrix updated with identity weights.

## See also

[`construct_posterior`](https://timothymerlin.github.io/koma/reference/construct_posterior.md)
for a function that uses this function's outputs.
