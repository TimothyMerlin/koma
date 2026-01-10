# Vectorize Gamma Matrix

Converts the character representation of the gamma matrix into a
transformation matrix and a constant vector, such that vector =
transformation_matrix %\*% parameters + constant_vector where vector =
vec(character_gamma_matrix).

## Usage

``` r
gamma_vectorization(character_gamma_matrix, identity_weights)
```

## Arguments

- character_gamma_matrix:

  A character matrix representing the gamma structure of the model.

- identity_weights:

  A list of identity weights for adjusting constant vectors.

## Value

A list with three elements:

- transformation_matrix:

  A numeric matrix used for parameter transformation.

- parameters:

  A character vector of parameter names.

- constant_vector:

  A numeric vector for constant terms.
