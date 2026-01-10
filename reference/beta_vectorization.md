# Vectorize Beta Matrix

Converts the character representation of the beta matrix into a
transformation matrix and constant vector, such that vector =
transformation_matrix %\*% parameters + constant_vector where vector =
vec(character_beta_matrix)

## Usage

``` r
beta_vectorization(character_beta_matrix)
```

## Arguments

- character_beta_matrix:

  A character matrix representing the beta structure of the model.

## Value

A list with three elements:

- transformation_matrix:

  A numeric matrix used for parameter transformation.

- parameters:

  A character vector of parameter names.

- constant_vector:

  A numeric vector for constant terms.
