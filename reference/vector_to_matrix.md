# Convert Vector to Matrix

Given a transformation matrix, a vector of parameters, and a constant
vector, this function constructs a matrix with specified row and column
dimensions. vector = transformation_matrix %\*% parameters,
constant_vector where vector = vec(matrix)

## Usage

``` r
vector_to_matrix(
  transformation_matrix,
  parameters,
  constant_vector,
  nrow,
  ncol
)
```

## Arguments

- transformation_matrix:

  A numeric matrix used for parameter transformation.

- parameters:

  A numeric vector of parameters.

- constant_vector:

  A numeric vector for constant terms.

- nrow:

  Number of rows for the resulting matrix.

- ncol:

  Number of columns for the resulting matrix.

## Value

A numeric matrix with specified row and column dimensions.
