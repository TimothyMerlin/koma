# Extract Lagged Values from Y Matrix

This function populates the matrix containing the predetermined
variables, i.e. the lagged variables, with the lagged values contained
in the Y matrix.

## Usage

``` r
get_lagged_values(predetermined_matrix, y_matrix, tx)
```

## Arguments

- predetermined_matrix:

  The predetermined matrix to be populated with lagged values.

- y_matrix:

  The y matrix containing the original values.

- tx:

  The time index for which the lagged values should be obtained.

## Value

The updated predetermined matrix with lagged values.
