# Convert Draw Lists to Matrices

Convert a list of MCMC draw objects into a matrix where rows correspond
to coefficients and columns correspond to draws. If the draws are 3D
arrays, the first slice is used.

## Usage

``` r
draws_to_matrix(draws)
```

## Arguments

- draws:

  A list of draws, where each element is a numeric vector or matrix
  representing coefficient values for a single draw.

## Value

A numeric matrix of draws, or `NULL` if `draws` is empty.
