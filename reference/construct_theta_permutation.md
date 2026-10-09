# Order the elements of theta with the zero restrictions last

Finds the order that moves the parameters of \\\theta_j\\ restricted to
zero to the end. The zero restrictions are only on the betas, i.e. on
the first column of \\\Theta_j\\. The free betas come first, then the
parameters of the other columns in their original order, then the
restricted betas.

## Usage

``` r
construct_theta_permutation(character_beta_matrix, jx, number_of_parameters)
```

## Arguments

- character_beta_matrix:

  A matrix \\\beta\\ that holds the coefficients in character form for
  all equations. The dimensions of the matrix are \\(k \times n)\\,
  where \\k\\ is the number of exogenous variables and \\n\\ the number
  of equations.

- jx:

  The index of equation \\j\\.

- number_of_parameters:

  The length of the vectorized \\\Theta_j\\, i.e. \\k (1 + n_j)\\.

## Value

A list with `permutation`, the positions of the elements of theta in the
new order, and `seperate_blocks_at`, the number of free parameters.

## Details

Indexing with the returned order, `theta[permutation]` and
`xi[permutation, permutation]`, gives the same result as multiplying
with the permutation matrix \\P\\, \\P \theta\\ and \\P \Xi P'\\, but is
much faster.
