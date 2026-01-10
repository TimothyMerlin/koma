# Constructs a matrix of endogenous variables appearing in equation j

This extracts a \\(T x n_j)\\ matrix of endogenous variables appearing
in equation \\j\\, with \\T\\ being the number of observations and
\\n_j\\ the number of endogenous variables in equation \\j\\.

## Usage

``` r
construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)
```

## Arguments

- y_matrix:

  A \\(T \times n)\\ matrix \\Y\\, where \\T\\ is the number of
  observations and \\n\\ the number of equations, i.e. endogenous
  variables.

- character_gamma_matrix:

  A matrix \\\Gamma\\ that holds the coefficients in character form for
  all equations. The dimensions of the matrix are \\(T \times n)\\,
  where \\T\\ is the number of observations and \\n\\ the number of
  equations.

- jx:

  The index of equation \\j\\.

## Value

If equation \\j\\ contains \\\gamma\\ parameters, the function returns a
subset of \\Y\\. If there are no gamma parameters, the function returns
NA.
