# Construct Z_j

\\Z_j = \[ \\ y_j - Y_j \* \gamma_j, Y_j \] \\\\

## Usage

``` r
construct_z_matrix_j(gamma_parameters_j, y_matrix, y_matrix_j, jx)
```

## Arguments

- gamma_parameters_j:

  A \\(n_j \times 1)\\ matrix with the parameters of the \\\gamma\\
  matrix, where \\n_j\\ is the number of endogenous variables in
  equation \\j\\.

- y_matrix:

  A \\(T \times n)\\ matrix \\Y\\, where \\T\\ is the number of
  observations and \\n\\ the number of equations, i.e. endogenous
  variables.

- y_matrix_j:

  A \\(T \times n_j)\\ matrix of endogenous variables appearing in
  equation \\j\\, with \\T\\ being the number of observations and
  \\n_j\\ the number of endogenous variables in equation \\j\\.

- jx:

  The index of equation \\j\\.

## Value

\\Z_j\\ matrix with dimensions \\T \times 2\\
