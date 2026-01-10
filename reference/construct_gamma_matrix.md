# Construct Gamma Matrix

Constructs a Gamma matrix based on the given system of equations and
endogenous variables.

This function constructs a Gamma matrix, which is a diagonal matrix
representing the coefficients of endogenous variables in the system of
equations. The Gamma matrix is used in various econometric models. It
also returns the constructed weights and parameters.

## Usage

``` r
construct_gamma_matrix(equations, endogenous_variables)
```

## Arguments

- equations:

  A character string containing the system of equations, where equations
  are separated by commas.

- endogenous_variables:

  A character vector representing the endogenous variables.

## Value

A Gamma matrix.
