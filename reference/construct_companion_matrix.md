# Construct Companion Matrices for Dynamic SEM

Construct Companion Matrices for Dynamic SEM

## Usage

``` r
construct_companion_matrix(posterior, exogenous_variables)
```

## Arguments

- posterior:

  A list containing gamma_matrix, beta_matrix, sigma_matrix and
  phi_matrix derived from the LIBA estimates.

- exogenous_variables:

  A character vector of names of exogenous variables.

## Value

A list containing matrices in companion form: phi_matrix,
beta_tilde_matrix, gamma_matrix, c_matrix, and values n and p.
