# Construct Reduced Form Parameters from Companion Matrix

This function computes the reduced form parameters companion_pi,
companion_theta, and companion_d using the provided companion_matrix.

## Usage

``` r
construct_reduced_form(companion_matrix)
```

## Arguments

- companion_matrix:

  A list containing the matrices beta_tilde_matrix, phi_matrix,
  gamma_matrix, and c_matrix used to calculate the reduced form
  parameters.

## Value

A list containing the matrices companion_pi, companion_theta, and
companion_d that represent the reduced form parameters
