# Construct Posterior Matrices from LIBA Estimates

This function constructs the posterior matrices using the LIBA
estimates. It primarily focuses on the posterior median.

## Usage

``` r
construct_posterior(sys_eq, estimate)
```

## Arguments

- sys_eq:

  A list containing detailed components of the system of equations such
  as `equations`, `endogenous_variables`, `identities`,
  `character_gamma_matrix`, and `character_beta_matrix`.

- estimate:

  A draw that contains beta, gamma and omega tilde estimates.

## Value

A list containing gamma_matrix, beta_matrix, sigma_matrix and phi_matrix
derived from the LIBA estimates.
