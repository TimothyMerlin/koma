# Construct Posterior Matrices from LIBA Estimates

This function constructs posterior system matrices from LIBA estimates.
It produces the structural coefficient matrices \\\Gamma\\ and \\B\\,
the implied reduced-form coefficient matrix \\\Phi\\, the structural
shock covariance matrix \\\Sigma\\, and the reduced-form innovation
covariance matrix \\\Omega = (\Gamma^{-1})'\Sigma\Gamma^{-1}\\.

## Usage

``` r
construct_posterior(sys_eq, estimate)
```

## Arguments

- sys_eq:

  A `koma_seq` object
  ([system_of_equations](https://timothymerlin.github.io/koma/reference/system_of_equations.md))
  containing details about the system of equations used in the model.

- estimate:

  A draw that contains beta, gamma and omega tilde estimates.

## Value

A list containing posterior system matrices:

- `gamma_matrix`: Posterior \\\Gamma\\ (n x n) structural coefficient
  matrix.

- `beta_matrix`: Posterior \\B\\ (k x n) exogenous coefficient matrix.

- `phi_matrix`: List of lagged endogenous coefficient matrices (n x n).

- `sigma_matrix`: Structural shock covariance matrix \\\Sigma\\ (n x n).

- `omega_matrix`: Reduced-form innovation covariance matrix \\\Omega =
  (\Gamma^{-1})'\Sigma\Gamma^{-1}\\ (n x n).

## Details

Identity equations are incorporated via
[`update_estimates_with_weights()`](https://timothymerlin.github.io/koma/reference/update_estimates_with_weights.md).
Structural shocks for identities are assumed to have zero variance,
implying corresponding zero rows and columns in \\\Sigma\\.
