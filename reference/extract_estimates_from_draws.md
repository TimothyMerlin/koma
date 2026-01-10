# Extract Median Estimates

This function computes either central tendency estimates (median or
mean) or returns estimates from a specific draw for gamma, beta, and
sigma matrices. It is mainly used internally by the
`construct_posterior` function.

## Usage

``` r
extract_estimates_from_draws(
  sys_eq,
  estimates,
  jx = NULL,
  central_tendency = "mean"
)
```

## Arguments

- sys_eq:

  A list containing detailed components of the system of equations such
  as `endogenous_variables`, `total_exogenous_variables`,
  `identity_equations`, `character_beta_matrix`, and
  `character_gamma_matrix`.

- estimates:

  A list of estimates, obtained from previous estimation procedures.

- jx:

  If an index is set then the posterior is constructed for estimate of
  draw jx, otherwise the posterior is constructed using the median of
  the estimates. The default is that the posterior is constructed using
  the median estimates.

- central_tendency:

  A character string indicating which central tendency measure ("mean"
  or "median") to use. Default is "mean". Active only when jx is NULL.

## Value

A list containing:

- `gamma_matrix`: Either central tendency estimates or specific draw
  estimates for gamma values, depending on `jx`.

- `beta_matrix`: Either central tendency estimates or specific draw
  estimates for beta values, depending on `jx`.

- `sigma_matrix`: Either central tendency estimates or specific draw
  estimates for sigma values, depending on `jx`.
