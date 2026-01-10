# Set Gibbs Sampler Specifications

This class updates the Gibbs sampler specifications in the global
environment variable `the$gibbs_sampler`.

## Usage

``` r
set_gibbs_spec(
  ndraws = NULL,
  burnin_ratio = NULL,
  nstore = NULL,
  tau = NULL,
  ...
)
```

## Arguments

- ndraws:

  Integer specifying the number of Gibbs sampler draws.

- burnin_ratio:

  Numeric specifying the ratio for the burn-in period.

- nstore:

  Integer specifying the frequency of stored draws.

- tau:

  Numeric tuning parameter for enforcing an acceptance rate.

- ...:

  Unused arguments that get discarded.

## Value

Invisible NULL. The function updates `the$gibbs_sampler` in place.

## Gibbs Sampler Specifications

- `ndraws`: Integer specifying the number of Gibbs sampler draws.
  Default is 2000.

- `burnin_ratio`: Numeric specifying the ratio for the burn-in period.
  Default is 0.5.

- `nstore`: Integer specifying the frequency of stored draws. Default is
  1.

- `tau`: Numeric tuning parameter for enforcing an acceptance rate.
  Default is 1.1.
