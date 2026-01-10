# Default Gibbs Specfications

Default Gibbs Specfications

## Usage

``` r
get_default_gibbs_spec()
```

## Gibbs Sampler Specifications

- `ndraws`: Integer specifying the number of Gibbs sampler draws.
  Default is 2000.

- `burnin_ratio`: Numeric specifying the ratio for the burn-in period.
  Default is 0.5.

- `nstore`: Integer specifying the frequency of stored draws. Default is
  1.

- `tau`: Numeric tuning parameter for enforcing an acceptance rate.
  Default is 1.1.
