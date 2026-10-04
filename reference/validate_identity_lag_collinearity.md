# Validate that Lagged Identities Are Not Collinear with Their Components

An identity holds exactly (no error term), so for any lag `k`,
`identity.L(k)` is an exact linear combination of
`component_1.L(k), ..., component_n.L(k)`. If `identity.L(k)` is used as
a predetermined variable in one equation while *every* one of its
components is *also* lagged by `k` somewhere else in the system, the
resulting `x_matrix` is guaranteed to be rank-deficient – independent of
the data. Because this only depends on the equation specification (which
variables are components of which identity, and which lags are used
where), it can be, and is, checked eagerly here rather than deferred to
estimation time.

## Usage

``` r
validate_identity_lag_collinearity(
  identities,
  predetermined_variables,
  call = rlang::caller_env()
)
```

## Arguments

- identities:

  A list of identities, as returned by
  [`get_identities()`](https://timothymerlin.github.io/koma/reference/get_identities.md).

- predetermined_variables:

  A character vector of lagged variable names, as returned by
  [`parse_lags()`](https://timothymerlin.github.io/koma/reference/parse_lags.md).

- call:

  The environment from which the error is called.

## Details

Identities with dynamic (data-derived) weights are skipped: their
weights can vary over time, so the exact dependency cannot be guaranteed
from the specification alone.
[`validate_full_rank()`](https://timothymerlin.github.io/koma/reference/validate_full_rank.md)
still catches those numerically once data is available.
