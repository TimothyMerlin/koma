# Running Means for koma_estimate Objects

Compute running means (cumulative averages) for coefficient draws from a
`koma_estimate` object. By default, beta, gamma, and sigma draws are
included when available.

## Usage

``` r
running_mean(x, ...)
```

## Arguments

- x:

  A `koma_estimate` object.

- ...:

  Additional arguments controlling the output. See Details.

## Value

A data.frame with columns `draw`, `value`, `variable`, `param`, `coef`,
`draw_position`, `in_grace_window`, and `label`.

## Details

Additional arguments supported in `...`:

- variables:

  Optional character vector of endogenous variables to include.

- params:

  Optional character vector of parameter groups to include (e.g.,
  "beta", "gamma", "sigma"). Defaults to all available.

- thin:

  Optional integer thinning interval for the stored draws. Default is 1
  (no thinning).

- max_draws:

  Optional integer cap on the number of draws returned. When set, the
  most recent draws are kept.

- grace_draws:

  Optional integer number of initial retained draws to flag as a grace
  window for convergence interpretation. If NULL, defaults to
  `max(50, ceiling(0.1 * n_retained))` per series.

Note: `sigma` values are based on `omega_tilde_jw` and use only
variances (no covariances) from each covariance draw.
