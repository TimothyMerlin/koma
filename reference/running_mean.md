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

## Examples

``` r
data("simulated_sem")
set.seed(11)

fit <- estimate(
  ts_data = simulated_sem$ts_data,
  sys_eq = simulated_sem$sys_eq,
  dates = simulated_sem$dates,
  options = list(gibbs = list(ndraws = 10))
)
#> 
#> ── Gibbs Sampler Settings ──────────────────────────────────────────────────────
#> 
#> ── System Wide Settings ──
#>   • Number of draws (`ndraws`): 10
#>   • Burn-in ratio (`burnin_ratio`): 0.5
#>   • Burn-in (`burnin`): 5
#>   • Store frequency (`nstore`): 1
#>   • Number of saved draws (`nsave`): 5
#>   • Tau (`tau`): 1.1
#> 
#> 
#> ── Estimation ──────────────────────────────────────────────────────────────────
rm_df <- running_mean(fit, params = "beta", max_draws = 100)
head(rm_df)
#>   draw     value    variable param             coef draw_position
#> 1    1 1.0786305 consumption  beta         constant             1
#> 2    1 0.4568034 consumption  beta consumption.L(1)             1
#> 3    1 0.2709837 consumption  beta consumption.L(2)             1
#> 4    2 1.1079817 consumption  beta         constant             2
#> 5    2 0.5100500 consumption  beta consumption.L(1)             2
#> 6    2 0.2207444 consumption  beta consumption.L(2)             2
#>   in_grace_window                             label
#> 1            TRUE         consumption:beta:constant
#> 2            TRUE consumption:beta:consumption.L(1)
#> 3            TRUE consumption:beta:consumption.L(2)
#> 4            TRUE         consumption:beta:constant
#> 5            TRUE consumption:beta:consumption.L(1)
#> 6            TRUE consumption:beta:consumption.L(2)
```
