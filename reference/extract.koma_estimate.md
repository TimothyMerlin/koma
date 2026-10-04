# Extract a texreg summary from a koma_estimate

Builds one or more texreg objects from a `koma_estimate`, so results can
be rendered with
[`texreg::screenreg()`](https://rdrr.io/pkg/texreg/man/screenreg.html)
or similar helpers.

## Usage

``` r
extract.koma_estimate(
  model,
  variables = NULL,
  central_tendency = "mean",
  ci_low = 5,
  ci_up = 95,
  digits = 2,
  ...
)
```

## Arguments

- model:

  A `koma_estimate` object.

- variables:

  Optional character vector of endogenous variables to include. Defaults
  to all variables in `model$estimates`.

- central_tendency:

  Central tendency used when summarizing estimates (e.g., "mean",
  "median"). Defaults to "mean".

- ci_low:

  Lower bound (percent) for credible intervals. Defaults to 5.

- ci_up:

  Upper bound (percent) for credible intervals. Defaults to 95.

- digits:

  Number of digits to round numeric values. Defaults to 2.

- ...:

  Unused. Included for
  [`texreg::extract()`](https://rdrr.io/pkg/texreg/man/extract.html)
  compatibility.

## Value

A `texreg` object when one variable is requested, otherwise a named list
of `texreg` objects.

## See also

[`summary.koma_estimate`](https://timothymerlin.github.io/koma/reference/summary.koma_estimate.md)
for summary output with optional texreg formatting.

## Examples

``` r
if (requireNamespace("texreg", quietly = TRUE)) {
  data("simulated_sem")
  set.seed(11)

  fit <- estimate(
    ts_data = simulated_sem$ts_data,
    sys_eq = simulated_sem$sys_eq,
    dates = simulated_sem$dates,
    options = list(gibbs = list(ndraws = 10))
  )
  texreg::extract(fit, variables = "consumption")
}
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
#> 
#> ── ⚠ MCMC Acceptance Probability Warnings ──────────────────────────────────────
#> • consumption: 80.0%
#> 
#> ℹ Some acceptance probabilities are outside the recommended range (20%-60%).
#> Consider revising the equations, tuning each equation's tau, or adjusting your priors.
#> 
#> Model name: KOMANo decimal places were defined for the GOF statistics.
#> 
#>                       coef.   lower CI   upper CI
#> constant          1.3310507  1.0846858  1.6364938
#> consumption.L(1)  0.4655126  0.4251005  0.4949979
#> consumption.L(2)  0.2276586  0.1882462  0.2601377
#> gdp              -0.2991305 -0.3646066 -0.1849941
#> 
#> No GOF block defined.
```
