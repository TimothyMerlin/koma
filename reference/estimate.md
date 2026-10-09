# Estimate the Simultaneous Equations Model (SEM)

Estimate a system of simultaneous equations model (SEM) using a Bayesian
approach. This function incorporates Gibbs sampling and allows for both
density and point forecasts.

## Usage

``` r
estimate(
  ts_data,
  sys_eq,
  dates,
  ...,
  options = list(gibbs = list(), fill = list(method = "mean")),
  estimates = NULL
)

# S3 method for class 'list'
estimate(
  ts_data,
  sys_eq,
  dates,
  ...,
  options = list(gibbs = list(), fill = list(method = "mean")),
  estimates = NULL
)
```

## Arguments

- ts_data:

  Time-series data set for the estimation.

- sys_eq:

  A `koma_seq` object
  ([system_of_equations](https://timothymerlin.github.io/koma/reference/system_of_equations.md))
  containing details about the system of equations used in the model.

- dates:

  Key-value list for date ranges in various model operations.

- ...:

  Additional parameters.

- options:

  Optional settings for estimation. Use
  `list(gibbs = list(), fill = list(method = "mean"))`. Elements:

  - `gibbs`: Gibbs sampler settings (see [Gibbs Sampler
    Specifications](https://timothymerlin.github.io/koma/reference/get_default_gibbs_spec.md)).

  - `fill$method`: "mean" or "median" used to fill ragged edges during
    estimation.

  See [Gibbs Sampler
  Specifications](https://timothymerlin.github.io/koma/reference/get_default_gibbs_spec.md).

- estimates:

  Ignored. Re-estimating only some equations of a previously estimated
  model is currently disabled; passing a `koma_estimate` object gives a
  warning and all equations are estimated.

## Value

An object of class `koma_estimate`.

An object of class `koma_estimate`is a list containing the following
elements:

- estimates:

  The estimated parameters and other relevant information obtained from
  the model.

- sys_eq:

  A `koma_seq` object containing details about the system of equations
  used in the model.

- ts_data:

  The time-series data used for the estimation, with any `NA` values
  removed and lagged variables created.

- y_matrix:

  The Y matrix constructed from the balanced data, used in the
  estimation process.

- x_matrix:

  The X matrix constructed from the balanced data, used in the
  estimation process.

- gibbs_specifications:

  The specifications used for the Gibbs sampling.

- dates:

  The date ranges used during estimation.

- plain_ts_names:

  Character vector of series names that were supplied as plain `ts` (not
  `koma_ts`) in `ts_data`. These are assumed to already be in rates and
  tagged accordingly; see the "Plain ts input" section below.

## Details

After estimation, use
[`summary`](https://timothymerlin.github.io/koma/reference/summary.koma_estimate.md)
for a full table of posterior summaries (with optional credible
intervals and texreg output) and
[`print`](https://timothymerlin.github.io/koma/reference/print.koma_estimate.md)
for a concise console-friendly overview of the estimated system.

## Parallel

This function provides the option for parallel computing through the
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
function. For a detailed example on executing `estimate` in parallel,
see the vignette: `vignette("parallel")`. For more details, see the
[future package
documentation](https://CRAN.R-project.org/package=future).

## Plain ts input

If any element of `ts_data` is a plain `ts` rather than a `koma_ts` (see
[`ets`](https://timothymerlin.github.io/koma/reference/koma_ts.md)), it
is assumed to already be in rates, the form the model estimates on, and
is converted to `koma_ts` with `series_type = "rate"`,
`method = "none"`: the values are used as-is, no rate/level
transformation is applied. A warning lists the affected series, and the
same list is stored in the returned object as `plain_ts_names` (also
surfaced when the `koma_estimate` is printed). If a series actually
needs to be converted from levels (e.g. via a percentage or diff_log
growth rate), convert it first with
[`ets`](https://timothymerlin.github.io/koma/reference/koma_ts.md) or
[`as_ets`](https://timothymerlin.github.io/koma/reference/koma_ts.md);
see
[`vignette("koma-extended-timeseries")`](https://timothymerlin.github.io/koma/articles/koma-extended-timeseries.md).

`koma_ts` objects may carry custom attributes beyond `series_type`/
`method` (e.g. a project-specific `value_type`). Since all series in
`ts_data` must share the same attribute names (see
[`as_mets`](https://timothymerlin.github.io/koma/reference/as_mets.md)),
any such extra attributes found on sibling `koma_ts` series are set to
`NA` on the converted series.

## Gibbs Sampler Specifications

- `ndraws`: Integer specifying the number of Gibbs sampler draws.
  Default is 2000.

- `burnin_ratio`: Numeric specifying the ratio for the burn-in period.
  Default is 0.5.

- `nstore`: Integer specifying the frequency of stored draws. Every
  `nstore`-th draw after the burn-in is kept. Default is 1.

- `tau`: Numeric tuning parameter for enforcing an acceptance rate.
  Default is 1.1.

## See also

- To create a `koma_seq` object see
  [`system_of_equations`](https://timothymerlin.github.io/koma/reference/system_of_equations.md).

- For a comprehensive example of using `estimate`, see
  `vignette("koma")`.

- Related functions within the package that may be of interest:
  [`forecast`](https://timothymerlin.github.io/koma/reference/forecast.md).

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
#> • Number of draws (`ndraws`): 10
#> • Burn-in ratio (`burnin_ratio`): 0.5
#> • Burn-in (`burnin`): 5
#> • Store frequency (`nstore`): 1
#> • Number of saved draws (`nsave`): 5
#> • Tau (`tau`): 1.1
#> 
#> 
#> ── Estimation ──────────────────────────────────────────────────────────────────
print(fit)
#> 
#> ── Estimates ───────────────────────────────────────────────────────────────────
#>     consumption ~  1.23 - 0.33 * gdp  +  0.49 * consumption.L(1)  +  0.22 * consumption.L(2)
#>      investment ~  2.47 - 1.32 * gdp  +  0.41 * investment.L(1)  +  0.35 * real_interest_rate
#> current_account ~  1.53 - 0.51 * current_account.L(1)  +  0.5 * world_gdp
#>   manufacturing ~  0.59  +  0.05 * manufacturing.L(1)  +  0.34 * world_gdp
#>         service ~  0.04  +  0.12 * service.L(1)  +  0.08 * population - 0.74 * gdp
#>             gdp == 0.4 * manufacturing  +  0.6 * service 
```
