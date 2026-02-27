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

  Optional. A `koma_estimate` object (see `estimate`) containing the
  estimates of the previously estimated simultaneous equations model.
  Use this parameter when some equations of the system need to be
  re-estimated.

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
documentation](https://cran.r-project.org/web/packages/future/future.pdf).

## Gibbs Sampler Specifications

- `ndraws`: Integer specifying the number of Gibbs sampler draws.
  Default is 2000.

- `burnin_ratio`: Numeric specifying the ratio for the burn-in period.
  Default is 0.5.

- `nstore`: Integer specifying the frequency of stored draws. Default is
  1.

- `tau`: Numeric tuning parameter for enforcing an acceptance rate.
  Default is 1.1.

## See also

- To create a `koma_seq` object see
  [`system_of_equations`](https://timothymerlin.github.io/koma/reference/system_of_equations.md).

- For a comprehensive example of using `estimate`, see
  `vignette("koma")`.

- Related functions within the package that may be of interest:
  [`forecast`](https://timothymerlin.github.io/koma/reference/forecast.md).
