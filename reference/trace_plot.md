# Trace Plots for koma_estimate Objects

Visualize MCMC trace plots for coefficient draws from a `koma_estimate`
object. By default, beta, gamma, and sigma draws are shown when
available.

## Usage

``` r
trace_plot(x, ...)
```

## Arguments

- x:

  A `koma_estimate` object.

- ...:

  Additional arguments controlling the plot. See Details.

## Value

A ggplot object, or a plotly object when `interactive = TRUE` and plotly
is available.

## Details

Additional arguments supported in `...`:

- variables:

  Optional character vector of endogenous variables to plot.

- params:

  Optional character vector of parameter groups to plot (e.g., "beta",
  "gamma", "sigma"). Defaults to all available.

- thin:

  Optional integer thinning interval for the stored draws. Default is 1
  (no thinning).

- max_draws:

  Optional integer cap on the number of draws per trace. When set, the
  most recent draws are kept.

- scales:

  Facet scale option passed to
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  Default is "free_y".

- facet_ncol:

  Optional integer number of columns for facets.

- interactive:

  Logical. If TRUE and plotly is available, return an interactive plot
  via
  [`plotly::ggplotly`](https://rdrr.io/pkg/plotly/man/ggplotly.html).
  Default is FALSE.

Note: `sigma` plots use `omega_tilde_jw` and show only variances (no
covariances) from each covariance draw.

## Examples

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  data("simulated_sem")
  set.seed(11)

  fit <- estimate(
    ts_data = simulated_sem$ts_data,
    sys_eq = simulated_sem$sys_eq,
    dates = simulated_sem$dates,
    options = list(gibbs = list(ndraws = 10))
  )
  trace_plot(fit, params = "beta", max_draws = 100)
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

```
