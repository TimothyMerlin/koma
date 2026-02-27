# Highest Density Regions for koma_estimate Objects

Computes highest density regions (HDRs) for coefficient draws from a
`koma_estimate` object.

## Usage

``` r
# S3 method for class 'koma_estimate'
hdr(
  x,
  variables = NULL,
  probs = c(0.5, 0.99),
  n_grid = 4096,
  integration = c("monte_carlo", "grid"),
  mc_use_observed = FALSE,
  mc_draws = NULL,
  mc_quantile_type = 7,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  include_sigma = FALSE,
  ...
)
```

## Arguments

- x:

  A `koma_estimate` object.

- variables:

  Optional character vector of endogenous variables to include. Defaults
  to all variables in `object$estimates`.

- probs:

  Numeric vector of target mass levels. Values in \\(0, 1\]\\ or \\\[0,
  100\]\\ are accepted. Default is `c(0.5, 0.99)`.

- n_grid:

  Number of grid points for the KDE. Default is 4096.

- integration:

  Integration approach for the HDR cutoff. Use `"grid"` for grid-based
  integration or `"monte_carlo"` for Monte Carlo integration. Default is
  `"monte_carlo"`.

- mc_use_observed:

  Logical. If TRUE, Monte Carlo integration uses the observed draws.
  This can be a reasonable approximation for large `x`. If FALSE, draws
  are sampled from the KDE mixture; only implemented for
  `kernel = "gaussian"`.

- mc_draws:

  Optional integer number of draws for Monte Carlo sampling when
  `mc_use_observed = FALSE`. Defaults to `max(length(x), 2000)`.

- mc_quantile_type:

  Quantile type passed to
  [`stats::quantile`](https://rdrr.io/r/stats/quantile.html) for the
  Monte Carlo cutoff. Type 1 matches the order-statistic construction in
  Hyndman (1996), while type 7 (the default) interpolates between
  adjacent order statistics for a smoother, lower-variance cutoff that
  is asymptotically equivalent. Default is 7.

- bw:

  Bandwidth for [`density`](https://rdrr.io/r/stats/density.html).
  Default is "nrd0".

- adjust:

  Bandwidth adjustment factor for
  [`density`](https://rdrr.io/r/stats/density.html). Default is 1.

- kernel:

  Kernel for [`density`](https://rdrr.io/r/stats/density.html). Default
  is "gaussian".

- include_sigma:

  Logical. If TRUE, compute HDRs for the error variance parameter
  (omega). Default is FALSE.

- ...:

  Additional arguments forwarded to
  [`density`](https://rdrr.io/r/stats/density.html).

## Value

A list with class `c("koma_estimate_hdr", "koma_hdr")` containing HDR
intervals per coefficient. The object also includes an `outliers` list
with draws outside the largest HDR level.
