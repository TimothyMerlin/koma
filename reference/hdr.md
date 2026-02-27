# Highest Density Regions from a Kernel Density Estimate

Computes highest density regions (HDRs) for a numeric sample using a
kernel density estimate. For multimodal densities, the HDR can consist
of multiple disjoint intervals.

## Usage

``` r
hdr(x, ...)

# Default S3 method
hdr(
  x,
  probs = c(0.5, 0.99),
  n_grid = 4096,
  integration = c("monte_carlo", "grid"),
  mc_use_observed = FALSE,
  mc_draws = NULL,
  mc_quantile_type = 7,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  ...
)
```

## Arguments

- x:

  A numeric vector of draws.

- ...:

  Additional arguments forwarded to
  [`density`](https://rdrr.io/r/stats/density.html). Do not pass `n`,
  `bw`, `adjust`, or `kernel` here.

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

## Value

A list with class `"koma_hdr"` containing:

- intervals:

  Named list of matrices with columns `lower` and `upper`, one matrix
  per `probs` level.

- mode:

  Location of the KDE mode.

- density:

  The density object returned by
  [`stats::density`](https://rdrr.io/r/stats/density.html).

- cutoff:

  Named numeric vector of density cutoffs for each level.

- mass:

  Named numeric vector of achieved mass for each level. For
  `integration = "grid"`, this is the area under the KDE above the
  cutoff (grid-based). For `integration = "monte_carlo"`, this is the
  Monte Carlo coverage, i.e. the fraction of sampled points with density
  above the cutoff.

- probs:

  Numeric vector of target masses in (0, 1\].

## Details

The Monte Carlo integration option follows the approach described by
Hyndman (1996) for computing HDRs from an estimated density.

## References

Hyndman, R. J. (1996). Computing and graphing highest density regions.
The American Statistician, 50(2), 120–126.
[doi:10.2307/2684423](https://doi.org/10.2307/2684423)
