# Highest Probability Density (HPD): HDR and HDI

``` r
library(koma)
```

## Overview

This vignette introduces highest probability density (HPD) summaries in
koma. Two related summaries are provided:

- [`hdr()`](https://timothymerlin.github.io/koma/reference/hdr.md)
  computes highest density regions (HDRs) using a kernel density
  estimate, following Hyndman (1996). For multimodal draws, HDRs can
  have multiple disjoint intervals. (Hyndman 1996)
- [`hdi()`](https://timothymerlin.github.io/koma/reference/hdi.md)
  computes highest density intervals (HDIs), defined as the shortest
  interval containing a target probability mass. HDIs are always a
  single interval.

Use HDRs when you want multimodal structure to be preserved. Use HDIs
when you want a single concise interval. For full function details, see
[`?hdr`](https://timothymerlin.github.io/koma/reference/hdr.md) and
[`?hdi`](https://timothymerlin.github.io/koma/reference/hdi.md).

## Draws-based HPD summaries

We start from a synthetic bimodal sample to highlight the difference.

``` r
set.seed(123)

draws <- c(
    rnorm(2000, mean = -2, sd = 0.3),
    rnorm(2000, mean = 2, sd = 0.3)
)
plot(stats::density(draws))
```

![](koma-hpd_files/figure-html/unnamed-chunk-2-1.png)

``` r

hdr_res <- hdr(
    draws,
    probs = c(0.5, 0.99),
    integration = "grid"
)

hdi_res <- hdi(
    draws,
    probs = c(0.5, 0.99)
)

hdr_res$intervals$level_99
#>           lower      upper
#> [1,] -3.1657900 -0.8137866
#> [2,]  0.8240061  3.1642691
hdi_res$intervals$level_99
#>          lower    upper
#> [1,] -2.675314 2.646959
```

In this example, the 99% HDR will typically return two intervals, one
around each mode, while the 99% HDI returns a single interval that spans
both modes.

## Tuning HDR estimation

HDRs are based on a kernel density estimate, so you can adjust
bandwidth, kernel choice, or the integration strategy:

``` r
hdr_tuned <- hdr(
    draws,
    probs = 0.9,
    bw = "nrd0",
    adjust = 1.2,
    kernel = "gaussian",
    integration = "monte_carlo",
    mc_use_observed = TRUE,
    mc_quantile_type = 7
)
```

## HDR and HDI from model estimates

If you have a `koma_estimate`, you can compute HPD summaries over
coefficient and variance draws. The helpers return nested lists indexed
by variable, parameter block, coefficient name, and probability level.

``` r
# Define a small system

equations <- "y ~ x + y.L(1)"
exogenous_variables <- "x"

sys_eq <- system_of_equations(
    equations = equations,
    exogenous_variables = exogenous_variables
)

# Simulate data (small sample for speed)

gamma_matrix <- matrix(1, nrow = 1)
beta_matrix <- matrix(c(0.2, 0.5, 0.3), nrow = 3)
sigma_matrix <- matrix(0.01, nrow = 1)

sample <- generate_sample_data(
    sample_size = 80,
    sample_start = c(2000, 1),
    burnin = 20,
    gamma_matrix = gamma_matrix,
    beta_matrix = beta_matrix,
    sigma_matrix = sigma_matrix,
    endogenous_variables = "y",
    exogenous_variables = "x",
    predetermined_variables = "y.L(1)"
)

ts_data <- lapply(sample$ts_data, function(x) {
    as_ets(x, series_type = "rate", method = "diff_log")
})

# Estimate with fewer draws for a quick example

dates <- list(
    estimation = list(start = c(2000, 3), end = c(2019, 4))
)

estimates <- estimate(
    ts_data,
    sys_eq,
    dates,
    options = list(gibbs = list(ndraws = 400, burnin_ratio = 0.5, nstore = 1))
)

# HDRs from coefficient draws

hdr_est <- hdr(estimates, probs = c(0.5, 0.99))
summary(hdr_est)

# HDIs from coefficient draws

hdi_est <- hdi(estimates, probs = c(0.5, 0.99))
summary(hdi_est)

# Example: inspect the 99% beta interval for x in equation y
hdi_est$intervals$y$beta$x$level_99
```

You can also visualize HDR results directly:

``` r
plot(hdr_est)
```

For more complete estimation and forecasting examples, see the getting
started vignette.

Hyndman, Rob J. 1996. “Computing and Graphing Highest Density Regions.”
*The American Statistician* 50 (2): 120–26.
<https://doi.org/10.2307/2684423>.
