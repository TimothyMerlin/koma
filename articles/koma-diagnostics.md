# MCMC Diagnostics for Estimated SEM

``` r
library(koma)
```

## Overview

This vignette shows how to inspect MCMC diagnostics for a
`koma_estimate` object:

- [`trace_plot()`](https://timothymerlin.github.io/koma/reference/trace_plot.md)
  for raw draw trajectories.
- [`running_mean_plot()`](https://timothymerlin.github.io/koma/reference/running_mean_plot.md)
  for cumulative averages.
- [`acf_plot()`](https://timothymerlin.github.io/koma/reference/acf_plot.md)
  for autocorrelation across lags.

## Build a Small Model

``` r
equations <- "consumption ~ gdp + consumption.L(1) + interest_rate,
investment ~ gdp + investment.L(1) + interest_rate,
gdp == (consumption/gdp)*consumption + (investment/gdp)*investment"

exogenous_variables <- c("interest_rate")

sys_eq <- system_of_equations(
    equations = equations,
    exogenous_variables = exogenous_variables
)

dates <- list(
    estimation = list(start = c(1996, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 1), end = c(2023, 4))
)
```

## Prepare Data and Estimate

``` r
data("small_open_economy")

series <- unique(c(sys_eq$endogenous_variables, sys_eq$exogenous_variables))
ts_data <- small_open_economy[series]

ts_data <- lapply(ts_data, function(x) {
    as_ets(x, series_type = "level", method = "diff_log")
})
ts_data$interest_rate <- as_ets(
    ts_data$interest_rate,
    series_type = "rate",
    method = "none"
)

set.seed(123)
estimates <- estimate(
  ts_data = ts_data,
  sys_eq = sys_eq,
  dates = dates,
  options = list(gibbs = list(ndraws = 200))
)
```

## Trace Plots

Trace plots help detect non-stationary behavior and abrupt jumps in the
chain.

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
    trace_plot(
        estimates,
        variables = c("consumption", "investment"),
        params = c("beta", "gamma"),
        thin = 2,
        max_draws = 100
    )
}
```

## Running Means

Running means make long-run stabilization of posterior draws easier to
assess.

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
    running_mean_plot(
        estimates,
        variables = c("consumption", "investment"),
        params = c("beta", "gamma"),
        grace_draws = 25
    )
}
```

## Autocorrelation Function (ACF) Plots

ACF plots highlight serial dependence in posterior draws by lag.

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
    acf_plot(
        estimates,
        variables = c("consumption", "investment"),
        params = c("beta", "gamma"),
        max_lag = 20
    )
}
```

## Interactive Diagnostics

If `plotly` is installed, all diagnostic plots can be returned as
interactive objects by setting `interactive = TRUE`.

``` r
if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("plotly", quietly = TRUE)) {
    acf_plot(
        estimates,
        variables = "consumption",
        params = "beta",
        interactive = TRUE
    )
}
```
