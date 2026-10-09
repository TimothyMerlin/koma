# Forecast the Simultaneous Equations Model (SEM)

This function produces forecasts for the SEM.

## Usage

``` r
forecast(
  estimates,
  dates,
  ...,
  restrictions = NULL,
  options = list(approximate = FALSE, probs = NULL, fill = list(method = "mean"),
    conditional_innov_method = "projection")
)

# S3 method for class 'koma_estimate'
forecast(
  estimates,
  dates,
  ...,
  restrictions = NULL,
  options = list(approximate = FALSE, probs = NULL, fill = list(method = "mean"),
    conditional_innov_method = "projection")
)
```

## Arguments

- estimates:

  A `koma_estimate` object
  ([`estimate`](https://timothymerlin.github.io/koma/reference/estimate.md))
  containing the estimates for the simultaneous equations model, as well
  as a list of time series and a `koma_seq` object
  ([`system_of_equations`](https://timothymerlin.github.io/koma/reference/system_of_equations.md))
  that were used in the estimation.

- dates:

  Key-value list for date ranges in various model operations.

- ...:

  Additional parameters.

- restrictions:

  List of model constraints. Default is empty.

- options:

  Optional settings for forecasting. Use
  `list(approximate = FALSE, probs = NULL, fill = list(method = "mean"), conditional_innov_method = "projection")`.
  Elements:

  - `approximate`: Logical. If FALSE (default), compute point forecasts
    from predictive draws. If TRUE, compute point forecasts from the
    mean/median of coefficient draws (fast approximation).

  - `probs`: Numeric vector of quantile probabilities. If NULL, no
    quantiles are returned. When `approximate = FALSE` and `probs` is
    NULL, defaults to `setdiff(get_quantiles(), 0.5)`.

  - `fill$method`: "mean" or "median" used for conditional fill before
    forecasting.

  - `conditional_innov_method`: Method for drawing conditional
    innovations. One of `"projection"` (default) or `"eigen"`.

## Value

An object of class `koma_forecast`.

An object of class `koma_forecast` is a list containing the following
elements:

- mean:

  Mean point forecasts as a list of time series of class `koma_ts`.

- median:

  Median point forecasts as a list of time series of class `koma_ts`.

- quantiles:

  A list of quantiles, where each element is named according to the
  quantile (e.g., "q_5", "q_50", "q_95"), and contains the forecasts for
  that quantile. This element is NULL if `quantiles = FALSE`.

- ts_data:

  Time-series data set used in forecasting.

- y_matrix:

  The Y matrix constructed from the balanced data up to the current
  quarter, used for forecasting.

- x_matrix:

  The X matrix used for forecasting.

## Details

The `forecast` function for SEM uses the estimates from the
`koma_estimate` object to produce point forecasts and, optionally,
quantile forecasts. When `options$approximate` is FALSE (default), point
forecasts are computed from the predictive draws (with quantiles
controlled by `options$probs`). When TRUE, point forecasts are computed
from the mean and median of the coefficient draws for faster,
approximate results.

The returned `koma_forecast` object keeps forecasts as named lists of
`koma_ts` (for `mean`, `median`, and `quantiles`) alongside the input
data and matrices used to produce them.

Use the
[`print`](https://timothymerlin.github.io/koma/reference/print.koma_forecast.md)
method to print a the forecast results, use the
[`plot`](https://timothymerlin.github.io/koma/reference/plot.koma_forecast.md)
method, to visualize the forecasts and prediction intervals.

## Parallel

This function provides the option for parallel computing through the
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
function. For a detailed example on executing `estimate` in parallel,
see the vignette: `vignette("parallel")`. For more details, see the
[future package
documentation](https://CRAN.R-project.org/package=future).

## See also

- For a comprehensive example of using `forecast`, see
  `vignette("koma")`.

- Related functions within the package that may be of interest:
  [`estimate`](https://timothymerlin.github.io/koma/reference/estimate.md).

## Examples

``` r
data("simulated_sem")

dates <- list(
  current = c(2024, 4),
  estimation = simulated_sem$dates$estimation,
  forecast = list(start = c(2025, 1), end = c(2025, 4))
)

ts_data <- simulated_sem$ts_data
# Endogenous series must stop at the last observed quarter before forecasting.
ts_data[simulated_sem$sys_eq$endogenous_variables] <- lapply(
  simulated_sem$sys_eq$endogenous_variables,
  function(x) {
    stats::window(ts_data[[x]], end = dates$current)
  }
)

set.seed(11)
fit <- estimate(
  ts_data = ts_data,
  sys_eq = simulated_sem$sys_eq,
  dates = dates,
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
fc <- forecast(fit, dates = dates)
#> 
#> ── Forecast ────────────────────────────────────────────────────────────────────
print(fc)
#> <koma_ts>
#> attributes:
#>   series_type: list[9]
#>   method: list[9]
#>   value_type: list[9]
#>   anker: list[9]
#> 
#> series:
#>         consumption investment current_account manufacturing service     gdp
#> 2025 Q1      6.4738     4.5367          0.8352        0.5733 -0.3877 -0.0033
#> 2025 Q2      5.3499     4.2385          1.1044        0.4406 -0.1163  0.1064
#> 2025 Q3      5.4708     4.3407         -0.4310       -0.5032 -0.1463 -0.2890
#> 2025 Q4      5.1219     3.5070          2.4524        1.1098 -0.4564  0.1701
#>         real_interest_rate world_gdp population
#> 2025 Q1            -1.0418   -0.9163     0.9900
#> 2025 Q2            -0.3902    0.0282    -0.3553
#> 2025 Q3            -0.3920   -2.4366    -1.1260
#> 2025 Q4             0.2817    1.8674    -2.7807
```
