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
documentation](https://cran.r-project.org/web/packages/future/future.pdf).

## See also

- For a comprehensive example of using `forecast`, see
  `vignette("koma")`.

- Related functions within the package that may be of interest:
  [`estimate`](https://timothymerlin.github.io/koma/reference/estimate.md).
