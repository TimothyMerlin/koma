# Forecast the Simultaneous Equations Model (SEM)

This function produces forecasts for the SEM.

## Usage

``` r
forecast(estimates, dates, ..., restrictions = NULL, point_forecast = NULL)
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

- point_forecast:

  A list that contains the following elements:

  - `active`: Determines the type of forecast generated. If TRUE, a
    point forecast is created. If FALSE, a density forecast is returned.
    Default is TRUE.

  - `central_tendency`: A character string indicating which central
    tendency measure ("mean" or "median") to use for summary statistics.
    Default is "mean".

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
  quantile (e.g., "5", "50", "95"), and contains the forecasts for that
  quantile. This element is NULL if `quantiles = FALSE`.

- ts_data:

  Time-series data set used in forecasting.

- y_matrix:

  The Y matrix constructed from the balanced data up to the current
  quarter, used for forecasting.

- x_matrix:

  The X matrix used for forecasting.

## Details

The `forecast` function for SEM uses the estimates from the
`koma_estimate` object to produce point forecasts or quantile forecasts
based on the `point_forecast` parameter. If `point_forecast$active` is
`TRUE`, only point forecasts are generated. If `FALSE`, quantile
forecasts are generated and included in the `quantiles` list.

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
