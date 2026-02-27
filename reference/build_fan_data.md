# Build Fan Chart Data from Forecast Quantiles

Constructs a long data frame with lower/upper band values for fan
charts. If requested quantiles are missing, they are computed from
forecast draws.

## Usage

``` r
build_fan_data(x, tsl, forecast_start, variables, fan_quantiles)
```

## Arguments

- x:

  A `koma_forecast` object.

- tsl:

  In-sample time series list used to anchor the forecast.

- forecast_start:

  Forecast start date for windowing.

- variables:

  Character vector of variables to include.

- fan_quantiles:

  Numeric probabilities for the fan chart.

## Value

A data frame with band values for plotting or `NULL` when no bands can
be constructed.
