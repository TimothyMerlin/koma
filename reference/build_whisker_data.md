# Build Whisker Data for Growth Rates from Forecast Quantiles

Constructs a data frame with lower/upper values for growth-rate
whiskers. If requested quantiles are missing, they are computed from
forecast draws.

## Usage

``` r
build_whisker_data(x, tsl, forecast_start, variables, fan_quantiles)
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

  Numeric probabilities for the whisker bounds.

## Value

A data frame with whisker bounds or `NULL` when no bounds can be
constructed.
