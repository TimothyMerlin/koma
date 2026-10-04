# Build Fan Chart Data from Forecast Draws

Constructs a long data frame with lower/upper band values for fan
charts. Each forecast draw is first converted to a level path, and the
bands are the quantiles of these level paths per horizon. Compounding
growth-rate quantiles instead would describe a path where every period
sits at the same extreme quantile, which overstates the width of the
bands.

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

  Numeric probabilities for the fan chart. Defaults to the quantiles
  stored in `x`.

## Value

A data frame with band values for plotting or `NULL` when no bands can
be constructed.
