# Plot koma Forecasts

Plot koma forecasts

## Usage

``` r
# S3 method for class 'koma_forecast'
plot(x, y = NULL, ...)
```

## Arguments

- x:

  A `koma_forecast` object
  ([forecast](https://timothymerlin.github.io/koma/reference/forecast.md)).

- y:

  Ignored. Included for compatibility with the generic function.

- ...:

  Additional parameters:

  variables

  :   A vector of variable names to plot.

  fig

  :   Optional. A Plotly figure object. Default is NULL.

  theme

  :   Optional. A theme for the plot. Default is NULL.

  fan

  :   Optional. Logical. If TRUE, add a fan chart from quantiles.

  fan_quantiles

  :   Optional. Numeric probabilities in (0, 1\] (or percentages in
      `[0, 100]`) to define fan-chart bands. Default uses the available
      quantiles.

  central_tendency

  :   Optional. A string specifying the type of forecast to print. Can
      be "mean", "median", or a quantile name like "q_5", "q_50",
      "q_95". Default is "mean" if available, otherwise "median", or a
      specified quantile.

## Value

A Plotly figure object displaying the data.
