# Print Method for koma_forecast Objects

This function prints the forecasts contained in a `koma_forecast` object
as a multivariate time series (mts). Users can specify which variables
to print and select either the mean forecast, the median forecast, or
specific quantiles of the forecast distribution.

## Usage

``` r
# S3 method for class 'koma_forecast'
print(x, ..., variables = NULL, central_tendency = NULL)
```

## Arguments

- x:

  A `koma_forecast` object.

- ...:

  Additional parameters.

- variables:

  Optional. A character vector of variable names to print. Default is
  NULL, which prints all forecasted variables.

- central_tendency:

  Optional. A string specifying the type of forecast to print. Can be
  "mean", "median", or a quantile name like "q_5", "q_50", "q_95".
  Default is "mean" if available, otherwise "median", or a specified
  quantile.

## Details

This function prints the forecasts contained in a `koma_forecast`
object. Users can choose to print either the mean forecast, the median
forecast, or specific quantiles of the forecast distribution.

If `variables` is specified, only the forecasts for those variables are
printed. If `central_tendency` is not specified, the function defaults
to printing the mean forecast if available, otherwise the median
forecast, or a specified quantile.
