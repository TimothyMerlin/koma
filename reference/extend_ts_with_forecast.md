# Extend Time Series List with Forecast Values at New Dates

This function takes a named list of original time series and extends
each series with forecast values from a multi-time series (mts) only at
dates not already in the original time series.

## Usage

``` r
extend_ts_with_forecast(ts_list, forecast_mts)
```

## Arguments

- ts_list:

  A named list of original time series objects.

- forecast_mts:

  A multi-time series object with forecast values. The column names
  should match the names in `ts_list`.

## Value

A named list of time series objects, each extended with forecast values
for new dates only. For time series whose names are not present in
`forecast_mts`, the original time series is returned unmodified.

## Details

The function will extend the time series in `ts_list` by appending
forecast values from `forecast_mts`. The extension will occur only for
dates that do not already exist in the original time series.
