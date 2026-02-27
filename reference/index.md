# Package index

## Package overview

- [`koma`](https://timothymerlin.github.io/koma/reference/koma.md)
  [`koma-package`](https://timothymerlin.github.io/koma/reference/koma.md)
  : koma: Large Scale Macroeconomic Model

## Core workflow

- [`system_of_equations()`](https://timothymerlin.github.io/koma/reference/system_of_equations.md)
  : System of Equations Class
- [`is_system_of_equations()`](https://timothymerlin.github.io/koma/reference/is_system_of_equations.md)
  : Check if Object is a System of Equations
- [`estimate()`](https://timothymerlin.github.io/koma/reference/estimate.md)
  : Estimate the Simultaneous Equations Model (SEM)
- [`forecast()`](https://timothymerlin.github.io/koma/reference/forecast.md)
  : Forecast the Simultaneous Equations Model (SEM)
- [`model_identification()`](https://timothymerlin.github.io/koma/reference/model_identification.md)
  : Identify Model Parameters from Character Matrices

## Extended timeseries

- [`ets()`](https://timothymerlin.github.io/koma/reference/koma_ts.md)
  [`as_ets()`](https://timothymerlin.github.io/koma/reference/koma_ts.md)
  [`is_ets()`](https://timothymerlin.github.io/koma/reference/koma_ts.md)
  : Extended Time-Series Object
- [`as_list()`](https://timothymerlin.github.io/koma/reference/as_list.md)
  : Convert an mets koma_ts object to a list
- [`as_mets()`](https://timothymerlin.github.io/koma/reference/as_mets.md)
  : Convert a list object with ets koma_ts objects to a koma_ts
  multivariate time series (mets)
- [`concat()`](https://timothymerlin.github.io/koma/reference/concat.md)
  : Concatenate two time series
- [`rate()`](https://timothymerlin.github.io/koma/reference/rate.md) :
  Compute the rate of change for a time series
- [`level()`](https://timothymerlin.github.io/koma/reference/level.md) :
  Compute the level for a time series
- [`rebase()`](https://timothymerlin.github.io/koma/reference/rebase.md)
  : Rebase Time Series Data Relative to a Base Period
- [`type()`](https://timothymerlin.github.io/koma/reference/type.md) :
  Get the type of a koma_ts object

## Datasets

- [`small_open_economy`](https://timothymerlin.github.io/koma/reference/small_open_economy.md)
  : Quarterly time series of key macro variables for a small open
  economy (Switzerland)
- [`klein`](https://timothymerlin.github.io/koma/reference/klein.md) :
  Klein macroeconomic time series (1970 Q1 onward)

## Utilities

- [`summary(`*`<koma_estimate>`*`)`](https://timothymerlin.github.io/koma/reference/summary.koma_estimate.md)
  : Summary method for koma_estimate objects
- [`print(`*`<koma_estimate>`*`)`](https://timothymerlin.github.io/koma/reference/print.koma_estimate.md)
  : Print method for koma_estimate objects
- [`plot(`*`<koma_forecast>`*`)`](https://timothymerlin.github.io/koma/reference/plot.koma_forecast.md)
  : Plot koma Forecasts
- [`print(`*`<koma_forecast>`*`)`](https://timothymerlin.github.io/koma/reference/print.koma_forecast.md)
  : Print Method for koma_forecast Objects
- [`init_koma_theme()`](https://timothymerlin.github.io/koma/reference/init_koma_theme.md)
  : Initiate Default Theme
- [`generate_sample_data()`](https://timothymerlin.github.io/koma/reference/generate_sample_data.md)
  : Generate Data for the Simultaneous Equations Model
- [`format(`*`<koma_seq>`*`)`](https://timothymerlin.github.io/koma/reference/format.koma_seq.md)
  : Format System of Equations
- [`print(`*`<koma_seq>`*`)`](https://timothymerlin.github.io/koma/reference/print.koma_seq.md)
  : Print System of Equations
- [`model_evaluation()`](https://timothymerlin.github.io/koma/reference/model_evaluation.md)
  : Calculate Out-of-Sample RMSE for a Specified Horizon

## HPD and Diagnostics

- [`hdi()`](https://timothymerlin.github.io/koma/reference/hdi.md) :
  Highest Density Intervals from Draws
- [`hdi(`*`<koma_estimate>`*`)`](https://timothymerlin.github.io/koma/reference/hdi.koma_estimate.md)
  : Highest Density Intervals for koma_estimate Objects
- [`hdi(`*`<koma_forecast>`*`)`](https://timothymerlin.github.io/koma/reference/hdi.koma_forecast.md)
  : Highest Density Intervals for koma_forecast Objects
- [`hdr()`](https://timothymerlin.github.io/koma/reference/hdr.md) :
  Highest Density Regions from a Kernel Density Estimate
- [`hdr(`*`<koma_estimate>`*`)`](https://timothymerlin.github.io/koma/reference/hdr.koma_estimate.md)
  : Highest Density Regions for koma_estimate Objects
- [`hdr(`*`<koma_forecast>`*`)`](https://timothymerlin.github.io/koma/reference/hdr.koma_forecast.md)
  : Highest Density Regions for koma_forecast Objects
- [`summary(`*`<koma_estimate_hdi>`*`)`](https://timothymerlin.github.io/koma/reference/summary.koma_estimate_hdi.md)
  : Summary for koma_estimate_hdi Objects
- [`summary(`*`<koma_estimate_hdr>`*`)`](https://timothymerlin.github.io/koma/reference/summary.koma_estimate_hdr.md)
  : Summary for koma_estimate_hdr Objects
- [`summary(`*`<koma_forecast>`*`)`](https://timothymerlin.github.io/koma/reference/summary.koma_forecast.md)
  : Summary for koma_forecast Objects
- [`summary(`*`<koma_forecast_hdi>`*`)`](https://timothymerlin.github.io/koma/reference/summary.koma_forecast_hdi.md)
  : Summary for koma_forecast_hdi Objects
- [`summary(`*`<koma_forecast_hdr>`*`)`](https://timothymerlin.github.io/koma/reference/summary.koma_forecast_hdr.md)
  : Summary for koma_forecast_hdr Objects
- [`summary(`*`<koma_hdi>`*`)`](https://timothymerlin.github.io/koma/reference/summary.koma_hdi.md)
  : Summary for koma_hdi Objects
- [`summary(`*`<koma_hdr>`*`)`](https://timothymerlin.github.io/koma/reference/summary.koma_hdr.md)
  : Summary for koma_hdr Objects
- [`print(`*`<koma_hdi>`*`)`](https://timothymerlin.github.io/koma/reference/print.koma_hdi.md)
  : Print method for koma_hdi objects
- [`print(`*`<koma_hdr>`*`)`](https://timothymerlin.github.io/koma/reference/print.koma_hdr.md)
  : Print method for koma_hdr objects
- [`trace_plot()`](https://timothymerlin.github.io/koma/reference/trace_plot.md)
  : Trace Plots for koma_estimate Objects
- [`acf_plot()`](https://timothymerlin.github.io/koma/reference/acf_plot.md)
  : Autocorrelation Function Plots for koma_estimate Objects
- [`running_mean()`](https://timothymerlin.github.io/koma/reference/running_mean.md)
  : Running Means for koma_estimate Objects
- [`running_mean_plot()`](https://timothymerlin.github.io/koma/reference/running_mean_plot.md)
  : Running Mean Plots for koma_estimate Objects
- [`plot(`*`<koma_estimate_hdr>`*`)`](https://timothymerlin.github.io/koma/reference/plot.koma_estimate_hdr.md)
  : Plot HDRs for koma_estimate Objects
- [`extract.koma_estimate()`](https://timothymerlin.github.io/koma/reference/extract.koma_estimate.md)
  : Extract a texreg summary from a koma_estimate
