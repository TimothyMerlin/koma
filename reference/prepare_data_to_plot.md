# Prepare Data for Plotting

This function prepares time series data by converting the data from wide
to long format.

## Usage

``` r
prepare_data_to_plot(mts_list, start)
```

## Arguments

- mts_list:

  A list with named multivariate time series objects: growth, level and
  / or growth_annual.

- start:

  A numeric value representing the forecast start date.

## Value

A single tibble in long format combining growth rates, annualized growth
rates, and level data.
