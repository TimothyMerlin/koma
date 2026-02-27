# Summarize Interval Estimates for koma_estimate\_\* Objects

Summarize Interval Estimates for koma_estimate\_\* Objects

## Usage

``` r
summary_interval_table(
  object,
  variables = NULL,
  params = NULL,
  digits = 3,
  center_label = "Mode",
  interval_label = "HDR"
)
```

## Arguments

- object:

  A koma_estimate\_\* object with interval output.

- variables:

  Optional character vector to filter variables.

- params:

  Optional character vector to filter parameters (e.g., "beta", "gamma",
  "sigma").

- digits:

  Number of digits to round numeric values.

- center_label:

  Label for the center statistic (e.g., "Mode", "Median").

- interval_label:

  Label for the interval type (e.g., "HDR", "HDI").

## Value

Invisibly returns `object`.
