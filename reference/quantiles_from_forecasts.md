# Calculate Quantiles from Multiple Forecast Matrices

This function calculates specified quantiles for each variable-time
combination across all draws.

## Usage

``` r
quantiles_from_forecasts(forecasts, freq, probs = NULL, include_mean = FALSE)
```

## Arguments

- forecasts:

  A list of matrices. Each matrix should represent a forecasts with the
  same dimensions (rows and columns) for comparison.

- freq:

  The frequency of the data.

- probs:

  A numeric vector specifying which quantiles to compute. Default is
  c(0.25, 0.5, 0.75, 1).

- include_mean:

  Logical. If TRUE, the mean of the forecasts will also be computed and
  returned along with the quantiles. Default is FALSE.

## Value

A named list of matrices, where each matrix corresponds to an original
matrix in `forecasts`. Each output matrix has an additional dimension
corresponding to the computed quantiles, with dimnames indicating the
quantile levels (e.g., "q_25" for the 25th percentile).
