# Highest Density Intervals for koma_forecast Objects

Computes highest density intervals (HDIs) for predictive draws in a
`koma_forecast` object.

## Usage

``` r
# S3 method for class 'koma_forecast'
hdi(x, variables = NULL, probs = c(0.5, 0.99), ...)
```

## Arguments

- x:

  A `koma_forecast` object.

- variables:

  Optional character vector of endogenous variables to include. Defaults
  to all variables in `x$forecasts`.

- probs:

  Numeric vector of target mass levels. Values in \\(0, 1\]\\ or \\\[0,
  100\]\\ are accepted. Default is `c(0.5, 0.99)`.

- ...:

  Unused.

## Value

A list with class `c("koma_forecast_hdi", "koma_hdi")` containing HDI
intervals per variable and horizon, along with per-horizon centers and
achieved masses.
