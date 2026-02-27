# Highest Density Intervals from Draws

Computes highest density intervals (HDIs) for a numeric sample. The HDI
is defined as the shortest interval containing a target probability
mass.

## Usage

``` r
hdi(x, ...)

# Default S3 method
hdi(x, probs = c(0.5, 0.99), ...)
```

## Arguments

- x:

  A numeric vector of draws.

- ...:

  Unused.

- probs:

  Numeric vector of target mass levels. Values in \\(0, 1\]\\ or \\\[0,
  100\]\\ are accepted. Default is `c(0.5, 0.99)`.

## Value

A list with class `"koma_hdi"` containing:

- intervals:

  Named list of matrices with columns `lower` and `upper`, one matrix
  per `probs` level.

- mode:

  Sample median of the draws (used as a center reference).

- cutoff:

  Named numeric vector of `NA` values (not defined for HDI).

- mass:

  Named numeric vector of achieved mass for each level.

- probs:

  Numeric vector of target masses in (0, 1\].
