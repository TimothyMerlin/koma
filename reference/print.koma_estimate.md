# Print method for koma_estimate objects

Provides a concise, console-friendly overview of the estimated system.

## Usage

``` r
# S3 method for class 'koma_estimate'
print(
  x,
  ...,
  variables = NULL,
  central_tendency = "mean",
  ci_low = 5,
  ci_up = 95,
  digits = 2
)
```

## Arguments

- x:

  A `koma_estimate` object.

- ...:

  Additional arguments forwarded to formatting internals.

- variables:

  Optional character vector of endogenous variables to print. Defaults
  to all variables.

- central_tendency:

  Central tendency used when summarizing estimates (e.g., "mean",
  "median"). Defaults to "mean".

- ci_low:

  Lower bound (percent) for credible intervals. Defaults to 5.

- ci_up:

  Upper bound (percent) for credible intervals. Defaults to 95.

- digits:

  Number of digits to print for numeric values. Defaults to 2.

## Value

Invisibly returns `x` after printing.

## See also

[`summary.koma_estimate`](https://timothymerlin.github.io/koma/reference/summary.koma_estimate.md)
for detailed posterior summaries.
