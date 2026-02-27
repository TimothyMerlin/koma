# Extract a texreg summary from a koma_estimate

Builds one or more texreg objects from a `koma_estimate`, so results can
be rendered with
[`texreg::screenreg()`](https://rdrr.io/pkg/texreg/man/screenreg.html)
or similar helpers.

## Usage

``` r
extract.koma_estimate(
  model,
  variables = NULL,
  central_tendency = "mean",
  ci_low = 5,
  ci_up = 95,
  digits = 2,
  ...
)
```

## Arguments

- model:

  A `koma_estimate` object.

- variables:

  Optional character vector of endogenous variables to include. Defaults
  to all variables in `model$estimates`.

- central_tendency:

  Central tendency used when summarizing estimates (e.g., "mean",
  "median"). Defaults to "mean".

- ci_low:

  Lower bound (percent) for credible intervals. Defaults to 5.

- ci_up:

  Upper bound (percent) for credible intervals. Defaults to 95.

- digits:

  Number of digits to round numeric values. Defaults to 2.

- ...:

  Unused. Included for
  [`texreg::extract()`](https://rdrr.io/pkg/texreg/man/extract.html)
  compatibility.

## Value

A `texreg` object when one variable is requested, otherwise a named list
of `texreg` objects.

## See also

[`summary.koma_estimate`](https://timothymerlin.github.io/koma/reference/summary.koma_estimate.md)
for summary output with optional texreg formatting.
