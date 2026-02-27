# Summary method for koma_estimate objects

This function provides a summary for koma_estimate objects. It can
return either a texreg object or an ASCII table.

## Usage

``` r
# S3 method for class 'koma_estimate'
summary(object, ...)
```

## Arguments

- object:

  A koma_estimate object.

- ...:

  Additional parameters:

  variables

  :   Optional. A character vector of variables to summarize. Default is
      NULL, which means all variables will be summarized.

  central_tendency

  :   Optional. A string specifying the measure of central tendency
      ("mean", "median"). Default is "mean".

  ci_low

  :   Optional. Lower bound of the confidence interval. Default is 5.

  ci_up

  :   Optional. Upper bound of the confidence interval. Default is 95.

  use_texreg

  :   Optional. If TRUE, prints a texreg summary when available.
      Defaults to TRUE when texreg is installed, otherwise FALSE.

  digits

  :   Optional. Number of digits to round numeric values. Default is 2.

  Additional arguments are forwarded to texreg output helpers (for
  example, arguments accepted by
  [`texreg::screenreg()`](https://rdrr.io/pkg/texreg/man/screenreg.html))
  when `use_texreg = TRUE`.

## Value

Returns a list of summary statistics for each variable (invisibly) when
`use_texreg` is FALSE. When `use_texreg` is TRUE and texreg is
installed, returns a texreg extract object that prints via `screenreg()`
when printed.
