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

  texreg_object

  :   Optional. If TRUE, returns a texreg object. Default is FALSE,
      which returns an ASCII table.

  digits

  :   Optional. Number of digits to round numeric values. Default is 2.

## Value

Depending on the value of texreg_object, returns either a list of texreg
objects or prints an ASCII table.
