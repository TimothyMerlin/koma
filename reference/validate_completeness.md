# Validate Completeness

The function checks if all exogenous variables are declared.

## Usage

``` r
validate_completeness(equations, exogenous_variables)
```

## Arguments

- equations:

  A character string or vector containing the system of equations. If a
  single string, equations should be separated by commas.

- exogenous_variables:

  A character vector of exogenous variables.

## Value

Logical. Returns `TRUE` if the equation is valid. Throws an error with a
specific message if any checks fail.
