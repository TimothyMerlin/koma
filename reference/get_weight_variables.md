# Extract Weight Names from Equations

This function takes a vector of equations and extracts the names of the
variables that appear within parentheses.

## Usage

``` r
get_weight_variables(equations)
```

## Arguments

- equations:

  A character string or vector containing the system of equations. If a
  single string, equations should be separated by commas.

## Value

A character vector of unique weight names that appeared within
parentheses in the input equations.
