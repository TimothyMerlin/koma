# Extract Variables from a System of Equations

This function identifies and extracts variable names from a set of
equations. It removes whitespace and splits the equations by common
operators such as `~`, `==`, `+`, `*`, and `-`. It filters out
components that represent numeric values, only returning variable names.

## Usage

``` r
get_variables(equations)
```

## Arguments

- equations:

  A character vector containing the equations to extract variables from.

## Value

A list of character vectors, where each vector contains the variables
from a single equation.
