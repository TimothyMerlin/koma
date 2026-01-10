# Extract Lagged Variables from Equations

This function extracts lagged variables from a given system of equations
using a provided pattern that defines how lagged variables are
structured.

It iterates over each equation, finds matching lag expressions, and
standardizes them in the form `var.L(lag)`. It also rewrites each
equation to expand lag expressions and returns both the modified
equations and the set of unique lagged variables found.

## Usage

``` r
extract_lagged_vars(equations, pattern)
```

## Arguments

- equations:

  A character vector of equations as strings.

- pattern:

  A list defining the regex pattern and capture positions for variable
  name and lag specification. Must contain elements `regex`, `var_pos`,
  and `lag_spec_pos`.

## Value

A list with two elements:

- variables:

  Character vector of unique lagged variables extracted.

- equations:

  Character vector of transformed equations.
