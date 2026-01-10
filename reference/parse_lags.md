# Parse Lagged Variables in Equation Strings

Identifies and standardizes lagged variable references in a system of
equations. Supports both `var.L(lag)` and `lag(var, lag)` notation.

The function processes each equation, detects lag patterns, and rewrites
them in a unified form. It returns both the modified equations and the
list of unique lagged variables.

## Usage

``` r
parse_lags(equations)
```

## Arguments

- equations:

  A character vector of equation strings.

## Value

A list with components:

- variables:

  A sorted character vector of unique lagged variables.

- equations:

  A character vector of equations with standardized lags.
