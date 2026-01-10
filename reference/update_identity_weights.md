# Update Identity Weights

Iteratively updates the weights in an identity list (`identities`) based
on the last value of the corresponding weights found in the `weights`
list.

## Usage

``` r
update_identity_weights(weights, identities)
```

## Arguments

- weights:

  A named list where each entry represents an equation (e.g., "gdp").
  Each equation is itself a named list, containing variable names (e.g.,
  "manufacturing") and their dynamic weight values.

  Example:
  `list(gdp = list(manufacturing = c(0.2, 0.3), service = c(0.7, 0.6)))`

- identities:

  A named list, where each entry represents an equation similar to
  `weights`. The equation has sublists "components" for variable names
  and "weights" for identity weights.

  Example:
  `list(gdp = list(components = list(manufacturing = "theta6_4"), weights = list(theta6_4 = NULL)))`

## Value

A named list containing the updated identity weights for each equation
and its variables, in the same structure as the input `identities`.
