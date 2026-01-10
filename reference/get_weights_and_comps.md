# Extract Component Weights from an Equation

Given a mathematical equation, this function identifies its individual
components and their associated weights. For components paired with a
numeric value via a `*`, that numeric value is considered its weight.
Otherwise, the weight is set to NULL.

## Usage

``` r
get_weights_and_comps(raw_eq, weights_list)
```

## Arguments

- raw_eq:

  A character string of a mathematical equation. The equation is
  expected to have a format like "lhs == rhs", where the right-hand side
  (rhs) might contain components combined using '+' or '-'.

- weights_list:

  A pre-constructed list containing character representations of weights
  derived from both `character_gamma_matrix` and
  `character_beta_matrix`.

## Value

A list with the following elements:

- `equation`: The original equation provided as input.

- `components`: A list of components extracted from the rhs of the
  equation.

- `weights`: A list of weights, corresponding to each component. If a
  component doesn't have an explicit weight in the equation, its weight
  is NULL.

## See also

[`get_identities`](https://timothymerlin.github.io/koma/reference/get_identities.md)
for a function that uses this function's outputs.
