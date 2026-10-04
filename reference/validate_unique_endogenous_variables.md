# Validate that Endogenous Variables Are Declared Only Once

An endogenous variable may be defined by exactly one equation: either a
stochastic equation (`~`) or an identity (`==`), never both, and never
twice. This is checked eagerly, right after `endogenous_variables` is
derived from `equations` and before the gamma/beta matrices and
identities are built from them, because a duplicate at that stage does
not fail cleanly – it produces mismatched matrix dimensions and surfaces
later as an unrelated, low-level internal error deep in identity/weight
construction.

## Usage

``` r
validate_unique_endogenous_variables(
  equations,
  endogenous_variables,
  call = rlang::caller_env()
)
```

## Arguments

- equations:

  A character vector of equations, positionally aligned with
  `endogenous_variables` (i.e. `endogenous_variables[i]` is the LHS
  variable of `equations[i]`), as is the case right after
  [`get_endogenous_variables()`](https://timothymerlin.github.io/koma/reference/get_endogenous_variables.md)
  has been applied to `equations`.

- endogenous_variables:

  A character vector of endogenous variable names, as returned by
  [`get_endogenous_variables()`](https://timothymerlin.github.io/koma/reference/get_endogenous_variables.md).

- call:

  The environment from which the error is called.

## Value

Invisibly `TRUE` if no conflicting or duplicate declarations are found.
