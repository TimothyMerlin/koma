# Add Parameter Draws to a Long Data Frame

Internal helper that checks whether a parameter group is selected and,
if so, delegates draw extraction to a caller-supplied builder function.

## Usage

``` r
add_draws(
  param_name,
  draws,
  coef_names,
  params,
  variable,
  build_draw_df,
  mat_transform = NULL
)
```

## Arguments

- param_name:

  Character scalar naming the parameter group (e.g., "beta").

- draws:

  A list of MCMC draws for the parameter group.

- coef_names:

  Character vector of coefficient names.

- params:

  Character vector of selected parameter groups.

- variable:

  Character scalar naming the endogenous variable.

- build_draw_df:

  Function that converts draws into a long data.frame.

- mat_transform:

  Optional function to transform the draw matrix before conversion.

## Value

A data.frame or `NULL` when the parameter group is not selected or no
draws are available.
