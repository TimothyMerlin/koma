# System of Equations Class

Create and manipulate a system of equations.

## Usage

``` r
system_of_equations(equations = vector(), exogenous_variables = vector(), ...)
```

## Arguments

- equations:

  A character string or vector containing the system of equations. If a
  single string, equations should be separated by commas.

- exogenous_variables:

  A character vector of exogenous variables.

- ...:

  Additional arguments for future extensions.

## Value

An object of class `koma_seq` with the following components:

- `equations`: A character vector of the equations.

- `endogenous_variables`: A character vector of endogenous variables.

- `stochastic_equations`: A character vector of stochastic equations.

- `identities`: A character vector of identity equations.

- `character_gamma_matrix`: A gamma matrix in character form.

- `character_beta_matrix`: A beta matrix in character form.

- `predetermined_variables`: A character vector of lagged variables.

- `total_exogenous_variables`: A character vector of combined constant,
  predetermined, and exogenous variables.

- `priors`: A list of priors per equation.

## Details

This function constructs an object of class `koma_seq` representing a
system of equations, extracting and organizing key components like
endogenous variables, gamma matrix, beta matrix, and more. Equations
should be separated by commas if provided as a single string.

## Equations

- Stochastic equations use `~` (e.g. `y ~ x1 + x2`).

- Identity equations use `==` (e.g. `y == 0.5*x1 + 0.5*x2`).

- Lagged variables are denoted by `X.L(x)` for variable `X` and lag
  `L(x)` (e.g. `.L(1)`, `.L(2)`).

- Intercept are included by default, you can also explicitly specify the
  constant by adding `constant` to the equation (e.g.
  `y ~ constant + x1`). To exclude the intercept, add `+0` or `-1` to
  the equation (e.g. `y ~ 0 + x1`).

For more details on the equation syntax, see `vignette("equations")`.

## See also

[`get_endogenous_variables()`](https://timothymerlin.github.io/koma/reference/get_endogenous_variables.md),
[`get_identities()`](https://timothymerlin.github.io/koma/reference/get_identities.md),
[`construct_gamma_matrix()`](https://timothymerlin.github.io/koma/reference/construct_gamma_matrix.md),
[`extract_lagged_vars()`](https://timothymerlin.github.io/koma/reference/extract_lagged_vars.md),
[`construct_beta_matrix()`](https://timothymerlin.github.io/koma/reference/construct_beta_matrix.md),
[`get_max_lag()`](https://timothymerlin.github.io/koma/reference/get_max_lag.md)

## Examples

``` r
equations <-
  "consumption ~ gdp + consumption.L(1) + consumption.L(2),
investment ~ gdp + investment.L(1) + real_interest_rate,
current_account ~ current_account.L(1) + world_gdp,
manufacturing ~ manufacturing.L(1) + world_gdp,
service ~ service.L(1) + population + gdp,
gdp == 0.4*manufacturing + 0.6*service"

exogenous_variables <- c("real_interest_rate", "world_gdp", "population")

system <- system_of_equations(equations, exogenous_variables)
print(system)
#> 
#> ── System of Equations ─────────────────────────────────────────────────────────
#>     consumption ~  constant + gdp + consumption.L(1) + consumption.L(2)
#>      investment ~  constant + gdp + investment.L(1) + real_interest_rate
#> current_account ~  constant + current_account.L(1) + world_gdp
#>   manufacturing ~  constant + manufacturing.L(1) + world_gdp
#>         service ~  constant + service.L(1) + population + gdp
#>             gdp == 0.4 * manufacturing + 0.6 * service 
```
