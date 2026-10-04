# Find the Positions of Lagged Endogenous Regressors

Identifies the lagged endogenous regressors in each equation. The result
depends only on the system of equations, so it is computed once and
reused by
[`construct_phi()`](https://timothymerlin.github.io/koma/reference/construct_phi.md)
for every draw.

## Usage

``` r
find_phi_positions(sys_eq)
```

## Arguments

- sys_eq:

  A list containing the system of equations. Must include `$equations`
  with the equations of the system, `$endogenous_variables` with the
  names of the endogenous variables, and `$total_exogenous_variables`
  with the names of all exogenous variables.

## Value

A nested list indexed by lag, equation and endogenous variable, holding
the row in the beta matrix and the row in \\\Phi(\ell)\\.
