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

  A list containing the system of equations. Must include
  `$endogenous_variables` with the names of the endogenous variables and
  `$character_beta_matrix` with the character beta matrix.

## Value

A nested list indexed by lag, equation and endogenous variable, holding
the row in the beta matrix and the row in \\\Phi(\ell)\\. It has one
entry for every lag up to the largest one, in order, so that
[`construct_phi()`](https://timothymerlin.github.io/koma/reference/construct_phi.md)
returns \\\Phi(1), \ldots, \Phi(L)\\ without gaps.
