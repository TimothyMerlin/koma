# Construct a Dynamic SEM (Structural Equation Model) Phi Matrix

This function constructs the list of lagged endogenous coefficient
matrices \\\Phi(1), \ldots, \Phi(L)\\ from the beta matrix. It maps the
coefficients of the lagged endogenous regressors, located by
[`find_phi_positions()`](https://timothymerlin.github.io/koma/reference/find_phi_positions.md),
into the corresponding \\\Phi(\ell)\\ matrix in the dynamic SEM:
\\Y\Gamma = \tilde{X}\tilde{B} + Y\_{t-1}\Phi(1) + \cdots +
Y\_{t-p}\Phi(L) + U.\\

## Usage

``` r
construct_phi(phi_positions, beta_matrix)
```

## Arguments

- phi_positions:

  Positions of the lagged endogenous regressors, as returned by
  [`find_phi_positions()`](https://timothymerlin.github.io/koma/reference/find_phi_positions.md).

- beta_matrix:

  A numeric matrix of beta coefficients corresponding to the exogenous
  variables in the system of equations.

## Value

A named list of \\n \times n\\ matrices, one per lag \\\ell\\, where
each matrix \\\Phi(\ell)\\ contains the coefficients on the
\\\ell\\-lagged endogenous variables. The list names correspond to lag
orders (as character strings).
