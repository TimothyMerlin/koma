# Construct a Dynamic SEM (Structural Equation Model) Phi Matrix

This function constructs the list of lagged endogenous coefficient
matrices \\\Phi(1), \ldots, \Phi(L)\\ from the system of equations and
the beta matrix. It identifies lagged endogenous regressors in each
equation and maps their coefficients into the corresponding
\\\Phi(\ell)\\ matrix in the dynamic SEM: \\Y\Gamma =
\tilde{X}\tilde{B} + Y\_{t-1}\Phi(1) + \cdots + Y\_{t-p}\Phi(L) + U.\\

## Usage

``` r
construct_phi(sys_eq, beta_matrix)
```

## Arguments

- sys_eq:

  A list containing the system of equations. Must include `$equations`
  with the equations of the system, `$endogenous_variables` with the
  names of the endogenous variables, and `$total_exogenous_variables`
  with the names of all exogenous variables.

- beta_matrix:

  A numeric matrix of beta coefficients corresponding to the exogenous
  variables in the system of equations.

## Value

A named list of \\n \times n\\ matrices, one per lag \\\ell\\, where
each matrix \\\Phi(\ell)\\ contains the coefficients on the
\\\ell\\-lagged endogenous variables. The list names correspond to lag
orders (as character strings).
