# Construct a Dynamic SEM (Structural Equation Model) Phi Matrix

This function constructs the Phi matrix from the given system of
equations and beta matrix. It processes the equations to find lagged
endogenous variables and uses them to build the Phi matrix, as used in
dynamic SEM: Y \* Gamma = X_tilde B_tilde + Y(-1) \* Phi(1) + ... +
Y(-p) \* Phi(L) + U.

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

A list of matrices representing the Phi(L) matrix for the given system
of equations.
