# Extract identities from equations

This function filters out equations containing 'epsilon', then extracts
weights and identities based on `character_gamma_matrix` and
`character_beta_matrix`.

## Usage

``` r
get_identities(equations, character_gamma_matrix, character_beta_matrix)
```

## Arguments

- equations:

  A character string containing the system of equations, where equations
  are separated by commas.

- character_gamma_matrix:

  A matrix \\\Gamma\\ that holds the coefficients in character form for
  all equations, with potential entries like "theta", to be used in the
  weights extraction process. The dimensions of the matrix are \\(T
  \times n)\\, where \\T\\ is the number of observations and \\n\\ the
  number of equations.

- character_beta_matrix:

  A matrix \\\beta\\ that holds the coefficients in character form for
  all equations, with potential entries like "theta", to be used in the
  weights extraction process. The dimensions of the matrix are \\(k
  \times n)\\, where \\k\\ is the number of exogenous variables and
  \\n\\ the number of equations.

## Value

A list of identities extracted from the input equations. Each entry
contains:

- equation: The raw equation

- components: The components extracted from the equation

- weights: The corresponding weights for each component
