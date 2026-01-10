# Validate Individual Equation

This function validates an individual equation to ensure that it follows
a specific structure. The expected format for the equation is
"left_variable == right_variables", where `right_variables` can be a
combination of variables separated by '+', '-', or '\*'. The function
checks the following:

- Validity of the variable names.

- Correct structure (exactly one '==' separator).

- Stochastic error term `epsilon` must be the last element.

- Duplicate regressors within a single equation are not allowed.

## Usage

``` r
validate_equation(equation)
```

## Arguments

- equation:

  A character string representing an equation in the format
  "left_variable == right_variables".

## Value

Logical. Returns `TRUE` if the equation is valid. Throws an error with a
specific message if any checks fail.
