# Validate Equations

This function validates a character vector of equations, ensuring that
they adhere to a specific format. The equations should be in the format
"left_variable==right_variables", where right variables can be separated
by '+', '-', or '\*'. The function checks for valid equation structure,
valid variable names, no duplicate regressors in a single equation, and
no duplicate dependent variables across all equations.

## Usage

``` r
validate_system_of_equations(x)
```

## Value

Returns object.
