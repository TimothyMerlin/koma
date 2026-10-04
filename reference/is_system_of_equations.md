# Check if Object is a System of Equations

This function checks if the given object inherits from the class
`koma_seq`, indicating that it represents a system of equations.

## Usage

``` r
is_system_of_equations(x)
```

## Arguments

- x:

  An object to be checked.

## Value

Logical. Returns `TRUE` if the object inherits from the class
`koma_seq`, and `FALSE` otherwise.

## Examples

``` r
sys <- system_of_equations("y ~ x", exogenous_variables = "x")
is_system_of_equations(sys)
#> [1] TRUE
```
