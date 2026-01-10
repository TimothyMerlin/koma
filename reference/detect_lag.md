# Detect Lag Number from Variable Name

This function takes a variable name as input and extracts the lag number
from it. The lag number is assumed to be a positive integer preceded by
the letter 'L'.

## Usage

``` r
detect_lag(variable_name)
```

## Arguments

- variable_name:

  A character string representing the variable name.

## Value

An integer representing the detected lag number from the variable name.
Returns NA if no lag detected.
