# Format System of Equations

This function formats an object of class `koma_seq` for better
readability. It formats the equations to ensure proper spacing around
operators and aligns the equations for a cleaner display. Priors are
read from `x$priors` and shown in front of the term they belong to, with
the error-term prior last. Equation specific settings are read from
`x$equation_settings` and appended in square brackets.

## Usage

``` r
# S3 method for class 'koma_seq'
format(x, ...)
```

## Arguments

- x:

  An object of class `koma_seq`.

- ...:

  Additional arguments passed to or from other methods.

## Value

A character vector of the formatted equations.
