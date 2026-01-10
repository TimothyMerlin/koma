# Get Central Tendency Value from a Named List

This function takes a named list `quantiles` and a string
`central_tendency` indicating which central tendency measure (mean or
median) to retrieve.

## Usage

``` r
get_central_tendency(central_tendency, quantiles)
```

## Arguments

- central_tendency:

  A string, either "mean" or "median".

- quantiles:

  A named list containing central tendency values.

## Value

The central tendency value from the list `quantiles` based on the
`central_tendency` argument.
