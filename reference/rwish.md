# Random Wishart Draw

Generates a random draw from the Wishart distribution. Adapted from
`MCMCpack::rwish` (Martin et al., 2011) under GPL-3.

## Usage

``` r
rwish(v, S)
```

## Arguments

- v:

  Degrees of freedom (scalar). Must be \>= dimension of S.

- S:

  Positive-definite scale matrix (p x p).

## Value

A single random draw (matrix) from the Wishart distribution.

## References

Martin, R., Quinn, K., & Park, J. (2011). MCMCpack: Markov Chain Monte
Carlo in R. *Journal of Statistical Software, 42*(9), 1-21.
