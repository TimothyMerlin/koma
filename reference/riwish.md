# Random Inverse-Wishart Draw

Generates a random draw from the Inverse-Wishart distribution.

## Usage

``` r
riwish(v, S)
```

## Arguments

- v:

  Degrees of freedom (scalar). Must be \>= dimension of S.

- S:

  Positive-definite scale matrix (p x p).

## Value

A single random draw (matrix) from the Inverse-Wishart distribution.

## Details

This function is adapted from `MCMCpack::riwish` (Martin et al., 2011)
under GPL-3. The Inverse-Wishart distribution is commonly used as a
prior for covariance matrices.

## References

Martin, R., Quinn, K., & Park, J. (2011). MCMCpack: Markov Chain Monte
Carlo in R. *Journal of Statistical Software, 42*(9), 1-21.
