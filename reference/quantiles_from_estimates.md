# Compute Quantiles of Coefficients from Estimations

This function takes the results of an estimation process, typically
stored in a data matrix or array, and computes specified quantiles. It
supports 1D, 2D, and 3D data structures and makes necessary adjustments
before calculations. For multi-dimensional data, the function performs
the percentile computation across each column, retaining only the first
column if the data is 3D.

## Usage

``` r
quantiles_from_estimates(data, include_mean = FALSE, probs = NULL)
```

## Arguments

- data:

  A list or array. If multi-dimensional, it should be either 2D or 3D.

- include_mean:

  Logical. If TRUE, the mean of the coefficients will also be computed
  and returned along with the quantiles. Default is FALSE.

- probs:

  A numerical vector specifying the probabilities for which percentiles
  should be computed. Default is obtained from `get_quantiles()`.

## Value

A named list of computed percentiles for each coefficient. The names are
prefixed with "q\_" and suffixed with the percentile value multiplied by
100 (e.g., "q_5", "q_50", "q_95"). If `include_mean` is TRUE, "q_mean"
is also included in the list.
