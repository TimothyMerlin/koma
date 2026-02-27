# Check Identity Equations in Forecast Output

Recomputes each identity from its component series and weights, then
warns if deviations exceed `tol`. Intended as a safeguard against
identity drift.

## Usage

``` r
validate_identities(ts_out, identities, tol = 1e-08, x_matrix = NULL)
```

## Arguments

- ts_out:

  A forecast output time-series matrix.

- identities:

  A named list of identity definitions produced by
  [`get_identities()`](https://timothymerlin.github.io/koma/reference/get_identities.md)
  and updated by
  [`get_seq_weights()`](https://timothymerlin.github.io/koma/reference/get_seq_weights.md).

- tol:

  Numeric tolerance for deviations between the identity and its
  reconstructed value.

- x_matrix:

  Optional matrix of exogenous variables to include when checking
  identities.

## Value

Invisibly returns `NULL`.
