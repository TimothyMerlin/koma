# Parse an Index Specification

Parses a comma-separated specification of integer indices, where each
component is either a single integer or a `lower:upper` range, into the
full integer vector it denotes. Shared by lag notation
(`.L()`/[`lag()`](https://rdrr.io/r/stats/lag.html)) and `dummies()`
indexed-variable expansion, since both use the same "single value or
range, comma-separated" spec syntax.

## Usage

``` r
parse_index_spec(spec)
```

## Arguments

- spec:

  A single string, e.g. `"1"`, `"1:4"`, or `"1:3,5"`.

## Value

An integer vector of unique indices, e.g. `c(1L, 2L, 3L, 5L)`.
