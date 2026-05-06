# Align a koma_ts attribute or metadata series to another time series

Align a koma_ts attribute or metadata series to another time series

## Usage

``` r
align_koma_attr(x, attr = NULL, template)
```

## Arguments

- x:

  A `koma_ts` object or metadata series.

- attr:

  Name of the attribute to align. Only used when `x` is `koma_ts`.

- template:

  A time series whose `tsp` should be used.

## Value

The aligned attribute value.
