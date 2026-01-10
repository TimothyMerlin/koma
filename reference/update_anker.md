# Update anker

This function updates the anker to the last observation of the realized
data.

## Usage

``` r
update_anker(x, y)
```

## Arguments

- x:

  Time series object of class `koma_ts` with realized / historical data.

- y:

  Time series object of class `koma_ts` with forecast data.

## Value

Return y of class `koma_ts` with updated anker.
