# Rebase Fan Chart Data

Scales the level bands with the same factor that
[`rebase()`](https://timothymerlin.github.io/koma/reference/rebase.md)
applies to the level series, so that the fan stays aligned with the
rebased level line.

## Usage

``` r
rebase_fan_data(fan_data, level_mts, start, end)
```

## Arguments

- fan_data:

  A data frame as returned by
  [`build_fan_data()`](https://timothymerlin.github.io/koma/reference/build_fan_data.md).

- level_mts:

  Level series (before rebasing) with one column per variable in
  `fan_data`.

- start:

  Start date of the index period.

- end:

  End date of the index period.

## Value

`fan_data` with rebased `lower` and `upper` values.
