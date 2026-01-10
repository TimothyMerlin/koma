# Extended Time Series (ets)

``` r
library(koma)
```

## Overview

`ets` objects are [`stats::ts`](https://rdrr.io/r/stats/ts.html) series
with extra metadata. The extra attributes (`series_type`, `value_type`,
and `method`) tell `koma` how to move between levels and rates while
keeping those attributes through common operations.

## Create an ets

``` r
x <- ets(
  data = 1:10,
  start = c(2019, 1),
  frequency = 4,
  series_type = "level",
  value_type = "real",
  method = "diff_log"
)
x
#> level, real, diff_log
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2019    1    2    3    4
#> 2020    5    6    7    8
#> 2021    9   10

ts_obj <- stats::ts(1:10, start = c(2019, 1), frequency = 4)
y <- as_ets(
  ts_obj,
  series_type = "level",
  value_type = "real",
  method = "diff_log"
)
attr(y, "ets_attributes")
#> [1] "series_type" "value_type"  "method"
```

## Windowing and extending

[`stats::window`](https://rdrr.io/r/stats/window.html) preserves `koma`
attributes, and `extend = TRUE` allows leading or trailing `NA` values
for future merges.

``` r
stats::window(x, start = c(2019, 4))
#> level, real, diff_log
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2019                   4
#> 2020    5    6    7    8
#> 2021    9   10

stats::window(x, start = 2018, extend = TRUE)
#> level, real, diff_log
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2018   NA   NA   NA   NA
#> 2019    1    2    3    4
#> 2020    5    6    7    8
#> 2021    9   10

stats::na.omit(stats::window(x, start = 2018, extend = TRUE))
#> level, real, diff_log
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2019    1    2    3    4
#> 2020    5    6    7    8
#> 2021    9   10
```

## Rates, levels, and anchors

[`rate()`](https://timothymerlin.github.io/koma/reference/rate.md)
converts a level series to growth rates. When it does, it stores an
`anker` attribute used by
[`level()`](https://timothymerlin.github.io/koma/reference/level.md) to
rebuild a level series later.

``` r
x_rate <- rate(x)
x_rate
#> rate, real, diff_log, c(1, 2019)
#>          Qtr1     Qtr2     Qtr3     Qtr4
#> 2019          69.31472 40.54651 28.76821
#> 2020 22.31436 18.23216 15.41507 13.35314
#> 2021 11.77830 10.53605
attr(x_rate, "anker")
#> [1]    1 2019

rate_window <- stats::window(x_rate, start = c(2019, 4))
level(rate_window)
#> level, real, diff_log
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2019              3    4
#> 2020    5    6    7    8
#> 2021    9   10
```

[`lag()`](https://rdrr.io/r/stats/lag.html) updates the anchor
automatically for rate series.

``` r
x_rate_lag <- lag(x_rate, k = -1)
x_rate_lag
#> rate, real, diff_log, c(1, 2019.25)
#>          Qtr1     Qtr2     Qtr3     Qtr4
#> 2019                   69.31472 40.54651
#> 2020 28.76821 22.31436 18.23216 15.41507
#> 2021 13.35314 11.77830 10.53605
attr(x_rate_lag, "anker")
#> [1]    1.00 2019.25
```

## Rebasing and aggregation

Rebase a series to a base period with
[`rebase()`](https://timothymerlin.github.io/koma/reference/rebase.md).

``` r
rebase(x, start = c(2020, 1), end = c(2020, 1))
#> level, real, diff_log
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2019   20   40   60   80
#> 2020  100  120  140  160
#> 2021  180  200
rebase(x, start = c(2020, 1), end = c(2020, 4))
#> level, real, diff_log
#>           Qtr1      Qtr2      Qtr3      Qtr4
#> 2019  15.38462  30.76923  46.15385  61.53846
#> 2020  76.92308  92.30769 107.69231 123.07692
#> 2021 138.46154 153.84615
```

If you have `tempdisagg` installed, you can aggregate with `ta()`.

``` r
if (requireNamespace("tempdisagg", quietly = TRUE)) {
  tempdisagg::ta(x, conversion = "sum", to = "annual")
}
#> level, real, diff_log
#> Time Series:
#> Start = 2019 
#> End = 2020 
#> Frequency = 1 
#> [1] 10 26
```

## Arithmetic and subsetting

Common transformations preserve attributes, so you can keep working in
`koma` without losing metadata.

``` r
log(x)
#> level, real, diff_log
#>           Qtr1      Qtr2      Qtr3      Qtr4
#> 2019 0.0000000 0.6931472 1.0986123 1.3862944
#> 2020 1.6094379 1.7917595 1.9459101 2.0794415
#> 2021 2.1972246 2.3025851
diff(x)
#> level, real, diff_log
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2019         1    1    1
#> 2020    1    1    1    1
#> 2021    1    1
x * 10
#> level, real, diff_log
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2019   10   20   30   40
#> 2020   50   60   70   80
#> 2021   90  100
x[1:2]
#> [1] 1 2
x / x
#> level, real, diff_log
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 2019    1    1    1    1
#> 2020    1    1    1    1
#> 2021    1    1
```
