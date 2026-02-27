# Find Optimal Ticks for y Axis

Find Optimal Ticks for y Axis

## Usage

``` r
get_optimal_ticks(df_long, x_range, whisker_data = NULL)
```

## Arguments

- df_long:

  A data frame in long format that includes 'data_type' as a column,
  which should contain the types 'growth' and 'level' for categorizing
  the data.

- x_range:

  A list with x-axis range with start and end values of format c(YEAR,
  QUARTER).

## Value

A list with y1 (growth) values and text, y2 (level) text and extremas.
