# Find Extrema Values for Growth Rates and Levels

Computes the minimum and maximum values for growth rates and levels from
a given data frame, adjusting these values by a certain percentage to
provide a margin. This function is useful for determining the range of
y-axis values in plotting functions.

## Usage

``` r
find_extremas(df_long, whisker_data = NULL)
```

## Arguments

- df_long:

  A data frame in long format that includes 'data_type' as a column,
  which should contain the types 'growth' and 'level' for categorizing
  the data.

## Value

A list with two elements 'growth' and 'level', each containing a 'min'
and 'max' sub-element. These represent the adjusted minimum and maximum
values for the growth rates and levels, respectively.
