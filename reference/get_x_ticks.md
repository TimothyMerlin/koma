# Generate Tick Text and Values for a Plot

This function generates tick text and values based on a provided data
frame and an optional existing plot figure. The tick text and values are
specifically structured for showcasing annual growth rates on a
timeline.

## Usage

``` r
get_x_ticks(df_long, fig = NULL)
```

## Arguments

- df_long:

  A data frame in a long format, containing at least the columns
  `data_type`, `color_code`, `value`, and `dates`. This data frame
  should have a row for each date and data type, along with the
  corresponding value and color code.

- fig:

  An optional existing plotly figure object to which new ticks will be
  added. If provided, the function will integrate new tick text and
  values with the existing ticks on the figure. Default is NULL,
  indicating no existing figure.

## Value

A list containing two elements: `ticktext` and `tickvals`. The
`ticktext` element is a character vector of HTML formatted strings for
each tick, showcasing the year and annual growth rate. The `tickvals`
element is an integer vector of years corresponding to each tick.
