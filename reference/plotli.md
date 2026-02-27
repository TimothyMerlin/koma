# Plotting Function

This function processes the data and creates a plot using Plotly and the
theme provided.

## Usage

``` r
plotli(
  df_long,
  fig = NULL,
  theme = NULL,
  fan_data = NULL,
  whisker_data = NULL,
  ...
)
```

## Arguments

- df_long:

  A data frame in long format, containing the data to be plotted.

- fig:

  Optional. A Plotly figure object to update, otherwise creates a new
  one.

- theme:

  List of default plot output parameters. Defaults to NULL, which leads
  to
  [`init_koma_theme`](https://timothymerlin.github.io/koma/reference/init_koma_theme.md)
  being called. Please see the vignette for details about tweaking
  themes.

- ...:

  Directly pass additional arguments to plotly::layout. For
  documentation, see
  <https://plotly.com/r/reference/#Layout_and_layout_style_objects>

## Value

A Plotly figure object displaying the data with the specified
formatting.

## References

<https://plotly.com/r/reference/#Layout_and_layout_style_objects>
