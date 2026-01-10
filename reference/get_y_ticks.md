# Calculate Y-Axis Tick Marks for Multiple Axes in Plotly

This function computes the tick marks for y-axes on a Plotly graph,
ensuring that gridlines align across multiple y-axes by matching dtick
ratios. It adjusts the maximum range of the axes based on these ratios
to align gridlines correctly.

## Usage

``` r
get_y_ticks(extremas, num_ticks, center_around)
```

## Arguments

- extremas:

  A list with 'min' and 'max' values for each y-axis.

- num_ticks:

  The desired number of ticks on each y-axis.

- center_around:

  A numeric value to center the axis ranges.

## Value

A list with dtick, min, and max for each y-axis, adjusted for alignment.

## Details

The function calculates tick mark spacing (`dtick`) and ratios, then
adjusts axis ranges to match the largest dtick ratio, ensuring gridlines
align across axes. It avoids reducing the axis range maximum to prevent
cutting off data points.

## References

Adapted from the methodology outlined by Victor Bezak in the GitHub
repository: https://github.com/VictorBezak/Plotly_Multi-Axes_Gridlines
