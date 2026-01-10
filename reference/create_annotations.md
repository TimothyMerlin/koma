# Create Plotly Annotations at Ticks

Generates a list of annotations for Plotly plots based on x tick
position. This function is designed to create text annotations for
specific points on a plot, such as in-sample and out-of-sample ticks,
with customizable y positions and formatting.

## Usage

``` r
create_annotations(
  x_ticks,
  y_position,
  font = list(family = NULL, size = NULL)
)
```

## Arguments

- x_ticks:

  A data frame containing the tick information. Must include columns
  `year` for the x-axis positions, `value` for the text of the
  annotations, and `color_code` for the color of the text.

- y_position:

  Numeric value specifying the y-axis position of all annotations,
  typically in paper coordinates (0 to 1).

- font:

  A list specifying font attributes for the annotation text, with
  optional `family` and `size` components to customize the font family
  and size, respectively. If not specified, default Plotly text
  properties are used.

## Value

A list of lists, where each inner list represents an annotation in the
format required by Plotly. Each annotation includes properties such as
position, text, and formatting.
