# Get Y Position for Annotations in a Figure

Calculates the y-axis position for the next annotation to be added to a
figure based on the number of existing annotations. This function
automates the adjustment of the y-axis position for annotations,
ensuring they are placed without overlap by starting with a base y
position and modifying it according to the current count of annotations
within the figure.

## Usage

``` r
get_y_position_for_annotations(fig)
```

## Arguments

- fig:

  A figure object that potentially contains annotations within its
  layout attributes. The figure object should adhere to the expected
  structure, including a list of layout attributes accessible via
  `$x$layoutAttrs`, where each element may or may not contain an
  `annotations` list.

## Value

Numeric value indicating the y-axis position for placing the next
annotation. This position is calculated based on a predefined base
position, with adjustments made for each existing annotation to prevent
overlap.
