# Attach Color Codes to Data Frame Based on Status

This function maps the `status_column` values in the provided data frame
to color codes, based on a specified color mapping, and attaches these
color codes as a new column `color_code` in the data frame.

## Usage

``` r
attach_color_code(df_long, marker_color, status_column)
```

## Arguments

- df_long:

  A data frame containing a column specified by `status_column` which
  indicates the status of each sample.

- marker_color:

  A named list or other key-value mapping structure where the names or
  keys correspond to statuses and the values correspond to color codes.

- status_column:

  The name of the column in `df_long` that contains the status
  information.

## Value

A data frame identical to `df_long`, but with an additional column
`color_code` which contains the color codes mapped from the
`status_column` based on the `marker_color` mapping.
