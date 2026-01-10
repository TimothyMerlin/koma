# Detect the edge of a list of time series

Identify either the earliest end or the latest start date across a set
of `ts` objects, relative to given `start` and `end` bounds.

## Usage

``` r
detect_edge(ts_data, start, end, direction = c("end", "start"))
```

## Arguments

- ts_data:

  Named list of `ts` objects to inspect.

- start:

  Numeric lower bound for trimming each series.

- end:

  Numeric upper bound for trimming each series.

- direction:

  Character; either `"end"` (default) to find the earliest end date, or
  `"start"` to find the latest start date.

## Value

A list with components:

- `date`: The detected edge date (numeric).

- `variable_names`: Names of the series achieving that edge.
