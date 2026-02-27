# Running Mean Plots for koma_estimate Objects

Visualize running means (cumulative averages) from
[`running_mean()`](https://timothymerlin.github.io/koma/reference/running_mean.md)
with a light grey band over the grace-window region.

## Usage

``` r
running_mean_plot(x, ...)
```

## Arguments

- x:

  A `koma_estimate` object.

- ...:

  Additional arguments controlling the plot. See Details in
  [`running_mean()`](https://timothymerlin.github.io/koma/reference/running_mean.md).

## Value

A ggplot object, or a plotly object when `interactive = TRUE` and plotly
is available.

## Details

Additional plot arguments in `...`:

- scales:

  Facet scale option passed to
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  Default is "free_y".

- facet_ncol:

  Optional integer number of columns for facets.

- interactive:

  Logical. If TRUE and plotly is available, return an interactive plot
  via
  [`plotly::ggplotly`](https://rdrr.io/pkg/plotly/man/ggplotly.html).
  Default is FALSE.
