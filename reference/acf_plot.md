# Autocorrelation Function Plots for koma_estimate Objects

Visualize autocorrelation functions (ACF) for coefficient draws from a
`koma_estimate` object. By default, beta, gamma, and sigma draws are
shown when available.

## Usage

``` r
acf_plot(x, ...)
```

## Arguments

- x:

  A `koma_estimate` object.

- ...:

  Additional arguments controlling the plot. See Details.

## Value

A ggplot object, or a plotly object when `interactive = TRUE` and plotly
is available.

## Details

Additional arguments supported in `...`:

- variables:

  Optional character vector of endogenous variables to plot.

- params:

  Optional character vector of parameter groups to plot (e.g., "beta",
  "gamma", "sigma"). Defaults to all available.

- thin:

  Optional integer thinning interval for the stored draws. Default is 1
  (no thinning).

- max_draws:

  Optional integer cap on the number of draws used per ACF. When set,
  the most recent draws are kept.

- max_lag:

  Optional integer maximum lag for ACF computation. When NULL, defaults
  to `min(30, n_draws - 1)` per series.

- conf_level:

  Optional numeric confidence level in `(0, 1)` used for ACF
  significance bands. Default is `0.95`.

- scales:

  Facet scale option passed to
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  Default is "fixed".

- facet_ncol:

  Optional integer number of columns for facets.

- interactive:

  Logical. If TRUE and plotly is available, return an interactive plot
  via
  [`plotly::ggplotly`](https://rdrr.io/pkg/plotly/man/ggplotly.html).
  Default is FALSE.

ACF values are computed with `stats::acf(..., plot = FALSE)` for each
retained coefficient draw series.

Note: `sigma` plots use `omega_tilde_jw` and show only variances (no
covariances) from each covariance draw.

The red dashed horizontal lines show approximate significance bounds
\\\pm z\_{1-\alpha/2}/\sqrt{n}\\ for zero autocorrelation, where
\\\alpha = 1 - \code{conf_level}\\ and \\n\\ is the number of retained
draws for the corresponding coefficient series.
