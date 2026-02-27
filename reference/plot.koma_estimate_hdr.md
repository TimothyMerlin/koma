# Plot HDRs for koma_estimate Objects

Plot HDR intervals for coefficients from a `koma_estimate_hdr` object.
Outliers beyond the outer HDR are shown if available in the object. The
plot requires HDR levels matching `box_levels` to be present in the
object.

## Usage

``` r
# S3 method for class 'koma_estimate_hdr'
plot(x, y = NULL, ...)
```

## Arguments

- x:

  A `koma_estimate_hdr` object.

- y:

  Ignored. Included for compatibility with the generic function.

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
  "gamma", "sigma").

- box_levels:

  Optional numeric probabilities in (0, 1\] (or \\\[0, 100\]\\) defining
  the inner and outer HDRs. By default, uses the available HDR levels
  closest to 50% and the maximum probability.

- interactive:

  Logical. If TRUE and plotly is available, return an interactive plot
  via
  [`plotly::ggplotly`](https://rdrr.io/pkg/plotly/man/ggplotly.html).
  Default is FALSE.
