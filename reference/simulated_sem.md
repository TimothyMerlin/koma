# Simulated SEM example data

A compact simulated example bundle for documentation and quick-start
workflows with
[`estimate()`](https://timothymerlin.github.io/koma/reference/estimate.md),
[`forecast()`](https://timothymerlin.github.io/koma/reference/forecast.md),
and diagnostic plots.

## Usage

``` r
simulated_sem
```

## Format

A `list` with three elements:

- ts_data:

  A named list of `ets` time-series objects.

- sys_eq:

  A `koma_seq` object defining the simulated system of equations.

- dates:

  A named list of date ranges for estimation examples.

## Source

Simulated within the package from the SEM data generator in
`data-raw/DATASET.R`.
