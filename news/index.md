# Changelog

## koma 0.2.2

## koma 0.2.2

- Fixed frequency-dependent forecasting, estimation, evaluation,
  weighting, and plotting paths so single-frequency monthly and yearly
  data work correctly.
- Fixed forecast plotting labels for non-quarterly data.
- Fixed
  [`estimate_sem()`](https://timothymerlin.github.io/koma/reference/estimate_sem.md)
  for multisession futures.
- Improved input validation for forecasting and
  [`system_of_equations()`](https://timothymerlin.github.io/koma/reference/system_of_equations.md).
- Improved `koma_ts` metadata handling for attribute preservation across
  transformations.

## koma 0.2.1

- Fix pkgdown site build by adding package URL and completing
  `_pkgdown.yml` reference index.

## koma 0.2.0

- Added fan chart support in forecast plots, including density-based
  visuals built from stochastic draws.
- Added growth-rate whiskers to forecast plots when fan charts are
  enabled.
- Improved conditional forecasting behavior and messaging.
- Tightened forecast/estimation validation and fixed several forecasting
  edge cases.
- Expanded estimation utilities: support `ts` inputs, per-series ts→ets
  overrides, and richer `texreg` extracts/summaries.
- Added/updated documentation and vignettes (error correction, extract
  methods, and forecast output guidance).
- Reduced/adjusted dependencies (e.g., removed `expm`, `tidyr`,
  `stringr`; moved `plotly` to Suggests; added standalone Wishart
  helpers).
- Improved CI and tooling (GitHub Actions checks, codecov, release/tag
  workflows, and renv handling).
- Added [`summary()`](https://rdrr.io/r/base/summary.html) output for
  `koma_forecast` with mean/median and quantile columns.
- Added MCMC trace plots via
  [`trace_plot()`](https://timothymerlin.github.io/koma/reference/trace_plot.md)
  for coefficient diagnostics.
- Added running-mean diagnostics via
  [`running_mean()`](https://timothymerlin.github.io/koma/reference/running_mean.md)
  and
  [`running_mean_plot()`](https://timothymerlin.github.io/koma/reference/running_mean_plot.md).
- Added autocorrelation diagnostics via
  [`acf_plot()`](https://timothymerlin.github.io/koma/reference/acf_plot.md).

## koma 0.1.0

- Initial github release.
