# koma (development)

* Added fan chart support in forecast plots, including density-based visuals built from stochastic draws.
* Added growth-rate whiskers to forecast plots when fan charts are enabled.
* Improved conditional forecasting behavior and messaging.
* Tightened forecast/estimation validation and fixed several forecasting edge cases.
* Expanded estimation utilities: support `ts` inputs, per-series ts→ets overrides, and richer `texreg` extracts/summaries.
* Added/updated documentation and vignettes (error correction, extract methods, and forecast output guidance).
* Reduced/adjusted dependencies (e.g., removed `expm`, `tidyr`, `stringr`; moved `plotly` to Suggests; added standalone Wishart helpers).
* Improved CI and tooling (GitHub Actions checks, codecov, release/tag workflows, and renv handling).
* Added `summary()` output for `koma_forecast` with mean/median and quantile columns.
* Added MCMC trace plots via `trace_plot()` for coefficient diagnostics.

# koma 0.1.0

* Initial github release.
