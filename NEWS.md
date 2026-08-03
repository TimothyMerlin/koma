# koma 0.3.1.9000

* Exported `set_koma_attr_policy()`, previously internal-only, since the error raised when merging mismatched `koma_ts` attributes (e.g. `anker`) directs users to call it.
* Added `get_koma_attr_policy()` to inspect a registered attribute policy, and `reset_koma_attr_policy()` to remove one (or all) registered policies, since policies are shared for the whole R session.
* Rank-deficient `x_matrix` caused by a lagged identity coinciding with lags of its own components (e.g. `gdp.L(1)` alongside lags of all of `gdp`'s components) is now caught early with an informative error identifying the collinear variables, instead of surfacing later as an opaque `"computationally singular"` error inside the Gibbs sampler (#135).
* **Behavior change:** `estimate()` no longer interactively prompts (via `readline()`) when `ts_data` contains plain `ts` series instead of `koma_ts`. It now silently converts them to `koma_ts` with `series_type = "rate"`, `method = "none"` (i.e. assumes the series is already in rates, the form the model estimates on, and applies no transformation) and emits a warning listing the affected series. Previously the interactive prompt defaulted to `series_type = "level"`, `method = "percentage"` if confirmed, which transformed the series; **this is a behavior change for any series that is actually in levels** — convert those to rates first with `ets()`/`as_ets()` (see `vignette("koma-extended-timeseries")`). If sibling `koma_ts` series carry custom attributes beyond `series_type`/`method` (e.g. a project-specific `value_type`), those are backfilled as `NA` on the converted series, since `as_mets()` requires every series in `ts_data` to share the same attribute names. The affected series names are stored on the returned `koma_estimate` object as `plain_ts_names` and are also shown whenever the object is printed. `forecast()` still returns `koma_ts` for these series in `mean`/`median`/`quantiles` (not plain `ts`), since printing, formatting, and plotting a forecast require every series in those lists to share a uniform `koma_ts` schema.

# koma 0.3.1

* Added `\value` documentation tags to `print.koma_forecast` and `print.koma_seq` to comply with CRAN policy.

# koma 0.3.0

# koma 0.2.2

# koma 0.2.2.9000

# koma 0.2.2

* Fixed frequency-dependent forecasting, estimation, evaluation, weighting, and plotting paths so single-frequency monthly and yearly data work correctly.
* Fixed forecast plotting labels for non-quarterly data.
* Fixed `estimate_sem()` for multisession futures.
* Improved input validation for forecasting and `system_of_equations()`.
* Improved `koma_ts` metadata handling for attribute preservation across transformations.

# koma 0.2.1

* Fix pkgdown site build by adding package URL and completing `_pkgdown.yml` reference index.

# koma 0.2.0

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
* Added running-mean diagnostics via `running_mean()` and `running_mean_plot()`.
* Added autocorrelation diagnostics via `acf_plot()`.

# koma 0.1.0

* Initial github release.
