# koma 0.4.1

## Breaking changes

* `estimate(..., estimates = )` is now ignored with a warning, and all equations are always estimated. The check for changed equations missed changes to contemporaneous regressors, priors, the sample and the data, and then returned stale draws.
* Identity weights are now named `theta_gamma<eq>_<col>` for endogenous components and `theta_beta<eq>_<row>` for exogenous ones (e.g. `theta_gamma6_4` instead of `theta6_4`). Code that sets weights by the old names, e.g. `sys_eq$identities$gdp$weights$theta6_4 <- 0.5`, no longer has any effect; use the new names. The `sys_eq` in `simulated_sem` was rebuilt with the new names.

## New features

* Printing a system of equations now shows its priors, e.g. `{0.4, 0.1} gdp`, the error-term prior and the equation settings, e.g. `[tau = 1.2]`. The print reads them from the object, so it reflects later changes to them.
* `estimate()` now checks the priors stored in the system of equations and errors on invalid ones, e.g. a prior for a term that is not in the equation or a variance that is not positive. Before, priors changed by hand after `system_of_equations()` were not checked.

## Changed results

The following fixes change the estimates or forecasts of affected models. Re-estimate them.

* Priors on contemporaneous endogenous regressors (e.g. `{0,1}gdp` with endogenous `gdp`) ignored the data, so the posterior was just the prior. It now combines prior and likelihood.
* Tight priors on such regressors far from the data estimate (e.g. `{1000,0.00001}gdp`) left the chain stuck at its start value. It now moves towards the prior, but can need many draws to converge; check the trace plot.
* A prior in front of `dummies()` (e.g. `{0,1}dummies(covid, 1:8)`) now applies to all dummies, not only the first.
* Identity weights that mix endogenous and exogenous components (e.g. `y == 0.3*b + 0.7*x1`) could overwrite each other, so estimation, forecasting and the identification check failed or used the wrong weight. Rebuild such models with `system_of_equations()`.
* `forecast()` could drop or misplace lagged endogenous regressors when an equation skips lower lags (e.g. only `x.L(4)`), uses lags of 10 or more, or has variable names that share a prefix or contain digits. Estimation is affected only where a ragged edge is filled.
* The identification check no longer advances the random number generator, so models with contemporaneous endogenous regressors give different draws for the same seed.
* For equations with priors and contemporaneous endogenous regressors, the saved error variance used the coefficients of the previous draw. Density forecasts change slightly.
* The starting value of the error covariance for equations with priors was far too large. This only matters with `burnin_ratio = 0` or a very short burn-in.
* `system_of_equations()` now rejects priors with a variance or error-term scale that is not positive (e.g. `{0,-0.01}gdp`). A negative variance used to pass silently and push the estimate away from the prior mean; a zero variance failed later with an unclear error.

## Bug fixes

### Identification

* `model_identification()` failed or wrongly rejected identified models for:
  * simultaneous systems with at most one lagged or exogenous variable (e.g. `a ~ b, b ~ a`);
  * identities with exogenous components (e.g. `y == 1*c + 1*i + 1*g` with exogenous `i` and `g`);
  * equations without a constant (e.g. `b ~ 0 + a + x1 + x2`);
  * identities with dynamic weights, when called directly on the output of `system_of_equations()`;
  * large systems with parameter names that share a prefix (e.g. `theta_gamma6_4` and `theta_gamma6_40`).
* The identification error now lists only the failing equations, with the counts behind the order condition or the rank behind the rank condition.

### Estimation and forecasting

* Sampler initialization now handles equations whose exogenous coefficients, including the intercept, are all restricted to zero.
* If the sampler cannot be started because the target has no proper maximum in gamma (e.g. a constant endogenous regressor), `estimate()` now says so, instead of failing with `Lapack routine dgesv: system is exactly singular` or a Cholesky error.
* Both samplers now handle equations with no free coefficients, drawing only their error variance.
* The saved `theta_jw` draws of equations without priors now hold the full coefficient vector, with the restricted coefficients as zeros, in the same form as for equations with priors. They used to hold only the free coefficients, in a different order.
* `estimate()` now stops with an error that names each failed equation and the reason, instead of returning an estimate that fails later in `summary()` or `forecast()`.
* Sampler settings that save no draws (e.g. `ndraws = 0`, or `nstore` larger than the draws after burn-in) now fail early.
* The sampler proposal scale `tau` must now contain finite, positive numbers. In particular, `tau = 0` is rejected instead of silently freezing the gamma draws.
* Invalid error covariance matrices (not a matrix, not square, or of the wrong size) now produce clear errors.
* `forecast()` now stops if `character_beta_matrix` is missing, instead of silently omitting lag dynamics.
* Forecast identity checks were skipped when exogenous series were supplied; incorrect identities now warn.
* The Metropolis-Hastings step now guards against an acceptance probability that is not a number. No known model triggers this.

## Performance

* `estimate()` is about 2 to 3 times faster than in 0.4.0 (e.g. the small macro model vignette: about 6 s to 2 s). The Gibbs samplers now compute per-equation quantities (regressor subsets, the inverse of `X'X`, the prior precision) once instead of in every draw, and work on plain matrices. The draws are unchanged.

# koma 0.4.0

## Breaking changes

* `estimate()` no longer interactively prompts (via `readline()`) when `ts_data` contains plain `ts` series instead of `koma_ts`. It now silently converts them to `koma_ts` with `series_type = "rate"`, `method = "none"` (i.e. assumes the series is already in rates, the form the model estimates on, and applies no transformation) and emits a warning listing the affected series. Previously the interactive prompt defaulted to `series_type = "level"`, `method = "percentage"` if confirmed, which transformed the series; **this is a behavior change for any series that is actually in levels** — convert those to rates first with `ets()`/`as_ets()` (see `vignette("koma-extended-timeseries")`). If sibling `koma_ts` series carry custom attributes beyond `series_type`/`method` (e.g. a project-specific `value_type`), those are backfilled as `NA` on the converted series, since `as_mets()` requires every series in `ts_data` to share the same attribute names. The affected series names are stored on the returned `koma_estimate` object as `plain_ts_names` and are also shown whenever the object is printed. `forecast()` still returns `koma_ts` for these series in `mean`/`median`/`quantiles` (not plain `ts`), since printing, formatting, and plotting a forecast require every series in those lists to share a uniform `koma_ts` schema.

## New features

* Added `dummies(prefix, spec)` equation syntax, e.g. `dummies(covid, 1:8)`, as shorthand for a set of dummy variables (`covid_1+covid_2+...+covid_8`). `spec` uses the same range/list syntax as lag notation. It's expanded before validation, so the expanded names must still be declared in `exogenous_variables` like any other regressor; see `vignette("koma-equations")`.
* Identity (`==`) equations can now appear anywhere in the system of equations, not only after every stochastic (`~`) equation. Previously, an identity placed before a stochastic equation caused that equation's column to be estimated in its place, leaving the real equation unestimated and surfacing only later as an opaque `<variable> not found in the estimates` error (#137).
* Added `digits` to `init_koma_theme()`, a list with `quarterly`, `level`, and `annual` elements controlling the decimal places shown in quarterly growth-rate hover values, level hover values, and annual growth-rate annotations respectively (previously hardcoded to 2, 2, and 1 decimal place; the annual default is now 2 as well).
* Exported `set_koma_attr_policy()`, previously internal-only, since the error raised when merging mismatched `koma_ts` attributes (e.g. `anker`) directs users to call it.
* Added `get_koma_attr_policy()` to inspect a registered attribute policy, and `reset_koma_attr_policy()` to remove one (or all) registered policies, since policies are shared for the whole R session.
* The estimation progress bar now advances with the Gibbs draws instead of once per equation, so it moves continuously rather than in large jumps while an equation is sampled. It updates at most every 0.5 seconds per worker to keep the overhead small. When re-estimating only some equations (`estimate(..., estimates = )`), the bar now counts only those equations instead of stopping short of 100%.

## Bug fixes

* Fixed Gibbs sampler thinning: `nstore` was ignored and every post-burn-in draw was kept, while the reported `nsave` understated the stored draws. The samplers now keep every `nstore`-th draw after burn-in and `nsave` is `floor((ndraws - burnin) / nstore)`. Settings that were previously truncated silently now fail early: `nstore` must be at least 1, `burnin_ratio` must lie in `[0, 1)`, and `ndraws * burnin_ratio` must be a whole number.
* Fixed priors in front of lag shorthand. `{0,1000}x.L(1,3,5)` now sets the prior on `x.L(1)`, `x.L(3)` and `x.L(5)`, and the same applies to ranges (`x.L(1:3)`) and `lag(x, 1:3)`. Previously the comma form silently attached the prior to the unlagged `x` instead, and the range and `lag()` forms silently dropped the prior.
* Fixed `plot()`/`plotli()` plotly titles: an `ifelse()` silently coerced a factor variable name to its underlying integer code instead of showing the variable name; replaced with `if`/`else`.
* Fixed `plot()`/`plotli()` so a partial `theme` argument (missing some fields) no longer drops straight to `init_koma_theme()`'s defaults for every field; missing fields are now recursively filled in from the defaults via a new internal `merge_theme()` helper.
* Fixed `plot()`/`plotli()` coloring the current year's annual growth-rate annotation grey (in-sample) even when some of its quarters are still forecasted. `to_long()` now classifies an observation as in-sample only once its full period (not just its start) has elapsed by the forecast start date, which matters for `growth_annual` data since an annual timestamp only marks the start of the year.
* Fixed the level fan chart in `plot(..., fan = TRUE)`. The bands compounded growth-rate quantiles, i.e. described a path where growth sits at the same extreme quantile in every period, which made them far too wide and strongly asymmetric. Each forecast draw is now converted to a level path first and the bands are the quantiles of these paths per horizon. Also fixed complementary quantiles such as `c(0.05, 0.95)` drawing the same band twice.
* Fixed the fan chart in `plot(..., fan = TRUE)` when the theme rebases the level (`init_koma_theme(index = ...)`): the level line was rebased but the fan bands were not, so they were drawn on a different scale. The bands are now scaled by the same factor.
* Fixed the `variables` check in `plot()` for forecasts: its condition used `||` instead of `&&`, so non-character, empty or `NA` values passed and only failed later with unrelated errors. `variables` must now be a non-empty character vector without `NA`.
* Fixed density forecasts with `restrictions` under `future::plan(future::multisession)`: every draw failed with "All forecast draws failed" because the restrictions reached the workers as an unevaluated promise referring to the caller's global environment.
* Fixed `model_evaluation()` ignoring `options$approximate`: it was passed to `forecast()` in the wrong place, so every evaluation ran the slower density forecast. `variables = NULL` now evaluates all endogenous variables as documented instead of returning an empty result. `model_evaluation()` now also validates its inputs (`variables` must be endogenous, `horizon` a positive whole number that fits into `dates$forecast`, `evaluate_on_levels` a single logical, and `dates$estimation`/`dates$forecast` valid ranges); a horizon longer than the forecast window previously returned `NaN` RMSEs.

## Error handling and validation

* `estimate()` now aborts with an informative error if `future::plan(future::multicore)` is active on macOS, instead of risking a silent segfault: Apple's Accelerate framework (used by `eigen()`) is not fork-safe. Switch to `future::plan(future::multisession)` instead.
* Rank-deficient `x_matrix` caused by a lagged identity coinciding with lags of its own components (e.g. `gdp.L(1)` alongside lags of all of `gdp`'s components) is now caught early with an informative error identifying the collinear variables, instead of surfacing later as an opaque `"computationally singular"` error inside the Gibbs sampler (#135).
* A variable declared as both a stochastic equation (`~`) and an identity (`==`) is now caught early with an informative error naming the variable and both conflicting equations, instead of surfacing later as an opaque `'names' attribute [...] must be the same length as the vector` error deep inside identity/weight construction.
* Hardened parsing of `system_of_equations()` input. Malformed priors (e.g. a wrong separator) now error instead of silently becoming `NA`, and negative-mean priors such as `{-0.4,0.1}` are no longer wrongly rejected. An equation with its own dependent variable on the right-hand side without a lag (e.g. `gdp ~ gdp + x1`) now errors instead of being silently dropped. Lag and `dummies()` specs with a stray extra colon (e.g. `.L(1:2:3)`) now error instead of being truncated. A trailing operator (e.g. `y ~`) and missing identity weights now produce messages that name the actual problem. The `[key=val,...]` settings block is now evaluated against an allowlist of functions instead of in `baseenv()`, so equation strings can no longer run arbitrary code.
* Bracketed index terms such as `covid[1:3]`, which were never supported, now fail validation with an error. Previously they passed validation, and a trailing one was mistaken for an equation settings block (`[key=val]`) and silently dropped from the equation (#138). Use `dummies(covid, 1:3)` for a set of dummy variables.
* The "Estimation start moved" warning now lists every series whose first non-`NA` observation falls after the requested estimation start, so it is clear which missing or lag-induced `NA`s shortened the sample.
* `forecast()` now validates `restrictions` before forecasting: each entry must be named after a variable and contain numeric `value` and `horizon` vectors of equal length, without missing values, with whole horizons within the forecast horizon and no duplicates. Malformed restrictions previously failed inside every draw with a misleading "All forecast draws failed". Restrictions for non-endogenous variables are dropped with a single warning before the draws start.
* The "Forecast horizon shortened" warning in `forecast()` is now issued once. It was raised per draw with a shared "warn once" flag, which multisession workers do not share, so density forecasts under `future::plan(future::multisession)` repeated the warning. The horizon is now shortened once before the draws start.
* Density forecasts now report the actual errors of failed draws. Previously they were dropped and only "All forecast draws failed. Likely causes: redundant/incompatible restrictions" was shown, whatever the cause. Errors and warnings of individual draws (e.g. a singular or ill-conditioned restriction system) are now collected and reported once, grouped by message with the number of draws affected, instead of being repeated for every draw. The first draw error is chained as the parent error, so `rlang::last_trace()` shows where the draw failed.
* Restrictions beyond a forecast horizon that was shortened because exogenous data end early now fail once with a clear error before the draws start, instead of failing in every draw.
* `rebase()` now validates the index period. A period partly outside the series previously computed the base from the available part only (with just a `window()` warning), a period fully outside failed with a cryptic `'start' cannot be after 'end'`, and `NA` values in the period silently turned the whole series into `NA`. These cases now fail with a clear error.

## Performance

* Sped up estimation by using `crossprod()`/`tcrossprod()` and the two-argument `solve(A, b)` instead of `t(X) %*% X` and `solve(A) %*% b` in the OLS helpers called on every Gibbs sampler draw.
* Sped up the Gibbs/Metropolis-Hastings sampler further: replaced `Matrix::solve()`/`chol()`/`det()` with their base R equivalents in the sampler's hot path (the `Matrix` generics pay S4 dispatch overhead even on plain matrices, which every call here is), and hoisted the per-equation-invariant `crossprod(x_matrix)` (and the restricted-column `crossprod(x_b)` used for `beta_hat`) out of the `ndraws`-iteration loop instead of recomputing it on every call. Draws are numerically unchanged (bit-identical given the same seed); benchmarks show roughly 10-30% faster estimation depending on model size, on top of the earlier `crossprod()`/`solve(A, b)` fix.
* Sped up `forecast()`: the positions of the lagged endogenous regressors were located with regular expressions in every forecast draw, although they depend only on the system of equations. They are now found once per forecast. Forecasts are numerically unchanged (bit-identical given the same seed); a density forecast with 1000 draws of the simulated example model is about 40% faster (1.29 s to 0.79 s).
* `estimate()` no longer serializes its whole calling environment to every parallel worker. The data matrices reached the worker closure as unevaluated promises, which kept the `estimate()` frame alive, including `ts_data` and, when re-estimating, the previous `estimates` with all draws. In a small example re-estimation, the data sent per worker dropped from about 2 MB to 160 KB.

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
