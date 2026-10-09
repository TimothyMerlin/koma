## Early update

This is a bug-fix release one week after 0.4.0 (published 2026-10-02). We
apologize for the quick resubmission. After the release we found bugs that
silently give wrong results without any error or warning. They have been in
koma since its first CRAN release, so users of all published versions are
affected. We therefore ask for an early update instead of waiting the usual
interval:

* A prior on a contemporaneous endogenous regressor (e.g. `{0,1}gdp` with
  endogenous `gdp`) made the estimate ignore the data: a line break before a
  `+` dropped the likelihood term, so the posterior was just the prior.
* A tight prior on such a regressor froze the sampler at its start value,
  because the prior density underflowed to zero.
* `forecast()` could drop or misplace lagged endogenous regressors (e.g. for
  lags of 10 or more, or variable names that share a prefix).
* `estimate(..., estimates = )` returned the previous draws when the model had
  changed. This option is now ignored with a warning and all equations are
  re-estimated.
* Identity weights mixing endogenous and exogenous components could overwrite
  each other, so estimation and forecasting used the wrong weight.

Before this submission we reviewed the estimation, sampler and forecasting
code to find remaining problems in the same areas, and fixed them in this
release, so that no further early update should be needed. The other fixes
correct the identification check, which wrongly rejected some identified
models, and improve input validation and error messages. See NEWS.md for
details.

The release contains two intentional changes that may affect user code: the
`estimates` argument of `estimate()` is ignored (see above), and identity
weights are renamed (e.g. `theta_gamma6_4` instead of `theta6_4`) to prevent
the name collisions above. Both are documented in NEWS.md.

## Test environments

* local: macOS Tahoe 26.7.1 (aarch64-apple-darwin23), R 4.6.1
* R-hub (R Consortium runners):
  * linux: R-devel (2026-10-06 r90643)
  * windows: R-devel (2026-10-08 r90650 ucrt)
  * macos: x86_64-apple-darwin20, R-devel (2026-10-08 r90646)
  * mkl: Intel MKL container, R-devel (2026-10-06 r90643)
* win-builder: R-devel, R-release

## R CMD check results

0 errors | 0 warnings | 0 notes

* CRAN's incoming check may note the short interval since 0.4.0; see
  "Early update" above.
* If flagged, "Rathke" and "Sarferaz" in the DESCRIPTION are proper names
  (surnames of the authors of the referenced forthcoming paper) and are
  spelled correctly.

## Reverse dependencies

koma has no reverse dependencies on CRAN.
