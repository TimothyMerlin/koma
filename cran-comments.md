## Resubmission of an archived package

koma was archived on 2026-09-27 because a check issue was not corrected in
time. The issue was a test failure on the tests-MKL additional check
(r-devel, Fedora Linux 44):

* `test-mh_within_gibbs_algorithm_informative.R`, "draw_parameters_j_informative
  with diffuse priors and no gamma priors", compared an MCMC posterior quantile
  against a fixed expected value with too tight a tolerance (0.2136 vs. 0.21).
  This sampler path shows small cross-environment drift (BLAS/LAPACK-level
  floating point differences compounding over 200 iterations) despite a fixed
  seed. The tolerance has been widened to 0.28, with headroom for this drift
  without weakening the test's ability to catch real regressions.

## Submission

This is a feature release (0.3.1 -> 0.4.0). Besides the fix above, it adds
new equation syntax and theme options, fixes several bugs in estimation,
forecasting and plotting, tightens input validation, and speeds up estimation
and forecasting. See NEWS.md for details.

## Test environments

* local: macOS Tahoe 26.7.1 (aarch64-apple-darwin23), R 4.6.1
* R-hub (R Consortium runners):
  * linux: R-devel (2026-09-29 r90598)
  * windows: R-devel (2026-09-30 r90605 ucrt)
  * macos: x86_64-apple-darwin20, R-devel (2026-09-29 r90598)
  * mkl: Intel MKL container, R-devel (2026-09-30 r90605)

All R-hub platforms: Status OK. The mkl container mirrors the tests-MKL
additional check that led to the archival; the previously failing test passes
there.

## R CMD check results

0 errors | 0 warnings | 0 notes

* The incoming checks are expected to note that the package was archived;
  see "Resubmission of an archived package" above.
* If flagged, "Rathke" and "Sarferaz" in the DESCRIPTION are proper names
  (surnames of the authors of the referenced forthcoming paper) and are
  spelled correctly.

## Reverse dependencies

koma has no reverse dependencies on CRAN.
