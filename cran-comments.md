## Resubmission

This is a resubmission. The following changes were made in response to reviewer feedback:

* Added `\value` tags to `print.koma_forecast.Rd` and `print.koma_seq.Rd` describing the return value and side effects of each print method.
* Reduced the overall check time, which previously exceeded 10 minutes
  (mainly `checking tests`, [515s]). The test suite included many
  full end-to-end MCMC estimation/forecast runs; the slowest and least
  essential of these (~37 of ~340 tests) are now skipped on CRAN via
  `testthat::skip_on_cran()`. They continue to run in our own CI and local
  development (where `NOT_CRAN` is set), so coverage of the sampler's
  statistical behavior is unaffected outside of CRAN's own check.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* "Rathke" and "Sarferaz" are flagged as possibly misspelled words in the DESCRIPTION. These are proper names (surnames of the authors of the referenced forthcoming paper) and are spelled correctly.
