KOMA - Large Macroeconomic Model
================

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![codecov](https://codecov.io/gh/TimothyMerlin/koma/branch/main/graph/badge.svg?token=8X0PR1F6TP)](https://app.codecov.io/gh/TimothyMerlin/koma)

**koma** is an R package for Bayesian estimation of simultaneous
equation models (SEMs) using Metropolis-within-Gibbs Markov Chain Monte
Carlo (MCMC) methods.

## Installation

Install the released version from CRAN:

``` r
install.packages("koma")
```

Or install the latest **development version** from GitHub:

``` r
pak::pak("TimothyMerlin/koma")
```

## Documentation

- **Getting started** → [Getting started with
  koma](https://timothymerlin.github.io/koma/articles/koma-getting-started.html)
- **Equation syntax** → [Equation syntax
  reference](https://timothymerlin.github.io/koma/articles/koma-equations.html)
- **Extended time series** → [Extended time series
  (ets)](https://timothymerlin.github.io/koma/articles/koma-extended-timeseries.html)
- **Parallelization** → [Executing koma in
  parallel](https://timothymerlin.github.io/koma/articles/koma-parallel.html)
- **Example: Klein model** → [Estimating Klein’s Model
  I](https://timothymerlin.github.io/koma/articles/koma-klein.html)
- **Example: small macro model** → [Estimating small macro model for
  Switzerland](https://timothymerlin.github.io/koma/articles/koma-small-macro-model.html)
- **Example: error correction** → [Error correction in a small open
  economy model](https://timothymerlin.github.io/koma/articles/koma-error-correction.html)
- **Diagnostics** → [MCMC diagnostics for an estimated
  SEM](https://timothymerlin.github.io/koma/articles/koma-diagnostics.html)
- **HPD intervals** → [Highest probability density (HDR and
  HDI)](https://timothymerlin.github.io/koma/articles/koma-hpd.html)

See the [full documentation site](https://timothymerlin.github.io/koma/)
for the complete function reference.

## Contributing

Contributions are welcome! See
[CONTRIBUTING.md](https://github.com/TimothyMerlin/koma/blob/main/CONTRIBUTING.md)
for the development setup and pull request process. Bug reports and feature
requests are welcome via
[GitHub issues](https://github.com/TimothyMerlin/koma/issues).
