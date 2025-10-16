KOMA - Large Macroeconomic Model
================

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![codecov](https://codecov.io/gh/TimothyMerlin/koma/branch/main/graph/badge.svg?token=8X0PR1F6TP)](https://codecov.io/gh/TimothyMerlin/koma)

> **⚠️ Beta Release**: This package is in active development. Core
> features are working but exported functions may change and bugs are
> possible. Please report issues on GitHub.

## Installation

To install the latest **release** of the KOMA package from GitHub:

``` r
# Install from GitHub
devtools::install_github("timothymerlin/koma@latest")

# Or using renv
renv::install("github::timothymerlin/koma@latest", rebuild = TRUE)
```

To install the latest **development version** of the KOMA package from
GitHub:

``` r
# Install from GitHub
devtools::install_github("timothymerlin/koma")

# Or using renv
renv::install("github::timothymerlin/koma", rebuild = TRUE)
```

## Development

### Creating a new Version

Increment the package version using `usethis`:

``` r
# Increment the version number (patch/minor/major)
usethis::use_version()
```

This updates the `DESCRIPTION` file.

#### Commit changes and tag the release

After updating the version:

``` bash
# Stage all changes
git add .

# Commit with a clear message
git commit -m "Incrementing version to x.x.x"

# Tag the release (protected)
git tag x.x.x

# Tag the latest release (unprotected, movable)
git tag -f latest
```

  - `x.x.x` is the new semantic version (e.g. `0.1.0`).
  - Protected tags (like `x.x.x`) are immutable to ensure release
    integrity.
  - `latest` is an uprotected tag that can be moved to point to the
    newest release.

#### Push branch and tags

``` bash
# Push commits
git push

# Push both tags
git push origin --tags
```
