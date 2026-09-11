# Contributing to koma

Thanks for your interest in contributing! This outlines how to propose a
change.

## Small fixes

Typos, broken links, and small documentation fixes can go straight to a pull
request.

## Bigger changes

For anything larger (new features, behavior changes, bug fixes that touch
estimation/forecasting logic), please open an
[issue](https://github.com/TimothyMerlin/koma/issues) first to discuss the
approach before investing time in a PR.

## Development setup

```r
# Fork the repo on GitHub, then clone your fork, then:
pak::pak(dependencies = TRUE)
devtools::load_all()
```

## Making changes

- Document exported functions with roxygen2 (markdown syntax is enabled);
  run `devtools::document()` after editing `@param`/`@return`/etc. and commit
  the regenerated files in `man/` and `NAMESPACE`.
- Add or update tests in `tests/testthat/` for any behavior change. Tests use
  testthat 3e; wrap slow/full MCMC end-to-end tests with `skip_on_cran()`.
- Add a bullet to the top (unreleased) section of `NEWS.md` describing the
  change from a user's perspective. Flag any breaking change explicitly.

## Before opening a pull request

```r
devtools::test()
devtools::check()
```

`R CMD check` should report 0 errors and 0 warnings (the CI workflow
`R-CMD-check.yaml` runs the same check on push/PR).

## Commit messages

This repo follows [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>(<scope>): <summary> [#<issue>]
```

- `type` is one of `feat`, `fix`, `perf`, `docs`, `test`, `refactor`, `chore`.
- `scope` is the affected area (e.g. `estimate`, `forecast`, `readme`).
- `[#<issue>]` is optional, for changes tied to a tracked issue.

Examples from the history: `fix(estimate): support identity equations in
any position [#137]`, `perf(matrix-construction): use crossprod()/solve(A,B)
in Gibbs-sampler [#21]`, `docs(readme): use pak::pak() for dev install`.

## Pull requests

Describe what changed and why, link any related issue, and make sure CI
(R CMD check, test coverage) passes. Squash or clean up the commit history
if it accumulated a lot of "wip" commits along the way.

## Release process (maintainers)

1. `usethis::use_version()` to bump the version in `DESCRIPTION`.
2. Commit: `git commit -m "Incrementing version to x.x.x"`.
3. Tag the release and move the movable `latest` tag:
   ```bash
   git tag x.x.x
   git tag -f latest
   ```
4. Push commits and tags: `git push && git push origin --tags`.
