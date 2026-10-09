# Expand Indexed Dummy-Variable Shorthand

Rewrites `dummies(prefix, spec)` calls into an additive series of plain
variable names `prefix_1+prefix_2+...`, where the indices come from
`spec` (parsed by
[`parse_index_spec()`](https://timothymerlin.github.io/koma/reference/parse_index_spec.md),
the same "single value or range, comma-separated" grammar used by lag
notation). For example, `dummies(covid, 1:8)` becomes
`covid_1+covid_2+...+covid_8`. A prior in front of the call is repeated
for every dummy, so `{0,1}dummies(covid, 1:2)` becomes
`{0,1}covid_1+{0,1}covid_2`.

This runs before any other equation processing (priors, settings,
validation), so the expanded terms are indistinguishable from terms the
user typed by hand for every downstream step - including
[`validate_completeness()`](https://timothymerlin.github.io/koma/reference/validate_completeness.md),
which will require each expanded name (e.g. `covid_1`) to be declared in
`exogenous_variables` like any other regressor. Unlike lagged variables,
there is no separate "base variable" backing a dummy family to validate
instead, so no such exemption exists.

Any `dummies(...)` call is matched loosely first (so a malformed one is
actually caught here, rather than left untouched to fail later as an
unhelpful generic "invalid variable"), then the prefix and spec are
validated strictly, raising
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
with the offending call shown verbatim if either is invalid.

## Usage

``` r
expand_dummies(equations)
```

## Arguments

- equations:

  A character vector of equation strings.

## Value

A character vector of equations with every `dummies(...)` call expanded.
