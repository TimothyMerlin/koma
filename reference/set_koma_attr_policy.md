# Register metadata behavior for a koma_ts attribute

Custom `koma_ts` attributes (e.g. `anker`) are combined using
[`default_koma_attr_merge()`](https://timothymerlin.github.io/koma/reference/default_koma_attr_merge.md)
unless a policy is registered here. The default policy requires both
operands' values to be identical and errors otherwise – use this
function to define how an attribute should behave under arithmetic
(`merge`), [`stats::lag()`](https://rdrr.io/r/stats/lag.html),
[`stats::window()`](https://rdrr.io/r/stats/window.html), and
[`stats::na.omit()`](https://rdrr.io/r/stats/na.fail.html).

## Usage

``` r
set_koma_attr_policy(
  attr,
  merge = NULL,
  lag = NULL,
  window = NULL,
  na_omit = NULL
)
```

## Arguments

- attr:

  Name of the attribute.

- merge:

  Optional binary merge handler with signature
  `function(left, right, attr, op = NULL, template = NULL)`.

- lag:

  Optional lag handler with signature
  `function(value, attr, template = NULL, ...)`.

- window:

  Optional window handler with signature
  `function(value, attr, template = NULL, ...)`.

- na_omit:

  Optional `na.omit` handler with signature
  `function(value, attr, template = NULL, ...)`.

## Value

The registered policy, invisibly.

## Details

Policies are registered globally, for the current R session: once set,
`attr` is handled the same way for *every* `koma_ts` operation, for
every pair of series, not just the ones that prompted the call. There is
no `unset`/reset function, so a policy stays in effect until it is
overwritten or the session ends. A permissive `merge` handler (e.g. one
that silently drops a mismatch) can mask a genuine bug in an unrelated
part of the codebase where that same mismatch should have raised an
error – register the narrowest handler that solves your actual case, not
a blanket "never error" one.

## Examples

``` r
# By default, koma_ts arithmetic errors if an attribute's values differ
# between operands (see `default_koma_attr_merge()`) -- e.g. subtracting
# two "rate" series to build an identity like an inflation spread, where
# each side carries its own (different) base-level anchor. A linear
# combination of two series' rates has no single base level to invert
# back to, so here dropping the anchor on mismatch is the right call.
#
# This changes how *all* koma_ts arithmetic handles "anker" for the rest
# of the session (see Details) -- only register this once you've decided
# that "anker" values are never meant to be compared for equality
# elsewhere in your workflow.
set_koma_attr_policy(
  "anker",
  merge = function(left, right, attr, op = NULL, template = NULL) NA
)
```
