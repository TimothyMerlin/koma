# Default merge behavior for a koma_ts attribute

Used by
[`merge_koma_attrs()`](https://timothymerlin.github.io/koma/reference/merge_koma_attrs.md)
for any attribute without a policy registered via
[`set_koma_attr_policy()`](https://timothymerlin.github.io/koma/reference/set_koma_attr_policy.md).
`NULL` operands are passed through unchanged; otherwise the two values
must be identical (per
[`base::all.equal()`](https://rdrr.io/r/base/all.equal.html)) or the
merge errors, since there is no generic way to combine two different
metadata values (e.g. two different `anker` anchors) without
attribute-specific knowledge of what the operation means.

## Usage

``` r
default_koma_attr_merge(left, right, attr, op = NULL, template = NULL)
```

## Arguments

- left:

  The first operand's attribute value, or `NULL` if absent.

- right:

  The second operand's attribute value, or `NULL` if absent.

- attr:

  Name of the attribute, used in the error message.

- op:

  Name of the arithmetic operation being performed (e.g. `"-"`), used in
  the error message.

- template:

  Unused; accepted for signature compatibility with handlers registered
  via
  [`set_koma_attr_policy()`](https://timothymerlin.github.io/koma/reference/set_koma_attr_policy.md).

## Value

`left` (or `right`, when `left` is `NULL`) if the values match or one
side is absent. Errors otherwise.
