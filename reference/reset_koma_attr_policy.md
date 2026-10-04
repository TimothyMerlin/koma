# Remove a registered koma_ts attribute policy

Undoes a previous
[`set_koma_attr_policy()`](https://timothymerlin.github.io/koma/reference/set_koma_attr_policy.md)
call, restoring the default behavior (see
[`default_koma_attr_merge()`](https://timothymerlin.github.io/koma/reference/default_koma_attr_merge.md))
for `attr`. Since policies are registered globally for the session, this
is the only way to undo one short of restarting R.

## Usage

``` r
reset_koma_attr_policy(attr = NULL)
```

## Arguments

- attr:

  Name of the attribute to reset. If `NULL`, every registered policy is
  removed.

## Value

The removed policy/policies, invisibly (as returned by
[`get_koma_attr_policy()`](https://timothymerlin.github.io/koma/reference/get_koma_attr_policy.md)),
or `NULL` if nothing was registered for `attr`.

## Examples

``` r
set_koma_attr_policy(
  "anker",
  merge = function(left, right, attr, op = NULL, template = NULL) NA
)
reset_koma_attr_policy("anker")
get_koma_attr_policy("anker") # NULL again
#> NULL
```
