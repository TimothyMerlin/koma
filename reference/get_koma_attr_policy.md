# Look up the registered policy for a koma_ts attribute

Look up the registered policy for a koma_ts attribute

## Usage

``` r
get_koma_attr_policy(attr)
```

## Arguments

- attr:

  Name of the attribute.

## Value

The list of handlers (`merge`, `lag`, `window`, `na_omit`) registered
via
[`set_koma_attr_policy()`](https://timothymerlin.github.io/koma/reference/set_koma_attr_policy.md)
for `attr`, or `NULL` if no policy has been registered – in which case
merges fall back to
[`default_koma_attr_merge()`](https://timothymerlin.github.io/koma/reference/default_koma_attr_merge.md)
and `lag`/`window`/`na_omit` leave the attribute value unchanged.

## Examples

``` r
set_koma_attr_policy(
  "anker",
  merge = function(left, right, attr, op = NULL, template = NULL) NA
)
get_koma_attr_policy("anker")
#> $merge
#> function (left, right, attr, op = NULL, template = NULL) 
#> NA
#> <environment: 0x562c9364d9a8>
#> 
#> $lag
#> NULL
#> 
#> $window
#> NULL
#> 
#> $na_omit
#> NULL
#> 
get_koma_attr_policy("some_other_attribute") # NULL: nothing registered
#> NULL
```
