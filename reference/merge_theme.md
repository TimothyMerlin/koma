# Merge a Partial Theme with the Default Theme

Recursively merges a user-supplied (possibly incomplete) theme list with
the defaults from
[`init_koma_theme()`](https://timothymerlin.github.io/koma/reference/init_koma_theme.md),
so that any omitted fields fall back to their default values.

## Usage

``` r
merge_theme(user_theme)
```

## Arguments

- user_theme:

  A (possibly partial) named list of theme overrides.

## Value

A complete theme list equivalent to
[`init_koma_theme()`](https://timothymerlin.github.io/koma/reference/init_koma_theme.md)
with the user's values applied on top.
