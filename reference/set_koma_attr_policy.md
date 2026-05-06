# Register metadata behavior for a koma_ts attribute

Register metadata behavior for a koma_ts attribute

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
