# Validate the Priors Stored in a System of Equations

`validate_priors()` checks the prior syntax in the equation strings when
the system is created. The priors can be changed afterwards in
`sys_eq$priors`, so this checks the stored priors again before they are
used.

## Usage

``` r
validate_sys_eq_priors(sys_eq, call = rlang::caller_env())
```

## Arguments

- sys_eq:

  A `koma_seq` object.

- call:

  The environment from which the error is called.

## Value

`TRUE` invisibly, or an error listing the invalid priors.
