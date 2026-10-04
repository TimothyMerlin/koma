# Validate Forecast Restrictions

Checks the structure of user-supplied forecast restrictions before any
forecast draw is computed. Without this check, malformed restrictions
only fail inside the individual draws, where density forecasts catch the
errors per draw and report a misleading "All forecast draws failed".

## Usage

``` r
validate_restrictions(
  restrictions,
  endogenous_variables,
  horizon,
  call = rlang::caller_env()
)
```

## Arguments

- restrictions:

  `NULL` or a named list. Each element is a list with numeric `value`
  and `horizon` vectors of equal length.

- endogenous_variables:

  Character vector of endogenous variable names.

- horizon:

  Integer forecast horizon.

- call:

  The environment from which the error is called.

## Value

The restrictions without entries for non-endogenous variables.

## Details

Restrictions for variables that are not endogenous are dropped with a
single warning.
