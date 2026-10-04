# Validate a Start/End Date Range in `dates`

Validate a Start/End Date Range in `dates`

## Usage

``` r
validate_date_range(dates, field, frequency = 4, call = rlang::caller_env())
```

## Arguments

- dates:

  A list of date ranges, e.g. `dates$estimation`.

- field:

  Name of the range in `dates` to validate, e.g. "estimation".

- frequency:

  Frequency of the time series.

- call:

  The environment from which the error is called.

## Value

Invisibly `NULL`; aborts if the range is invalid.
