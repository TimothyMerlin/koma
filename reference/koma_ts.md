# Extended Time-Series Object

The function `ets` is used to create an extended time-series (ets)
object. Any additional attribute passed is saved as such in the ets.

`as_ets` and `is_ets` coerce an object to a time- series and test
whether an object is a time series of class `koma_ts`.

## Usage

``` r
ets(
  data = NA,
  start = NULL,
  end = NULL,
  frequency = NULL,
  deltat = NULL,
  ts.eps = getOption("ts.eps"),
  ...
)

as_ets(x = stats::ts(), ...)

is_ets(x)
```

## Arguments

- data:

  a vector or matrix of the observed time-series values. A data frame
  will be coerced to a numeric matrix via `data.matrix`. (See also
  ‘Details’.)

- start:

  the time of the first observation. Either a single number or a vector
  of two numbers (the second of which is an integer), which specify a
  natural time unit and a (1-based) number of samples into the time
  unit. See the examples for the use of the second form.

- end:

  the time of the last observation, specified in the same way as
  `start`.

- frequency:

  the number of observations per unit of time.

- deltat:

  the fraction of the sampling period between successive observations;
  e.g., 1/12 for monthly data. Only one of `frequency` or `deltat`
  should be provided.

- ts.eps:

  time series comparison tolerance. Frequencies are considered equal if
  their absolute difference is less than `ts.eps`. It is also used to
  check consistency of `end - start`, `frequency`, and the length of the
  time-series.

- ...:

  Additional attributes.

- x:

  An object to be coerced / checked.

## Value

A koma_ts object.

A koma_ts object.

TRUE if the object is of class `koma_ts`, otherwise FALSE.

## Details

Mixed arithmetic between `koma_ts` and plain `ts` objects currently
follows base R's group generic dispatch and may emit an "Incompatible
methods" warning even when the resulting values are valid. Coercing both
operands to `koma_ts` avoids that warning.

## See also

[`ts`](https://rdrr.io/r/stats/ts.html)
