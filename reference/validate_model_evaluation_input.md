# Validate Model Evaluation Input

Validate Model Evaluation Input

## Usage

``` r
validate_model_evaluation_input(
  sys_eq,
  variables,
  horizon,
  ts_data,
  dates,
  evaluate_on_levels,
  call = rlang::caller_env()
)
```

## Arguments

- sys_eq:

  A `koma_seq` object
  ([system_of_equations](https://timothymerlin.github.io/koma/reference/system_of_equations.md))
  containing details about the system of equations used in the model.

- variables:

  A character vector of name(s) of the stochastic endogenous variable
  for which the forecast error(s) should be calculated. If NULL it is
  calculated for all variables.

- horizon:

  The forecast horizon in quarters up to which the RMSE should be
  calculated.

- ts_data:

  time series data set, must include data until end date of forecasting
  period.

- dates:

  Key-value list for date ranges in various model operations.

- evaluate_on_levels:

  Boolean, if TRUE RMSE is calculated on levels if FALSE on growth
  rates.

- call:

  The environment from which the error is called.

## Value

The variables to evaluate: `variables`, or all endogenous variables if
`variables` is `NULL`.
