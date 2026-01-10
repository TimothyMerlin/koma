# Calculate Out-of-Sample RMSE for a Specified Horizon

Computes the Root Mean Square Error (RMSE) of a model for a given
prediction horizon, incrementing the in-sample data by a quarter for
each calculation until the specified horizon equals the end date in the
forecast period.

## Usage

``` r
model_evaluation(
  sys_eq,
  variables,
  horizon,
  ts_data,
  dates,
  ...,
  evaluate_on_levels = TRUE,
  options = NULL,
  point_forecast = NULL,
  restrictions = NULL
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

- ...:

  Additional parameters.

- evaluate_on_levels:

  Boolean, if TRUE RMSE is calculated on levels if FALSE on growth
  rates.

- options:

  Optional settings for modifying the Gibbs sampler specifications for
  all equations. See [Gibbs Sampler
  Specifications](https://timothymerlin.github.io/koma/reference/get_default_gibbs_spec.md).

- point_forecast:

  A list that contains the following elements:

  - `active`: Determines the type of forecast generated. If TRUE, a
    point forecast is created. If FALSE, a density forecast is returned.
    Default is TRUE.

  - `central_tendency`: A character string indicating which central
    tendency measure ("mean" or "median") to use for summary statistics.
    Default is "mean".

- restrictions:

  List of model constraints. Default is empty.

## Value

DataFrame containing the RMSE of the selected Variables up to the
desired horizon.

## Details

The function initiates the RMSE calculation from `dates$forecast$start`
and continues until `dates$forecast$start + horizon` equals
`dates$forecast$end`. In each iteration, a quarter is added to both the
in-sample data and to `dates$forecast$start`.
