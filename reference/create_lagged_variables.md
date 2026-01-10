# Create Lagged Variables for Time Series Data

This function creates lagged variables for the given time series data,
considering the provided endogenous and exogenous variables along with
predetermined variables. The lagged variables are created based on the
pattern of the predetermined variables.

## Usage

``` r
create_lagged_variables(
  ts_data,
  endogenous_variables,
  exogenous_variables,
  predetermined_variables
)
```

## Arguments

- ts_data:

  A named list of time series objects.

- endogenous_variables:

  A character vector of names of endogenous variables.

- exogenous_variables:

  A character vector of names of exogenous variables.

- predetermined_variables:

  A character vector of names of predetermined variables, which contains
  lag information in the format "var.L(k)" where "var" is the variable
  name and "k" is the lag (e.g., "var.L(1)" for the first lag of the
  variable "var").

## Value

A named list of time series objects with the lagged variables added
according to the predetermined pattern.
