# Calculate Summary Statistics for Estimates

Computes summary statistics for a specified endogenous variable within a
system of equations. It returns the coefficients and standard errors of
the equation's variables.

## Usage

``` r
summary_statistics(
  endogenous_variables,
  estimates,
  sys_eq,
  central_tendency = "mean",
  ci_low = 5,
  ci_up = 95
)
```

## Arguments

- endogenous_variables:

  A character vector of name(s) of the stochastic endogenous variable to
  summarize.

- estimates:

  A list of estimates for each stochastic endogenous variable.

- sys_eq:

  A list containing endogenous and exogenous variables,
  character_gamma_matrix, and character_beta_matrix.

- central_tendency:

  A character string indicating which central tendency measure ("mean"
  or "median") to use for summary statistics. Default is "mean".

- ci_low:

  The lower bound for the confidence interval as a percentile. Default
  is 5.

- ci_up:

  The upper bound for the confidence interval as a percentile. Default
  is 95.

## Value

A list of summary statistics for each endogenous variable.
