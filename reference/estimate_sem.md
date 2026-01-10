# Estimate Parameters in a System of Equations

This function estimates parameters in a given system of equations using
either a single thread or parallel computing.

## Usage

``` r
estimate_sem(sys_eq, y_matrix, x_matrix, eq_jx = NULL)
```

## Arguments

- sys_eq:

  A `koma_seq` object
  ([system_of_equations](https://timothymerlin.github.io/koma/reference/system_of_equations.md))
  containing details about the system of equations used in the model.

- y_matrix:

  A \\(T \times n)\\ matrix \\Y\\, where \\T\\ is the number of
  observations and \\n\\ the number of equations, i.e. endogenous
  variables.

- x_matrix:

  A \\(T \times k)\\ matrix \\X\\ of observations on \\k\\ exogenous
  variables.

- eq_jx:

  A numeric vector indicating the indices of the endogenous equations
  \\j\\ to be estimated. If NULL, all endogenous equations are
  estimated. The vector should contain positive integers corresponding
  to the positions of the equations within the `endogenous_variables`
  list.

## Value

List of estimates for the endogenous variables.

## Details

This function provides the option for parallel computing through the
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
function. For more details, see the future package documentation:
https://cran.r-project.org/web/packages/future/future.pdf
