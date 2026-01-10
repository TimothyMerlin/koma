# Identify Model Parameters from Character Matrices

This function calculates the gamma and beta matrices based on the given
character gamma and beta matrices, along with identity weights. It
checks the identification of all stochastic equations and verifies the
order and rank conditions for model identification.

## Usage

``` r
model_identification(
  character_gamma_matrix,
  character_beta_matrix,
  identity_weights,
  call = rlang::caller_env()
)
```

## Arguments

- character_gamma_matrix:

  A character matrix representing the gamma structure of the model.

- character_beta_matrix:

  A character matrix representing the beta structure of the model.

- identity_weights:

  A list of identity weights to help construct constant vectors.

- call:

  The environment from which the error is called. Defaults to
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html),
  and is used to provide context in case of an error.

## Value

NULL. This function is used for side effects, stopping execution if
model identification conditions are not met.
