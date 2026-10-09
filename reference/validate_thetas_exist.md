# Verify every theta placeholder found in the gamma/beta matrices resolves back to a cell in one of those same matrices.

Under normal operation this is a tautology: `character_weights` is
extracted directly from `character_gamma_matrix`/`character_beta_matrix`
in the first place, so every value is guaranteed to be found again. This
exists as a defensive invariant check in case a future change to
[`construct_gamma_matrix()`](https://timothymerlin.github.io/koma/reference/construct_gamma_matrix.md)/[`construct_beta_matrix()`](https://timothymerlin.github.io/koma/reference/construct_beta_matrix.md)
ever desynchronizes the two – if it ever fires, it indicates an internal
bug rather than a problem with the user's equations, since
[`validate_completeness()`](https://timothymerlin.github.io/koma/reference/validate_completeness.md)
has already run by this point in the pipeline and ruled out undeclared
or mismatched variables.

## Usage

``` r
validate_thetas_exist(
  character_weights,
  character_gamma_matrix,
  character_beta_matrix
)
```

## Arguments

- character_weights:

  Character vector of theta placeholder strings (e.g.
  `"theta_gamma6_4"`), as extracted from the gamma/beta matrices.

- character_gamma_matrix, character_beta_matrix:

  The character matrices `character_weights` was extracted from.
