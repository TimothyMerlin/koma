# Prepare Draw Matrix and Indices

Internal helper that applies thinning, converts draw lists to a matrix,
and applies an optional matrix transformation.

## Usage

``` r
prepare_draw_matrix(draws, thin = 1L, mat_transform = NULL)
```

## Arguments

- draws:

  A list of MCMC draws.

- thin:

  Positive integer thinning interval.

- mat_transform:

  Optional function to transform the draw matrix.

## Value

A list with elements `mat` (numeric matrix) and `draw_idx` (integer
vector), or `NULL` when no draws are available.
