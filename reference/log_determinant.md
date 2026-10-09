# Logarithm of a determinant

Computes \\\log(\det(x))\\ without forming the determinant itself, which
overflows or underflows for matrices on a very large or small scale.

## Usage

``` r
log_determinant(x)
```

## Arguments

- x:

  A square numeric matrix.

## Value

The logarithm of the determinant of `x`, `-Inf` if `x` is singular and
`NaN` if the determinant is negative.
