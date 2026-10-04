# Highest Density Intervals from Draws

Computes highest density intervals (HDIs) for a numeric sample. The HDI
is defined as the shortest interval containing a target probability
mass.

## Usage

``` r
hdi(x, ...)

# Default S3 method
hdi(x, probs = c(0.5, 0.99), ...)
```

## Arguments

- x:

  A numeric vector of draws.

- ...:

  Unused.

- probs:

  Numeric vector of target mass levels. Values in \\(0, 1\]\\ or \\\[0,
  100\]\\ are accepted. Default is `c(0.5, 0.99)`.

## Value

A list with class `"koma_hdi"` containing:

- intervals:

  Named list of matrices with columns `lower` and `upper`, one matrix
  per `probs` level.

- mode:

  Sample median of the draws (used as a center reference).

- cutoff:

  Named numeric vector of `NA` values (not defined for HDI).

- mass:

  Named numeric vector of achieved mass for each level.

- probs:

  Numeric vector of target masses in (0, 1\].

## Examples

``` r
x <- rnorm(1000)
hdi(x, probs = c(0.5, 0.9))
#> ==========
#> level_50
#> --------
#> Median: 0.008
#> Intervals: [-0.626; 0.671]
#> 
#> level_90
#> --------
#> Median: 0.008
#> Intervals: [-1.596; 1.652]
#> 
#> ==========
#> Median, [HDI] 

data("simulated_sem")
set.seed(11)

fit <- estimate(
  ts_data = simulated_sem$ts_data,
  sys_eq = simulated_sem$sys_eq,
  dates = simulated_sem$dates,
  options = list(gibbs = list(ndraws = 10))
)
#> 
#> ── Gibbs Sampler Settings ──────────────────────────────────────────────────────
#> 
#> ── System Wide Settings ──
#>   • Number of draws (`ndraws`): 10
#>   • Burn-in ratio (`burnin_ratio`): 0.5
#>   • Burn-in (`burnin`): 5
#>   • Store frequency (`nstore`): 1
#>   • Number of saved draws (`nsave`): 5
#>   • Tau (`tau`): 1.1
#> 
#> 
#> ── Estimation ──────────────────────────────────────────────────────────────────
#> 
#> ── ⚠ MCMC Acceptance Probability Warnings ──────────────────────────────────────
#> • consumption: 80.0%
#> 
#> ℹ Some acceptance probabilities are outside the recommended range (20%-60%).
#> Consider revising the equations, tuning each equation's tau, or adjusting your priors.
#> 
hdi_fit <- hdi(fit, probs = c(0.5, 0.9))
names(hdi_fit$intervals)
#> [1] "consumption"     "investment"      "current_account" "manufacturing"  
#> [5] "service"        
```
