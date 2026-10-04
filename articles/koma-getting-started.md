# Estimating and Forecasting with the koma Package

``` r

library(koma)
```

## Overview

This vignette walks through a minimal end-to-end workflow: define a
small system, prepare data, estimate, and forecast. For full syntax
details, see the [equation
reference](https://timothymerlin.github.io/koma/articles/koma-equations.md),
and for time series handling see the [ets
vignette](https://timothymerlin.github.io/koma/articles/koma-extended-timeseries.md).

## Define a small system

We start with four stochastic equations and two identities that mirror a
small open economy. The interest rate, world GDP, and the exchange rate
are treated as exogenous in this example. To keep the setup minimal, the
GDP identity below uses fixed illustrative weights. For an example with
time-varying weights computed from nominal series, see the [Klein
vignette](https://timothymerlin.github.io/koma/articles/koma-klein.md).

``` r

equations <- "consumption ~ gdp + consumption.L(1) + interest_rate,
investment ~ gdp + investment.L(1) + interest_rate,
exports ~ world_gdp + exchange_rate + exports.L(1),
imports ~ gdp + exchange_rate + imports.L(1),
gdp == 0.55*consumption + 0.20*investment + 0.30*exports - 0.05*imports"

exogenous_variables <- c("interest_rate", "world_gdp", "exchange_rate")
```

## Build the system

``` r

sys_eq <- system_of_equations(
    equations = equations,
    exogenous_variables = exogenous_variables
)

print(sys_eq)
#> 
#> ── System of Equations ─────────────────────────────────────────────────────────
#> consumption ~  constant + gdp + consumption.L(1) + interest_rate
#>  investment ~  constant + gdp + investment.L(1) + interest_rate
#>     exports ~  constant + world_gdp + exchange_rate + exports.L(1)
#>     imports ~  constant + gdp + exchange_rate + imports.L(1)
#>         gdp == 0.55 * consumption + 0.20 * investment + 0.30 * exports-0.05 * imports
```

## Pick estimation and forecast ranges

We use the last year of the dataset as a short out-of-sample forecast
period. For this introductory example, the identity already contains
fixed numeric weights, so only estimation and forecast ranges are
needed.

``` r

dates <- list(
    estimation = list(start = c(1996, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 1), end = c(2023, 4))
)
```

## Prepare the data

We use the `small_open_economy` dataset, which is a list of `ts`
objects. We’ll keep only the variables that appear in the system.

``` r

data("small_open_economy")
series <- unique(c(sys_eq$endogenous_variables, sys_eq$exogenous_variables))
ts_data <- small_open_economy[series]
```

If you pass `ts` objects directly,
[`estimate()`](https://timothymerlin.github.io/koma/reference/estimate.md)
assumes they are already in rates (the form the model estimates on) and
converts them with `series_type = "rate"`, `method = "none"` (no
transformation applied), emitting a warning that lists the affected
series:

``` r

estimates <- estimate(ts_data, sys_eq, dates)
#> ! The following series are plain <ts> objects, not <koma_ts>: "consumption",
#>   "investment", "exports", "imports", "gdp", "interest_rate", "world_gdp",
#>   and "exchange_rate".
#> i They are assumed to already be in rates, the form the model estimates on,
#>   and are converted to <koma_ts> with `series_type = "rate"`, `method =
#>   "none"`. The values are used as-is; no rate/level transformation is
#>   applied.
#> i To convert a series from levels (e.g. a percentage or diff_log growth
#>   rate), wrap it first with `ets()` or `as_ets()`. See
#>   `vignette("koma-extended-timeseries")` for details.
```

Most of these series are actually in levels and need a diff_log
transform to become growth rates, and `interest_rate` is a rate that
needs no transform, so here we convert explicitly instead of relying on
the `rate`/`none` default:

``` r

ts_data <- lapply(ts_data, function(x) {
    as_ets(x, series_type = "level", method = "diff_log")
})
ts_data$interest_rate <- as_ets(
    ts_data$interest_rate,
    series_type = "rate",
    method = "none"
)
```

## Estimate the model

``` r

estimates <- estimate(
    ts_data,
    sys_eq,
    dates
)
#> 
#> ── Gibbs Sampler Settings ──────────────────────────────────────────────────────
#> ── System Wide Settings ──
#> • Number of draws (`ndraws`): 2000
#> • Burn-in ratio (`burnin_ratio`): 0.5
#> • Burn-in (`burnin`): 1000
#> • Store frequency (`nstore`): 1
#> • Number of saved draws (`nsave`): 1000
#> • Tau (`tau`): 1.1
#> 
#> 
#> ── Estimation ──────────────────────────────────────────────────────────────────
#> 
#> ── ⚠ MCMC Acceptance Probability Warnings ──────────────────────────────────────
#> • consumption: 60.7%
#> 
#> ℹ Some acceptance probabilities are outside the recommended range (20%-60%).
#> Consider revising the equations, tuning each equation's tau, or adjusting your priors.

print(estimates)
#> 
#> ── Estimates ───────────────────────────────────────────────────────────────────
#> consumption ~  0.35 - 0.02 * gdp  +  0.06 * consumption.L(1)  +  0.04 * interest_rate
#>  investment ~  - 0.29  +  1.99 * gdp - 0.03 * investment.L(1) - 0.14 * interest_rate
#>     exports ~  - 0.09  +  2.98 * world_gdp  +  0.28 * exchange_rate - 0.28 * exports.L(1)
#>     imports ~  0.02  +  2.2 * gdp - 0.19 * exchange_rate - 0.14 * imports.L(1)
#>         gdp == 0.55 * consumption  +  0.20 * investment  +  0.30 * exports - 0.05 * imports
summary(estimates)
#> 
#> ==============================================================================
#>                   consumption    investment     exports         imports       
#> ------------------------------------------------------------------------------
#> constant            0.35          -0.29          -0.09            0.02        
#>                   [ 0.27; 0.43]  [-0.64; 0.03]  [-0.64;  0.47]  [-0.42;  0.44]
#> consumption.L(1)    0.06                                                      
#>                   [-0.11; 0.24]                                               
#> interest_rate       0.04          -0.14                                       
#>                   [ 0.00; 0.08]  [-0.36; 0.08]                                
#> gdp                -0.02           1.99                           2.20        
#>                   [-0.10; 0.08]  [ 1.45; 2.53]                  [ 1.52;  2.92]
#> investment.L(1)                   -0.03                                       
#>                                  [-0.20; 0.14]                                
#> exports.L(1)                                     -0.28                        
#>                                                 [-0.43; -0.12]                
#> world_gdp                                         2.98                        
#>                                                 [ 2.13;  3.77]                
#> exchange_rate                                     0.28           -0.19        
#>                                                 [ 0.12;  0.45]  [-0.33; -0.06]
#> imports.L(1)                                                     -0.14        
#>                                                                 [-0.31;  0.03]
#> ==============================================================================
#> Posterior mean (90% credible interval: [5.0%, 95.0%])
#> Estimation period: 1996 Q1 - 2019 Q4
```

## Forecast and inspect

Before forecasting, truncate endogenous series so they end in the
quarter before the forecast start date.

``` r

estimates$ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
        stats::window(estimates$ts_data[[x]], end = c(2022, 4))
    })
```

``` r

forecasts <- forecast(estimates, dates)
#> 
#> ── Forecast ────────────────────────────────────────────────────────────────────
print(forecasts)
#> <koma_ts>
#> attributes:
#>   series_type: list[8]
#>   method: list[8]
#>   anker: list[8]
#> 
#> series:
#>         consumption investment exports imports    gdp interest_rate world_gdp
#> 2023 Q1      0.3994     1.1504  1.4131  1.6973 0.7888        1.1009    0.4827
#> 2023 Q2      0.4162     0.0896  0.3795  0.6738 0.3270        1.5227    0.4083
#> 2023 Q3      0.4270     0.3604  0.5249  1.1850 0.4051        1.7075    0.4259
#> 2023 Q4      0.4392     0.2191  0.3685  0.7204 0.3599        1.7006    0.2906
#>         exchange_rate
#> 2023 Q1        0.9217
#> 2023 Q2       -1.3863
#> 2023 Q3       -1.7869
#> 2023 Q4       -0.7502

rate(forecasts$mean$gdp)
#> <koma_ts>
#> attributes:
#>   series_type:  chr "rate"
#>   method:  chr "diff_log"
#>   anker:  num [1:2] 191669 2023
#> 
#> series:
#>           Qtr1      Qtr2      Qtr3      Qtr4
#> 2023 0.7888037 0.3270159 0.4051231 0.3599249
level(forecasts$mean$gdp)
#> <koma_ts>
#> attributes:
#>   series_type:  chr "level"
#>   method:  chr "diff_log"
#> 
#> series:
#>          Qtr1     Qtr2     Qtr3     Qtr4
#> 2022                            191668.9
#> 2023 193186.7 193819.5 194606.3 195308.0
```

You can also summarize forecast horizons with mean/median and quantiles:

``` r

summary(forecasts)
#> =========================================
#> consumption  Mean   Median  5%      95%  
#> -----------------------------------------
#> 2023 Q1      0.399   0.409  -0.029   0.77
#> 2023 Q2      0.416   0.419   0.012  0.844
#> 2023 Q3      0.427   0.421   0.032  0.837
#> 2023 Q4      0.439    0.44  -0.006  0.886
#> =========================================
#> 
#> ========================================
#> investment  Mean   Median  5%      95%  
#> ----------------------------------------
#> 2023 Q1      1.15   1.147  -3.572   5.61
#> 2023 Q2      0.09   0.184  -4.979  5.078
#> 2023 Q3      0.36   0.359  -4.727  5.509
#> 2023 Q4     0.219   0.236  -4.954  5.657
#> ========================================
#> 
#> =====================================
#> exports  Mean   Median  5%      95%  
#> -------------------------------------
#> 2023 Q1  1.413   1.453  -2.239  5.073
#> 2023 Q2   0.38   0.327  -3.216  4.085
#> 2023 Q3  0.525   0.578  -3.728  4.495
#> 2023 Q4  0.369   0.363  -3.389  4.178
#> =====================================
#> 
#> =====================================
#> imports  Mean   Median  5%      95%  
#> -------------------------------------
#> 2023 Q1  1.697   1.688  -2.416  6.267
#> 2023 Q2  0.674   0.715  -4.183  5.304
#> 2023 Q3  1.185   1.212  -3.699  6.097
#> 2023 Q4   0.72   0.724  -4.007  5.457
#> =====================================
#> 
#> =====================================
#> gdp      Mean   Median  5%      95%  
#> -------------------------------------
#> 2023 Q1  0.789   0.796  -0.837   2.48
#> 2023 Q2  0.327   0.326  -1.441  2.145
#> 2023 Q3  0.405   0.405  -1.504  2.291
#> 2023 Q4   0.36   0.403  -1.424  2.185
#> =====================================
#> 
#> ==========================================
#> interest_rate  Mean   Median  5%     95%  
#> ------------------------------------------
#> 2023 Q1        1.101   1.101  1.101  1.101
#> 2023 Q2        1.523   1.523  1.523  1.523
#> 2023 Q3        1.708   1.708  1.708  1.708
#> 2023 Q4        1.701   1.701  1.701  1.701
#> ==========================================
#> 
#> ======================================
#> world_gdp  Mean   Median  5%     95%  
#> --------------------------------------
#> 2023 Q1    0.483   0.483  0.483  0.483
#> 2023 Q2    0.408   0.408  0.408  0.408
#> 2023 Q3    0.426   0.426  0.426  0.426
#> 2023 Q4    0.291   0.291  0.291  0.291
#> ======================================
#> 
#> =============================================
#> exchange_rate  Mean    Median  5%      95%   
#> ---------------------------------------------
#> 2023 Q1         0.922   0.922   0.922   0.922
#> 2023 Q2        -1.386  -1.386  -1.386  -1.386
#> 2023 Q3        -1.787  -1.787  -1.787  -1.787
#> 2023 Q4         -0.75   -0.75   -0.75   -0.75
#> =============================================
#> 
#> Mean, Median, Quantiles
summary(forecasts, variables = "gdp", horizon = 2)
#> =====================================
#> gdp      Mean   Median  5%      95%  
#> -------------------------------------
#> 2023 Q1  0.789   0.796  -0.837   2.48
#> 2023 Q2  0.327   0.326  -1.441  2.145
#> =====================================
#> 
#> Mean, Median, Quantiles
```

``` r

if (requireNamespace("plotly", quietly = TRUE)) {
    plot(forecasts, variables = c("gdp", "consumption"))
}
```
