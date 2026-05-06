# Estimating and Forecasting with the koma Package

``` r

library(koma)
```

## Overview

This vignette walks through a minimal end-to-end workflow: define a
small system, prepare data, estimate, and forecast. For full syntax
details, see the [equation
reference](https://timothymerlin.github.io/koma/equations.md), and for
time series handling see the [ets
vignette](https://timothymerlin.github.io/koma/koma-extended-timeseries.md).

## Define a small system

We start with four stochastic equations and two identities that mirror a
small open economy. The interest rate, world GDP, and the exchange rate
are treated as exogenous in this example. To keep the setup minimal, the
GDP identity below uses fixed illustrative weights. For an example with
time-varying weights computed from nominal series, see the [Klein
vignette](https://timothymerlin.github.io/koma/koma-klein.md).

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
will prompt you for default conversion settings and optional exceptions.
For example, we override the defaults to use level/diff_log and keep
`interest_rate` as a rate series:

``` r

estimates <- estimate(ts_data, sys_eq, dates)
#> Some of the time series in `ts_data` are not `ets`.
#> They will be automatically converted with `as_ets` using the defaults:
#> Default settings
#> series_type = level
#> method = percentage
#> Are these correct? (y/n): n
#> Enter series_type: level
#> Enter method: diff_log
#> Specify exception to default settings? (y/n): y
#> Enter series name: interest_rate
#> Enter series_type for interest_rate (default level): rate
#> Enter method for interest_rate (default diff_log): none
#> Specify exception to default settings? (y/n): n
```

In this vignette we convert explicitly to keep the example
non-interactive:

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
#>  investment ~  - 0.29  +  1.99 * gdp - 0.02 * investment.L(1) - 0.14 * interest_rate
#>     exports ~  - 0.09  +  2.98 * world_gdp  +  0.28 * exchange_rate - 0.28 * exports.L(1)
#>     imports ~  0.01  +  2.2 * gdp - 0.19 * exchange_rate - 0.14 * imports.L(1)
#>         gdp == 0.55 * consumption  +  0.20 * investment  +  0.30 * exports - 0.05 * imports
summary(estimates)
#> 
#> ==============================================================================
#>                   consumption    investment     exports         imports       
#> ------------------------------------------------------------------------------
#> constant            0.35          -0.29          -0.09            0.01        
#>                   [ 0.26; 0.43]  [-0.63; 0.04]  [-0.64;  0.47]  [-0.43;  0.45]
#> consumption.L(1)    0.06                                                      
#>                   [-0.11; 0.23]                                               
#> interest_rate       0.04          -0.14                                       
#>                   [ 0.00; 0.08]  [-0.35; 0.08]                                
#> gdp                -0.02           1.99                           2.20        
#>                   [-0.10; 0.08]  [ 1.45; 2.53]                  [ 1.52;  2.92]
#> investment.L(1)                   -0.02                                       
#>                                  [-0.21; 0.15]                                
#> exports.L(1)                                     -0.28                        
#>                                                 [-0.43; -0.12]                
#> world_gdp                                         2.98                        
#>                                                 [ 2.13;  3.77]                
#> exchange_rate                                     0.28           -0.19        
#>                                                 [ 0.12;  0.45]  [-0.32; -0.06]
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
#> 2023 Q1      0.3983     1.1411  1.4131  1.6923 0.7866        1.1009    0.4827
#> 2023 Q2      0.4141     0.0788  0.3795  0.6664 0.3241        1.5227    0.4083
#> 2023 Q3      0.4250     0.3424  0.5249  1.1760 0.4009        1.7075    0.4259
#> 2023 Q4      0.4376     0.1941  0.3685  0.7114 0.3545        1.7006    0.2906
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
#> 2023 0.7865677 0.3240562 0.4008722 0.3544575
level(forecasts$mean$gdp)
#> <koma_ts>
#> attributes:
#>   series_type:  chr "level"
#>   method:  chr "diff_log"
#> 
#> series:
#>          Qtr1     Qtr2     Qtr3     Qtr4
#> 2022                            191668.9
#> 2023 193182.4 193809.4 194587.9 195278.9
```

You can also summarize forecast horizons with mean/median and quantiles:

``` r

summary(forecasts)
#> =========================================
#> consumption  Mean   Median  5%      95%  
#> -----------------------------------------
#> 2023 Q1      0.398   0.409  -0.036  0.786
#> 2023 Q2      0.414    0.41   0.008  0.841
#> 2023 Q3      0.425   0.429   0.027  0.839
#> 2023 Q4      0.438   0.439  -0.008  0.874
#> =========================================
#> 
#> ========================================
#> investment  Mean   Median  5%      95%  
#> ----------------------------------------
#> 2023 Q1     1.141   1.098  -3.367  5.648
#> 2023 Q2     0.079   0.169  -5.048  5.026
#> 2023 Q3     0.342   0.341  -4.778  5.396
#> 2023 Q4     0.194   0.238  -4.954  5.389
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
#> 2023 Q1  1.692   1.651  -2.404  6.283
#> 2023 Q2  0.666   0.693  -4.141   5.25
#> 2023 Q3  1.176   1.242  -3.758  6.184
#> 2023 Q4  0.711   0.719  -4.063  5.269
#> =====================================
#> 
#> =====================================
#> gdp      Mean   Median  5%      95%  
#> -------------------------------------
#> 2023 Q1  0.787   0.795   -0.84  2.486
#> 2023 Q2  0.324   0.326  -1.421  2.146
#> 2023 Q3  0.401   0.404  -1.539  2.283
#> 2023 Q4  0.354   0.408  -1.431  2.199
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
#> 2023 Q1  0.787   0.795   -0.84  2.486
#> 2023 Q2  0.324   0.326  -1.421  2.146
#> =====================================
#> 
#> Mean, Median, Quantiles
```

``` r

if (requireNamespace("plotly", quietly = TRUE)) {
    plot(forecasts, variables = c("gdp", "consumption"))
}
```
