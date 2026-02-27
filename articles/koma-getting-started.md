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
are treated as exogenous in this example.

``` r
equations <- "consumption ~ gdp + consumption.L(1) + interest_rate,
investment ~ gdp + investment.L(1) + interest_rate,
exports ~ world_gdp + exchange_rate + exports.L(1),
imports ~ gdp + exchange_rate + imports.L(1),
gdp == (consumption/gdp)*consumption + (investment/gdp)*investment + (exports/gdp)*exports - (imports/gdp)*imports"

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
#>         gdp == (consumption/gdp) * consumption + (investment/gdp) * investment + (exports/gdp) * exports-(imports/gdp) * imports
```

## Pick estimation and forecast ranges

We use the last year of the dataset as a short out-of-sample forecast
period. The identity weights are computed from level shares over the
estimation window.

``` r
dates <- list(
    estimation = list(start = c(1996, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 1), end = c(2023, 4)),
    dynamic_weights = list(start = c(1996, 1), end = c(2019, 4))
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
#>         gdp == (consumption/gdp) * consumption  +  (investment/gdp) * investment  +  (exports/gdp) * exports - (imports/gdp) * imports
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
#>         consumption investment exports imports    gdp interest_rate world_gdp
#> 2023 Q1      0.3989     0.8350  1.4131  1.3520 0.6384        1.1009    0.4827
#> 2023 Q2      0.4152    -0.1407  0.3795  0.4908 0.2083        1.5227    0.4083
#> 2023 Q3      0.4284    -0.0777  0.5249  0.7429 0.1960        1.7075    0.4259
#> 2023 Q4      0.4387    -0.0304  0.3685  0.5198 0.2329        1.7006    0.2906
#>         exchange_rate
#> 2023 Q1        0.9217
#> 2023 Q2       -1.3863
#> 2023 Q3       -1.7869
#> 2023 Q4       -0.7502

rate(forecasts$mean$gdp)
#> rate, diff_log, c(191668.860623222, 2022.75)
#>           Qtr1      Qtr2      Qtr3      Qtr4
#> 2023 0.6384174 0.2083174 0.1959766 0.2328886
level(forecasts$mean$gdp)
#> level, diff_log
#>          Qtr1     Qtr2     Qtr3     Qtr4
#> 2022                            191668.9
#> 2023 192896.4 193298.7 193677.9 194129.4
```

You can also summarize forecast horizons with mean/median and quantiles:

``` r
summary(forecasts)
#> =========================================
#> consumption  Mean   Median  5%      95%  
#> -----------------------------------------
#> 2023 Q1      0.399   0.409  -0.034  0.789
#> 2023 Q2      0.415   0.414   0.003  0.843
#> 2023 Q3      0.428   0.434   0.037  0.843
#> 2023 Q4      0.439   0.437  -0.004  0.881
#> =========================================
#> 
#> =========================================
#> investment  Mean    Median  5%      95%  
#> -----------------------------------------
#> 2023 Q1      0.835   0.795   -3.62  5.269
#> 2023 Q2     -0.141  -0.129  -5.131  4.645
#> 2023 Q3     -0.078  -0.087  -5.036  4.598
#> 2023 Q4      -0.03   0.009  -5.005  4.899
#> =========================================
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
#> 2023 Q1  1.352   1.392  -1.803  4.605
#> 2023 Q2  0.491   0.504  -2.952  3.907
#> 2023 Q3  0.743   0.806  -3.049  4.298
#> 2023 Q4   0.52   0.489  -2.811  4.053
#> =====================================
#> 
#> =====================================
#> gdp      Mean   Median  5%      95%  
#> -------------------------------------
#> 2023 Q1  0.638   0.622   -0.95   2.33
#> 2023 Q2  0.208   0.189  -1.575  2.043
#> 2023 Q3  0.196   0.198  -1.526  1.992
#> 2023 Q4  0.233   0.262  -1.601  2.145
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
#> 2023 Q1  0.638   0.622   -0.95   2.33
#> 2023 Q2  0.208   0.189  -1.575  2.043
#> =====================================
#> 
#> Mean, Median, Quantiles
```

``` r
if (requireNamespace("plotly", quietly = TRUE)) {
    plot(forecasts, variables = c("gdp", "consumption"))
}
```
