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
#> • imports: 60.2%
#> 
#> ℹ Some acceptance probabilities are outside the recommended range (20%-60%).
#> Consider revising the equations, tuning each equation's tau, or adjusting your priors.

print(estimates)
#> 
#> ── Estimates ───────────────────────────────────────────────────────────────────
#> consumption ~  0.35 - 0.03 * gdp  +  0.07 * consumption.L(1)  +  0.04 * interest_rate
#>  investment ~  - 0.28  +  1.96 * gdp - 0.02 * investment.L(1) - 0.14 * interest_rate
#>     exports ~  - 0.11  +  3.01 * world_gdp  +  0.28 * exchange_rate - 0.28 * exports.L(1)
#>     imports ~  0.02  +  2.18 * gdp - 0.19 * exchange_rate - 0.14 * imports.L(1)
#>         gdp == 0.55 * consumption  +  0.20 * investment  +  0.30 * exports - 0.05 * imports
summary(estimates)
#> 
#> ==============================================================================
#>                   consumption    investment     exports         imports       
#> ------------------------------------------------------------------------------
#> constant            0.35          -0.28          -0.11            0.02        
#>                   [ 0.27; 0.44]  [-0.60; 0.03]  [-0.67;  0.46]  [-0.42;  0.42]
#> consumption.L(1)    0.07                                                      
#>                   [-0.12; 0.25]                                               
#> interest_rate       0.04          -0.14                                       
#>                   [ 0.00; 0.08]  [-0.36; 0.06]                                
#> gdp                -0.03           1.96                           2.18        
#>                   [-0.12; 0.06]  [ 1.45; 2.47]                  [ 1.51;  2.86]
#> investment.L(1)                   -0.02                                       
#>                                  [-0.19; 0.15]                                
#> exports.L(1)                                     -0.28                        
#>                                                 [-0.45; -0.12]                
#> world_gdp                                         3.01                        
#>                                                 [ 2.16;  3.87]                
#> exchange_rate                                     0.28           -0.19        
#>                                                 [ 0.10;  0.46]  [-0.32; -0.06]
#> imports.L(1)                                                     -0.14        
#>                                                                 [-0.32;  0.03]
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
#> 2023 Q1      0.3843     0.8860  1.1943  1.4343 0.6751        1.1009    0.4827
#> 2023 Q2      0.4286     0.1396  0.4464  0.8656 0.3543        1.5227    0.4083
#> 2023 Q3      0.4511     0.2475  0.6343  1.1059 0.4326        1.7075    0.4259
#> 2023 Q4      0.4448     0.2232  0.4136  0.8681 0.3699        1.7006    0.2906
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
#> 2023 0.6751463 0.3542830 0.4325583 0.3699307
level(forecasts$mean$gdp)
#> <koma_ts>
#> attributes:
#>   series_type:  chr "level"
#>   method:  chr "diff_log"
#> 
#> series:
#>          Qtr1     Qtr2     Qtr3     Qtr4
#> 2022                            191668.9
#> 2023 192967.3 193652.1 194491.6 195212.4
```

You can also summarize forecast horizons with mean/median and quantiles:

``` r

summary(forecasts)
#> =========================================
#> consumption  Mean   Median  5%      95%  
#> -----------------------------------------
#> 2023 Q1      0.384   0.381  -0.005  0.788
#> 2023 Q2      0.429   0.428   0.019  0.845
#> 2023 Q3      0.451   0.453   0.065  0.883
#> 2023 Q4      0.445   0.445   0.027  0.862
#> =========================================
#> 
#> ========================================
#> investment  Mean   Median  5%      95%  
#> ----------------------------------------
#> 2023 Q1     0.886   0.887  -4.024  5.511
#> 2023 Q2      0.14   0.169  -5.086  5.289
#> 2023 Q3     0.247   0.243  -5.043  5.212
#> 2023 Q4     0.223   0.226  -4.826  5.118
#> ========================================
#> 
#> =====================================
#> exports  Mean   Median  5%      95%  
#> -------------------------------------
#> 2023 Q1  1.194   1.276   -2.74  5.005
#> 2023 Q2  0.446   0.384  -3.682  4.423
#> 2023 Q3  0.634   0.693  -3.142  4.533
#> 2023 Q4  0.414   0.459  -3.518   4.44
#> =====================================
#> 
#> =====================================
#> imports  Mean   Median  5%      95%  
#> -------------------------------------
#> 2023 Q1  1.434   1.491  -3.094   5.89
#> 2023 Q2  0.866   0.836  -3.725   5.75
#> 2023 Q3  1.106   1.143  -3.786   5.89
#> 2023 Q4  0.868   0.756  -3.777  5.955
#> =====================================
#> 
#> =====================================
#> gdp      Mean   Median  5%      95%  
#> -------------------------------------
#> 2023 Q1  0.675   0.688  -1.082  2.356
#> 2023 Q2  0.354   0.353    -1.5   2.21
#> 2023 Q3  0.433    0.46  -1.432  2.125
#> 2023 Q4   0.37   0.378  -1.434  2.146
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
#> 2023 Q1  0.675   0.688  -1.082  2.356
#> 2023 Q2  0.354   0.353    -1.5   2.21
#> =====================================
#> 
#> Mean, Median, Quantiles
```

``` r

if (requireNamespace("plotly", quietly = TRUE)) {
    plot(forecasts, variables = c("gdp", "consumption"))
}
```
