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

We convert the raw `ts` series to `ets` objects and trim endogenous
series to the estimation window, leaving exogenous data available for
the forecast.

``` r
series_level <- c(
    "consumption",
    "investment",
    "exports",
    "imports",
    "domestic_demand",
    "gdp",
    "world_gdp",
    "exchange_rate"
)

ts_data <- lapply(series_level, function(x) {
    as_ets(small_open_economy[[x]],
        series_type = "level",
        method = "diff_log"
    )
})
names(ts_data) <- series_level

ts_data$interest_rate <- as_ets(
    small_open_economy$interest_rate,
    series_type = "rate",
    method = "none"
)

ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
        stats::window(ts_data[[x]], end = dates$estimation$end)
    })
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
#> Posterior mean (90% credible interval: [5.0%,  95.0%])
```

## Forecast and inspect

``` r
forecasts <- forecast(estimates, dates)
#> 
#> ── Forecast ────────────────────────────────────────────────────────────────────
#> ✔ Forecasting completed.
#> ✔ Forecasting completed.
print(forecasts)
#>         consumption  investment   exports   imports       gdp interest_rate
#> 2023 Q1   0.4053098  0.93760500 1.3944842 1.2688461 0.6992962      1.100896
#> 2023 Q2   0.4320724 -0.15417638 0.3433207 0.5094146 0.1858886      1.522694
#> 2023 Q3   0.4408521 -0.08781447 0.5726424 0.7634791 0.2186501      1.707523
#> 2023 Q4   0.4409708 -0.07201064 0.3998921 0.5492471 0.2269048      1.700577
#>         world_gdp exchange_rate
#> 2023 Q1 0.4827344     0.9217413
#> 2023 Q2 0.4083410    -1.3863355
#> 2023 Q3 0.4259099    -1.7869351
#> 2023 Q4 0.2905613    -0.7501833

rate(forecasts$mean$gdp)
#> rate, diff_log, c(189347.268533194, 2022.75)
#>           Qtr1      Qtr2      Qtr3      Qtr4
#> 2023 0.6992962 0.1858886 0.2186501 0.2269048
level(forecasts$mean$gdp)
#> level, diff_log
#>          Qtr1     Qtr2     Qtr3     Qtr4
#> 2022                            189347.3
#> 2023 190676.0 191030.8 191448.9 191883.8
```

``` r
if (requireNamespace("plotly", quietly = TRUE)) {
    plot(forecasts, variables = c("gdp", "consumption"))
}
```
