# Error Correction in a Small Open Economy Model

``` r
library(koma)
```

## Overview

When a set of variables is cointegrated—meaning they share a common
long-run stochastic trend—short-run dynamics can be represented using an
error-correction model (ECM). An ECM decomposes movements into short-run
adjustments and a long-run equilibrium relationship, with deviations
from the equilibrium feeding back into short-run growth through an
error-correction term. For background, see
<https://en.wikipedia.org/wiki/Error_correction_model>.

In this vignette, we specify a small structural system that includes an
ECM for exports. We then construct the required level variables, prepare
the data, and estimate the model.

### Export ECM Intuition

We omit contemporaneous exchange-rate growth in the short-run dynamics,
assuming exports do not respond to very short-run exchange-rate
movements in this vignette.

The export equation is motivated by a standard export-demand
relationship. In the long run, export volumes depend on foreign demand,
proxied by world GDP, and on relative prices or international
competitiveness, proxied by the exchange rate. If exports, world GDP,
and the exchange rate are cointegrated, their long-run relationship can
be written in levels.

Short-run export growth is then modeled as a function of current changes
in foreign demand, together with an error-correction term that captures
the deviation from the long-run equilibrium in the previous period. The
coefficient on this term measures the speed at which exports adjust back
toward the long-run relationship following a shock.

If exports $x_{t}$, world GDP $y_{t}$, and the exchange rate $q_{t}$ are
cointegrated, the long-run equilibrium relationship can be written as
$$x_{t} = \alpha + \beta_{y}y_{t} + \beta_{q}q_{t} + u_{t}.$$

The deviation from this equilibrium in the previous period defines the
error-correction term,
$${ECT}_{t - 1} = x_{t - 1} - \alpha - \beta_{y}y_{t - 1} - \beta_{q}q_{t - 1}.$$

A minimal one-lag ECM for export growth is then
$$\Delta x_{t} = \gamma + \phi\,\Delta x_{t - 1} + \theta\,\Delta y_{t} + \lambda\,{ECT}_{t - 1} + \varepsilon_{t}.$$

Substituting the error-correction term into the ECM and expanding yields
$$\begin{aligned}
{\Delta x_{t}} & {= \gamma + \phi\,\Delta x_{t - 1} + \theta\,\Delta y_{t}} \\
 & {\quad + \lambda x_{t - 1} - \lambda\alpha - \lambda\beta_{y}y_{t - 1} - \lambda\beta_{q}q_{t - 1} + \varepsilon_{t}.}
\end{aligned}$$

This is the form estimated in the model, where lagged levels enter
directly. The coefficient $\lambda$ is the speed of adjustment toward
the long-run equilibrium.

The long-run coefficients are recovered as
$$\beta_{y} = - \frac{\text{coef}\left( y_{t - 1} \right)}{\lambda},\qquad\beta_{q} = - \frac{\text{coef}\left( q_{t - 1} \right)}{\lambda}.$$

## Define the SEM

We now translate the error-correction representation into a structural
system that can be estimated directly. Rather than including the
error-correction term explicitly, the model is written with lagged level
variables. This formulation is algebraically equivalent to the ECM
expansion above and allows the speed-of-adjustment and long-run
relationships to be recovered from the estimated coefficients.

``` r
equations <- "consumption ~ gdp + consumption.L(1),
investment ~ investment.L(1),
exports ~ world_gdp + exports.L(1) + exports_level.L(1) + world_gdp_level.L(1) + exchange_rate_level.L(1),
imports ~ exports + consumption + investment + imports.L(1),
gdp == 0.6*consumption + 0.6*domestic_demand + 0.5*exports - 0.4*imports,
domestic_demand == 0.6*consumption + 0.4*investment,
exports_level == 1*exports + 1*exports_level.L(1),
world_gdp_level == 1*world_gdp + 1*world_gdp_level.L(1),
exchange_rate_level == 1*exchange_rate + 1*exchange_rate_level.L(1)"

exogenous_variables <- c("world_gdp", "exchange_rate")
```

## Create the SEM

``` r
sys_eq <- system_of_equations(
    equations = equations,
    exogenous_variables = exogenous_variables
)

print(sys_eq)
#> 
#> ── System of Equations ─────────────────────────────────────────────────────────
#>         consumption ~  constant + gdp + consumption.L(1)
#>          investment ~  constant + investment.L(1)
#>             exports ~  constant + world_gdp + exports.L(1) + exports_level.L(1) + world_gdp_level.L(1) + exchange_rate_level.L(1)
#>             imports ~  constant + exports + consumption + investment + imports.L(1)
#>                 gdp == 0.6 * consumption + 0.6 * domestic_demand + 0.5 * exports-0.4 * imports
#>     domestic_demand == 0.6 * consumption + 0.4 * investment
#>       exports_level == 1 * exports + 1 * exports_level.L(1)
#>     world_gdp_level == 1 * world_gdp + 1 * world_gdp_level.L(1)
#> exchange_rate_level == 1 * exchange_rate + 1 * exchange_rate_level.L(1)
```

## Preparing the Data

`koma` is estimated in growth rates, so error correction terms need to
enter in levels. We create level terms as `series_type = "level"` with
`method = "none"` so
[`rate()`](https://timothymerlin.github.io/koma/reference/rate.md)
leaves them unchanged during estimation.

We use `small_open_economy`, which provides the needed series in levels.

``` r
?small_open_economy
```

Convert the base series to `ets` objects first, then add the level terms
for error correction. Adding the level terms after the base conversion
avoids overwriting them when the list is rebuilt.

We take logs to make the long-run relationship linear in levels and to
match the `diff_log` transformation used for growth rates. Multiplying
by 100 keeps the level terms on the same scale as `diff_log`, which also
returns percent changes.

``` r
ts_data <- small_open_economy[c(
    "consumption", "investment", "exports", "imports",
    "gdp", "domestic_demand", "world_gdp", "exchange_rate"
)]

series <- names(ts_data)
ts_data <- lapply(series, function(x) {
    as_ets(ts_data[[x]],
        series_type = "level", method = "diff_log"
    )
})
names(ts_data) <- series

ts_data$exports_level <- as_ets(log(ts_data$exports) * 100,
    series_type = "level", method = "none"
)
ts_data$world_gdp_level <- as_ets(log(ts_data$world_gdp) * 100,
    series_type = "level", method = "none"
)
ts_data$exchange_rate_level <- as_ets(log(ts_data$exchange_rate) * 100,
    series_type = "level", method = "none"
)
```

## Estimation and Forecast Dates

With the data prepared, define the estimation and forecast windows.

``` r
dates <- list(
    estimation = list(start = c(1996, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 1), end = c(2024, 4))
)
```

## Estimating the Model

``` r
estimates <- estimate(
    sys_eq,
    ts_data = ts_data,
    dates = dates
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
print(estimates)
#> 
#> ── Estimates ───────────────────────────────────────────────────────────────────
#>         consumption ~  0.35 - 0.01 * gdp  +  0.1 * consumption.L(1)
#>          investment ~  0.45  +  0.21 * investment.L(1)
#>             exports ~  - 1083.18  +  2.87 * world_gdp - 0.19 * exports.L(1) - 0.24 * exports_level.L(1)  +  0.55 * world_gdp_level.L(1)  +  0.06 * exchange_rate_level.L(1)
#>             imports ~  - 0.01 - 0.01 * exports  +  1.51 * consumption  +  0.98 * investment - 0.13 * imports.L(1)
#>                 gdp == 0.6 * consumption  +  0.6 * domestic_demand  +  0.5 * exports - 0.4 * imports
#>     domestic_demand == 0.6 * consumption  +  0.4 * investment
#>       exports_level == 1 * exports  +  1 * exports_level.L(1)
#>     world_gdp_level == 1 * world_gdp  +  1 * world_gdp_level.L(1)
#> exchange_rate_level == 1 * exchange_rate  +  1 * exchange_rate_level.L(1)
```

``` r
summary(estimates, variables = "exports")
#> 
#> =============================================
#>                           exports            
#> ---------------------------------------------
#> constant                   -1083.18          
#>                           [-1643.61; -529.64]
#> exports.L(1)                  -0.19          
#>                           [   -0.37;   -0.01]
#> exports_level.L(1)            -0.24          
#>                           [   -0.37;   -0.12]
#> world_gdp_level.L(1)           0.55          
#>                           [    0.27;    0.84]
#> exchange_rate_level.L(1)       0.06          
#>                           [    0.01;    0.11]
#> world_gdp                      2.87          
#>                           [    1.93;    3.76]
#> =============================================
#> Posterior mean (90% credible interval: [5.0%, 95.0%])
#> Estimation period: 1996 Q1 - 2019 Q4
```

``` r
ecm_stats <- summary(estimates, variables = "exports")
ecm_coef <- ecm_stats[["exports"]]@coef
adjustment_speed <- ecm_coef["exports_level.L(1)"]
long_run_world_gdp <- -ecm_coef["world_gdp_level.L(1)"] / adjustment_speed
long_run_exchange_rate <- -ecm_coef["exchange_rate_level.L(1)"] / adjustment_speed

sprintf(
    "Adjustment speed: %.3f, Long-run world GDP: %.3f, Exchange rate: %.3f",
    adjustment_speed,
    long_run_world_gdp,
    long_run_exchange_rate
)
#> [1] "Adjustment speed: -0.242, Long-run world GDP: 2.285, Exchange rate: 0.247"
```

The adjustment speed is -0.24, which is negative and implies about 24%
of the gap closes each period. The long-run elasticities imply that a 1%
rise in world GDP is associated with roughly 2.28% higher exports in the
long run, while a 1% increase in the exchange-rate index implies 0.25%
higher exports if higher values indicate depreciation (here the exchange
rate is CHF/EUR, so higher values mean depreciation; flip the sign if
the index is defined the other way).

## Forecasting

``` r
estimates$ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
        stats::window(estimates$ts_data[[x]], end = c(2022, 4))
    })

forecasts <- forecast(
    estimates,
    dates = dates
)
#> 
#> ── Forecast ────────────────────────────────────────────────────────────────────
print(forecasts)
#>         consumption investment exports imports    gdp domestic_demand
#> 2023 Q1      0.3747     0.3695  0.8578  1.0266 0.4666          0.3726
#> 2023 Q2      0.3810     0.4639  0.7363  0.7798 0.5333          0.4141
#> 2023 Q3      0.3714     0.5915  0.6504  0.9925 0.4267          0.4594
#> 2023 Q4      0.3995     0.5453  0.2972  0.9804 0.2708          0.4578
#> 2024 Q1      0.3907     0.5046  0.8627  0.8809 0.5751          0.4362
#> 2024 Q2      0.3899     0.4520  0.6635  0.8540 0.4729          0.4147
#> 2024 Q3      0.3862     0.5442  1.3581  0.9590 0.7968          0.4494
#> 2024 Q4      0.4007     0.5277  0.5356  1.1307 0.3268          0.4515
#>         exports_level world_gdp_level exchange_rate_level world_gdp
#> 2023 Q1      1168.954        2474.282             -0.7646    0.4827
#> 2023 Q2      1169.690        2474.690             -2.1510    0.4083
#> 2023 Q3      1170.340        2475.116             -3.9379    0.4259
#> 2023 Q4      1170.637        2475.407             -4.6881    0.2906
#> 2024 Q1      1171.500        2475.865             -5.2093    0.4586
#> 2024 Q2      1172.164        2476.261             -2.6797    0.3955
#> 2024 Q3      1173.522        2476.770             -4.9959    0.5096
#> 2024 Q4      1174.057        2477.210             -6.6168    0.4393
#>         exchange_rate
#> 2023 Q1        0.9217
#> 2023 Q2       -1.3863
#> 2023 Q3       -1.7869
#> 2023 Q4       -0.7502
#> 2024 Q1       -0.5212
#> 2024 Q2        2.5295
#> 2024 Q3       -2.3162
#> 2024 Q4       -1.6209
```

``` r
rate(forecasts$mean$exports)
#> rate, diff_log, c(118297.572909318, 2022.75)
#>           Qtr1      Qtr2      Qtr3      Qtr4
#> 2023 0.8577540 0.7362662 0.6503629 0.2972050
#> 2024 0.8627210 0.6635003 1.3581153 0.5355779
level(forecasts$mean$exports)
#> level, diff_log
#>          Qtr1     Qtr2     Qtr3     Qtr4
#> 2022                            118297.6
#> 2023 119316.6 120198.4 120982.6 121342.7
#> 2024 122394.1 123208.9 124893.6 125564.3
```
