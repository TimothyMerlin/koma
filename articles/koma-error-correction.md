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

If exports $`x_t`$, world GDP $`y_t`$, and the exchange rate $`q_t`$ are
cointegrated, the long-run equilibrium relationship can be written as
``` math
x_t = \alpha + \beta_y y_t + \beta_q q_t + u_t .
```

The deviation from this equilibrium in the previous period defines the
error-correction term,
``` math
\mathrm{ECT}_{t-1}
= x_{t-1} - \alpha - \beta_y y_{t-1} - \beta_q q_{t-1} .
```

A minimal one-lag ECM for export growth is then
``` math
\Delta x_t
= \gamma
+ \phi\,\Delta x_{t-1}
+ \theta\,\Delta y_t
+ \lambda\,\mathrm{ECT}_{t-1}
+ \varepsilon_t .
```

Substituting the error-correction term into the ECM and expanding yields
``` math
\begin{aligned}
\Delta x_t
&= \gamma
+ \phi\,\Delta x_{t-1}
+ \theta\,\Delta y_t \\
&\quad
+ \lambda x_{t-1}
- \lambda\alpha
- \lambda\beta_y y_{t-1}
- \lambda\beta_q q_{t-1}
+ \varepsilon_t .
\end{aligned}
```

This is the form estimated in the model, where lagged levels enter
directly. The coefficient $`\lambda`$ is the speed of adjustment toward
the long-run equilibrium.

The long-run coefficients are recovered as
``` math
\beta_y = -\frac{\text{coef}(y_{t-1})}{\lambda},
\qquad
\beta_q = -\frac{\text{coef}(q_{t-1})}{\lambda}.
```

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
#>         consumption ~  0.36 - 0.02 * gdp  +  0.1 * consumption.L(1)
#>          investment ~  0.46  +  0.21 * investment.L(1)
#>             exports ~  - 1092.88  +  2.84 * world_gdp - 0.19 * exports.L(1) - 0.24 * exports_level.L(1)  +  0.56 * world_gdp_level.L(1)  +  0.06 * exchange_rate_level.L(1)
#>             imports ~  0.07  +  0.01 * exports  +  1.36 * consumption  +  0.91 * investment - 0.12 * imports.L(1)
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
#> constant                   -1092.88          
#>                           [-1707.82; -489.90]
#> exports.L(1)                  -0.19          
#>                           [   -0.37;   -0.03]
#> exports_level.L(1)            -0.24          
#>                           [   -0.38;   -0.10]
#> world_gdp_level.L(1)           0.56          
#>                           [    0.25;    0.87]
#> exchange_rate_level.L(1)       0.06          
#>                           [    0.01;    0.11]
#> world_gdp                      2.84          
#>                           [    1.93;    3.70]
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
#> [1] "Adjustment speed: -0.243, Long-run world GDP: 2.290, Exchange rate: 0.253"
```

The adjustment speed is -0.24, which is negative and implies about 24%
of the gap closes each period. The long-run elasticities imply that a 1%
rise in world GDP is associated with roughly 2.29% higher exports in the
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
#> Warning: ! Forecast draws raised warnings:
#> • 1000 of 1000 draws: ! Identity "exports_level" could not be checked.
#> • 1000 of 1000 draws: ! Identity "world_gdp_level" could not be checked.
#> • 1000 of 1000 draws: ! Identity "exchange_rate_level" could not be checked.
print(forecasts)
#> <koma_ts>
#> attributes:
#>   series_type: list[11]
#>   method: list[11]
#>   anker: list[11]
#> 
#> series:
#>         consumption investment exports imports    gdp domestic_demand
#> 2023 Q1      0.3633     0.3662  0.6760  1.0907 0.3384          0.3644
#> 2023 Q2      0.3909     0.6123  0.6867  1.1159 0.4192          0.4795
#> 2023 Q3      0.3880     0.5712  0.8183  0.8603 0.5746          0.4613
#> 2023 Q4      0.3907     0.6095  0.1604  1.0972 0.1627          0.4782
#> 2024 Q1      0.3781     0.5003  1.1389  0.7743 0.7427          0.4270
#> 2024 Q2      0.3783     0.5461  0.5293  0.9950 0.3608          0.4454
#> 2024 Q3      0.3882     0.5323  1.3523  0.9453 0.7984          0.4458
#> 2024 Q4      0.3855     0.6270  0.7458  1.1638 0.4280          0.4821
#>         exports_level world_gdp_level exchange_rate_level world_gdp
#> 2023 Q1      1168.772        2474.282             -0.7646    0.4827
#> 2023 Q2      1169.458        2474.690             -2.1510    0.4083
#> 2023 Q3      1170.277        2475.116             -3.9379    0.4259
#> 2023 Q4      1170.437        2475.407             -4.6881    0.2906
#> 2024 Q1      1171.576        2475.865             -5.2093    0.4586
#> 2024 Q2      1172.105        2476.261             -2.6797    0.3955
#> 2024 Q3      1173.458        2476.770             -4.9959    0.5096
#> 2024 Q4      1174.204        2477.210             -6.6168    0.4393
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
#> <koma_ts>
#> attributes:
#>   series_type:  chr "rate"
#>   method:  chr "diff_log"
#>   anker:  num [1:2] 118298 2023
#> 
#> series:
#>           Qtr1      Qtr2      Qtr3      Qtr4
#> 2023 0.6759805 0.6866861 0.8183252 0.1604028
#> 2024 1.1389034 0.5292635 1.3523289 0.7457637
level(forecasts$mean$exports)
#> <koma_ts>
#> attributes:
#>   series_type:  chr "level"
#>   method:  chr "diff_log"
#> 
#> series:
#>          Qtr1     Qtr2     Qtr3     Qtr4
#> 2022                            118297.6
#> 2023 119100.0 119920.6 120906.0 121100.1
#> 2024 122487.2 123137.2 124813.7 125748.0
```
