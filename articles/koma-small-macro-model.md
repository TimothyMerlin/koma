# Estimating Small Macro Model for Switzerland

## Equations of the Small Macro Model

**Stochastic equations**

$$\begin{aligned}
{\text{Consumption}\quad C_{t}} & {= \beta_{1} + \beta_{1,2}C_{t - 1} + \gamma_{1,7}Y_{t} + \varepsilon_{1t},} \\
{\text{Investment}\quad I_{t}} & {= \beta_{2} + \beta_{2,3}I_{t - 1} + \varepsilon_{2t},} \\
{\text{Exports}\quad X_{t}} & {= \beta_{3} + \beta_{3,4}X_{t - 1} + \beta_{3,7}WGDP_{t} + \varepsilon_{3t},} \\
{\text{Imports}\quad M_{t}} & {= \beta_{4} + \gamma_{4,8}D_{t} + \beta_{4,5}I_{t - 1} + \varepsilon_{4t},} \\
{\text{Prices}\quad P_{t}} & {= \beta_{5} + \beta_{5,9}ER_{t} + \beta_{5,10}OP_{t} + \beta_{5,6}P_{t - 1} + \varepsilon_{5t},} \\
{\text{Interest Rate}\quad R_{t}} & {= \beta_{6} + \beta_{6,6}P_{t - 1} + \beta_{6,8}R_{GE,t} + \gamma_{6,5}P_{t} + \varepsilon_{6t}.}
\end{aligned}$$

**Equilibrium condition**

$$\begin{aligned}
{\text{Equilibrium Ouput}\quad GDP_{t}} & {= 0.6*C_{t} + 0.6*D_{t} + 0.5*X_{t} - 0.4*I_{t}}
\end{aligned}$$

**Identities**

$$\begin{aligned}
{\text{Domestic Demand}\quad D_{t}} & {= 0.6*C_{t} + 0.4*I_{t}}
\end{aligned}$$

**Coefficients**  
- The $\beta$’s and $\gamma$’s are structural coefficients to be
estimated.

**Endogenous variables**

{C_t}{Consumption at time .} {I_t}{Gross fixed capital formation
(investment) at time .} {X_t}{Exports at time .} {M_t}{Imports at time
.} {P_t}{Price level at time .} {R_t}{Short-term interest rate at time
.} {D_t}{Domestic demand, defined by .}

{GDP_t}{Equilibrium output (demand), defined by .}

**Exogenous variables**

{WGDP_t}{World GDP at time .} {ER_t}{Exchange rate at time .} {OP_t}{Oil
price at time .}

{}{German interest rate at time }.

**Other variables**

{G_t}{Government spending at time .} {T_t}{Taxes at time .}

{epsilon_it}{Stochastic error term for equation at time .}

## Defining the SEM in R

To estimate and forecast this model, we need to specify the equations,
the exogenous variables, and the date ranges for estimation and
forecasting.

``` r
equations <- "consumption ~ gdp + consumption.L(1),
investment ~ investment.L(1),
exports ~ world_gdp + exports.L(1),
imports ~ domestic_demand + imports.L(1),
prices ~ exchange_rate + oil_price + prices.L(1),
interest_rate ~ prices + interest_rate_germany + prices.L(1),
gdp == 0.6*consumption + 0.6*domestic_demand + 0.5*exports - 0.4*imports,
domestic_demand == 0.6*consumption + 0.4*investment"

exogenous_variables <- c("world_gdp", "interest_rate_germany", "exchange_rate", "oil_price")
```

The `equations` string contains the model equations, while
`exogenous_variables` lists the exogenous variables. The `.L(1)`
notation indicates a lag of one period. For further details on the
syntax, please refer to the [equation syntax
documentation](https://timothymerlin.github.io/koma/equations.md).

## Creating the SEM

The `system_of_equations` function is used to create a system of
equations object, which can then be used for estimation and forecasting.

``` r
library(koma)

sys_eq <- system_of_equations(
  equations = equations,
  exogenous_variables = exogenous_variables
)

print(sys_eq)
## 
## ── System of Equations ─────────────────────────────────────────────────────────
##     consumption ~  constant + gdp + consumption.L(1)
##      investment ~  constant + investment.L(1)
##         exports ~  constant + world_gdp + exports.L(1)
##         imports ~  constant + domestic_demand + imports.L(1)
##          prices ~  constant + exchange_rate + oil_price + prices.L(1)
##   interest_rate ~  constant + prices + interest_rate_germany + prices.L(1)
##             gdp == 0.6 * consumption + 0.6 * domestic_demand + 0.5 * exports-0.4 * imports
## domestic_demand == 0.6 * consumption + 0.4 * investment
```

## Defing the Estimation and Forecast Dates

Now, we need to specify the dates for estimation and forecasting.

``` r
dates <- list(
  estimation = list(start = c(1996, 1), end = c(2019, 4)),
  forecast = list(start = c(2023, 1), end = c(2023, 4))
)
```

## Preparing the Data

We will use the `small_open_economy` dataset, which contains the
required variables for the model. This dataset is a list of standard
time series (`ts`) objects, where each object corresponds to a variable
in the model. You can explore the dataset by using the
[`?small_open_economy`](https://timothymerlin.github.io/koma/reference/small_open_economy.md)
command to view its documentation and structure.

``` r
?small_open_economy
```

To prepare the dataset for use with the `koma` package, we need to
convert the `ts` objects to the `ets` format using the `as_ets`
function.

The model is estimated in growth rates. The `small_open_economy` dataset
is in levels. We use the `ets` to tell the model that the data is in
levels and how to convert it to growth rates. `ets` is generalized to
accept any additional attributes. Below we use this feature to save the
series’ value_type (real or nominal).

``` r
series <- names(small_open_economy)
series <- series[!series %in% c("interest_rate", "interest_rate_germany")]

ts_data <- lapply(series, function(x) {
  as_ets(small_open_economy[[x]],
    series_type = "level", method = "diff_log"
  )
})
names(ts_data) <- series

ts_data$interest_rate <- as_ets(small_open_economy$interest_rate,
  series_type = "rate", method = "none"
)
ts_data$interest_rate_germany <- as_ets(small_open_economy$interest_rate_germany,
  series_type = "rate", method = "none"
)
```

With the data prepared, we can now proceed to estimate the model. Before
forecasting, however, we need to truncate the endogenous variables to
ensure they only include data up to the forecast start date.

``` r
ts_data[sys_eq$endogenous_variables] <-
  lapply(sys_eq$endogenous_variables, function(x) {
    stats::window(ts_data[[x]], end = c(2019, 4))
  })
```

## Estimating the Model

The names of the list elements should match the variable names used in
the equations. Now, we can estimate the model using the `estimate`
function.

``` r
estimates <- estimate(
  sys_eq,
  ts_data = ts_data,
  dates = dates
)
## 
## ── Gibbs Sampler Settings ──────────────────────────────────────────────────────
## ── System Wide Settings ──
## • Number of draws (`ndraws`): 2000
## • Burn-in ratio (`burnin_ratio`): 0.5
## • Burn-in (`burnin`): 1000
## • Store frequency (`nstore`): 1
## • Number of saved draws (`nsave`): 1000
## • Tau (`tau`): 1.1
## 
## 
## ── Estimation ──────────────────────────────────────────────────────────────────
## 
## ── ⚠ MCMC Acceptance Probability Warnings ──────────────────────────────────────
## • consumption: 60.7%
## 
## ℹ Some acceptance probabilities are outside the recommended range (20%-60%).
## Consider revising the equations, tuning each equation's tau, or adjusting your priors.
print(estimates)
## 
## ── Estimates ───────────────────────────────────────────────────────────────────
##     consumption ~  0.36 - 0.03 * gdp  +  0.11 * consumption.L(1)
##      investment ~  0.45  +  0.21 * investment.L(1)
##         exports ~  - 0.29  +  3.2 * world_gdp - 0.3 * exports.L(1)
##         imports ~  - 0.05  +  2.61 * domestic_demand - 0.11 * imports.L(1)
##          prices ~  0.04  +  0.03 * exchange_rate  +  0.01 * oil_price  +  0.56 * prices.L(1)
##   interest_rate ~  - 0.51  +  0.47 * prices  +  0.57 * interest_rate_germany - 0.19 * prices.L(1)
##             gdp == 0.6 * consumption  +  0.6 * domestic_demand  +  0.5 * exports - 0.4 * imports
## domestic_demand == 0.6 * consumption  +  0.4 * investment
```

``` r
summary(estimates)
## 
## ===============================================================================================================
##                        consumption    investment    exports         imports        prices        interest_rate 
## ---------------------------------------------------------------------------------------------------------------
## constant                 0.36          0.45          -0.29           -0.05          0.04          -0.51        
##                        [ 0.28; 0.46]  [0.19; 0.71]  [-0.86;  0.32]  [-0.55; 0.44]  [0.01; 0.06]  [-0.63; -0.40]
## consumption.L(1)         0.11                                                                                  
##                        [-0.08; 0.29]                                                                           
## gdp                     -0.03                                                                                  
##                        [-0.13; 0.06]                                                                           
## investment.L(1)                        0.21                                                                    
##                                       [0.03; 0.40]                                                             
## exports.L(1)                                         -0.30                                                     
##                                                     [-0.48; -0.13]                                             
## world_gdp                                             3.20                                                     
##                                                     [ 2.27;  4.05]                                             
## imports.L(1)                                                         -0.11                                     
##                                                                     [-0.28; 0.06]                              
## domestic_demand                                                       2.61                                     
##                                                                     [ 1.62; 3.74]                              
## prices.L(1)                                                                         0.56          -0.19        
##                                                                                    [0.46; 0.66]  [-0.60;  0.21]
## exchange_rate                                                                       0.03                       
##                                                                                    [0.02; 0.05]                
## oil_price                                                                           0.01                       
##                                                                                    [0.01; 0.01]                
## interest_rate_germany                                                                              0.57        
##                                                                                                  [ 0.52;  0.62]
## prices                                                                                             0.47        
##                                                                                                  [ 0.05;  0.96]
## ===============================================================================================================
## Posterior mean (90% credible interval: [5.0%, 95.0%])
## Estimation period: 1996 Q1 - 2019 Q4
```

``` r
summary(estimates, variables = "investment")
## 
## =============================
##                  investment  
## -----------------------------
## constant          0.45       
##                  [0.19; 0.71]
## investment.L(1)   0.21       
##                  [0.03; 0.40]
## =============================
## Posterior mean (90% credible interval: [5.0%, 95.0%])
## Estimation period: 1996 Q1 - 2019 Q4
```

To see how to run the estimation in parallel, refer to the [parallel
estimation vignette](https://timothymerlin.github.io/koma/parallel.md).

## Diagnostics

You can visualize MCMC diagnostics for coefficient draws using
[`trace_plot()`](https://timothymerlin.github.io/koma/reference/trace_plot.md).

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  trace_plot(estimates, variables = c("consumption", "investment"))
}
```

You can also inspect running means (cumulative averages) with
[`running_mean_plot()`](https://timothymerlin.github.io/koma/reference/running_mean_plot.md).

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  running_mean_plot(estimates, variables = c("consumption", "investment"))
}
```

You can inspect autocorrelation diagnostics with
[`acf_plot()`](https://timothymerlin.github.io/koma/reference/acf_plot.md).

``` r
if (requireNamespace("ggplot2", quietly = TRUE)) {
  acf_plot(estimates, variables = c("consumption", "investment"))
}
```

## Forecasting

To forecast the model, we can use the `forecast` function. This function
will generate forecasts for the specified date range.

``` r
forecasts <- forecast(
  estimates,
  dates = dates
)
## 
## ── Forecast ────────────────────────────────────────────────────────────────────
## 
## Conditional fill detected after "2019 Q4".
## Missing observations for: "consumption", "investment", "exports", "imports",
## "prices", "interest_rate", "gdp", and "domestic_demand"
## Missing values will be conditionally filled up to "2022 Q4" before forecasting.
## ✔ Forecasting completed.
print(forecasts)
##         consumption investment exports imports  prices interest_rate    gdp
## 2023 Q1      0.3940     0.4953  1.2033  1.0677 -0.0587        0.8385 0.6717
## 2023 Q2      0.3821     0.4917  0.6258  0.9778 -0.0703        1.2689 0.4066
## 2023 Q3      0.3836     0.5117  0.7445  0.9278  0.0100        1.5664 0.4922
## 2023 Q4      0.3883     0.5711  0.3600  0.9941 -0.0114        1.6947 0.2922
##         domestic_demand world_gdp interest_rate_germany exchange_rate oil_price
## 2023 Q1          0.4345    0.4827                2.3887        0.9217   -8.7520
## 2023 Q2          0.4259    0.4083                3.1460       -1.3863   -3.3787
## 2023 Q3          0.4348    0.4259                3.6393       -1.7869    9.9001
## 2023 Q4          0.4614    0.2906                3.8850       -0.7502   -3.3373
```

``` r
rate(forecasts$mean$gdp)
## rate, diff_log, c(192510.114596192, 2022.75)
##           Qtr1      Qtr2      Qtr3      Qtr4
## 2023 0.6716659 0.4066033 0.4921892 0.2921809
level(forecasts$mean$gdp)
## level, diff_log
##          Qtr1     Qtr2     Qtr3     Qtr4
## 2022                            192510.1
## 2023 193807.5 194597.1 195557.3 196129.5
```

### Conditional Forecasting

Conditional forecasting allows you to impose restrictions on certain
variables during the forecast period to fix their values. This is useful
for scenario analysis and policy evaluation, as the forecasts will
reflect these user-defined conditions.

``` r
fig <- plot(forecasts, variables = "prices")
fig

restrictions <- list(prices = list(value = 0.5, horizon = 1))

forecasts <- forecast(estimates,
  dates = dates,
  restrictions = restrictions
)
plot(forecasts, fig = fig, variables = "prices")
forecasts$mean$prices
```
