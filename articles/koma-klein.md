# Estimating Klein's Model I

## Equations of Klein’s Model

### Stochastic equations

$$\begin{aligned}
{\text{consumption}\quad C_{t}} & {= \beta_{0} + \alpha_{1}P_{t} + \alpha_{2}P_{t - 1} + \alpha_{3}(W_{t}^{p} + W_{t}^{g}) + \varepsilon_{1t},} \\
{\text{investment}\quad I_{t}} & {= \beta_{0} + \beta_{1}P_{t} + \beta_{2}P_{t - 1} + \beta_{3}K_{t - 1} + \varepsilon_{2t},} \\
{\text{private wages}\quad W_{t}^{p}} & {= \gamma_{0} + \gamma_{1}Y_{t} + \gamma_{2}Y_{t - 1} + \gamma_{3}A_{t} + \varepsilon_{3t}.}
\end{aligned}$$

The $\alpha$’s, $\beta$’s, and $\gamma$’s are the coefficients of the
model. $P_{t}$ is the private profits, $W_{t}^{p}$ is the private wages,
$W_{t}^{g}$ is the government wages, $K_{t - 1}$ is the capital stock at
time $t - 1$, and $A_{t}$ is a time trend. The $\varepsilon_{it}$ are
stochastic error terms.

### Equilibrium condition

$$\begin{aligned}
{\text{equilibrium demand}\quad Y_{t}} & {= C_{t} + I_{t} + G_{t}}
\end{aligned}$$

$Y_{t}$ is the equilibrium demand, $C_{t}$ is the consumption, $I_{t}$
is the investment, and $G_{t}$ is the government spending.

### Identities

$$\begin{aligned}
{\text{equilibrium demand}\quad Y_{t}} & {= C_{t} + I_{t} + G_{t}} \\
{\text{private profits}\quad P_{t}} & {= Y_{t} - T_{t} - W_{t}^{p}} \\
{\text{capital stock}\quad K_{t}} & {= K_{t - 1} + I_{t}}
\end{aligned}$$

Here, $T_{t}$ represents taxes, $K_{t}$ denotes the capital stock at
time $t$, and $G_{t}$ signifies government spending.

## Defining Klein’s Model in R

To estimate and forecast this model, we need to specify the equations,
the exogenous variables, and the date ranges for estimation and
forecasting.

``` r
equations <- "consumption ~ profits + profits.L(1) + total_wages,
investment ~ profits + profits.L(1) + capital_stock.L(1),
wages ~ gdp + gdp.L(1) + time_trend,
gdp == (n_consumption/n_gdp)*consumption + (n_investment/n_gdp)*investment + (n_government/n_gdp)*government,
profits == (n_gdp/n_profits)*gdp - (n_taxes/n_profits)*taxes - (n_wages/n_profits)*wages,
capital_stock == 1.001*capital_stock.L(1) + 0.0002*investment,
total_wages == (n_wages/n_total_wages)*wages + (n_government_wages/n_total_wages)*government_wages"

exogenous_variables <- c("government", "government_wages", "taxes", "time_trend")
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
##   consumption ~  constant + profits + profits.L(1) + total_wages
##    investment ~  constant + profits + profits.L(1) + capital_stock.L(1)
##         wages ~  constant + gdp + gdp.L(1) + time_trend
##           gdp == (n_consumption/n_gdp) * consumption + (n_investment/n_gdp) * investment + (n_government/n_gdp) * government
##       profits == (n_gdp/n_profits) * gdp-(n_taxes/n_profits) * taxes-(n_wages/n_profits) * wages
## capital_stock == 1.001 * capital_stock.L(1) + 0.0002 * investment
##   total_wages == (n_wages/n_total_wages) * wages + (n_government_wages/n_total_wages) * government_wages
```

## Defing the Estimation and Forecast Dates

Now, we need to specify the dates for estimation and forecasting.

``` r
dates <- list(
  estimation = list(start = c(1990, 1), end = c(2020, 4)),
  forecast = list(start = c(2021, 1), end = c(2023, 4)),
  dynamic_weights = list(start = c(1990, 1), end = c(2020, 4))
)
```

## Preparing the Data

We will use the `klein` dataset, which contains the required variables
for the model. This dataset is a list of standard time series (`ts`)
objects, where each object corresponds to a variable in the model. You
can explore the dataset by using the
[`?klein`](https://timothymerlin.github.io/koma/reference/klein.md)
command to view its documentation and structure.

``` r
?klein
```

To prepare the dataset for use with the `koma` package, we need to
convert the `ts` objects to the `ets` format using the `as_ets`
function.

The model is estimated in growth rates. The `klein` dataset is in
levels. We use the `ets` to tell the model that the data is in levels
and how to convert it to growth rates. `ets` is generalized to accept
any additional attributes. Below we use this feature to save the series’
value_type (real or nominal).

``` r
# Some series in the klein dataset are nominal. We need to deflate them using the GDP deflator.
klein$profits <- klein$n_profits / (klein$d_gdp / 100)
klein$capital_stock <- klein$n_capital_stock / (klein$d_gdp / 100)
klein$wages <- klein$n_wages / (klein$d_gdp / 100)
klein$government_wages <- klein$n_government_wages / (klein$d_gdp / 100)
klein$taxes <- klein$n_taxes / (klein$d_gdp / 100)

real_series <- c(
  "gdp", "consumption", "investment", "profits", "capital_stock",
  "government", "wages", "government_wages", "taxes"
)
real_ts_data <- lapply(real_series, function(x) {
  as_ets(klein[[x]],
    series_type = "level", method = "diff_log", value_type = "real"
  )
})
names(real_ts_data) <- real_series

real_ts_data$total_wages <- real_ts_data$government_wages + real_ts_data$wages

# net exports can be negative, we thus want to use percentage change instead of log difference
real_ts_data$net_exports <- as_ets(klein$net_exports,
  series_type = "level", method = "percentage", value_type = "real"
)

# To compute the weights in the model, we include the nominal series in the dataset as well.
nominal_series <- c(
  "n_gdp", "n_investment", "n_government", "n_government_wages",
  "n_taxes", "n_profits", "n_wages", "n_consumption"
)
nominal_ts_data <- lapply(nominal_series, function(x) {
  as_ets(klein[[x]],
    series_type = "level", method = "diff_log", value_type = "nominal"
  )
})
names(nominal_ts_data) <- nominal_series

nominal_ts_data$n_total_wages <- nominal_ts_data$n_government_wages + nominal_ts_data$n_wages

# Combining the real and nominal series
ts_data <- c(real_ts_data, nominal_ts_data)

# Adding the time trend
ts_data$time_trend <- ets(seq_len(length(klein$taxes)),
  start = stats::start(ts_data$taxes), frequency = 4,
  series_type = "level", method = "none", value_type = NA
)
```

With the data prepared, we can now proceed to estimate the model. Before
forecasting, however, we need to truncate the endogenous variables to
ensure they only include data up to the forecast start date.

``` r
ts_data[sys_eq$endogenous_variables] <-
  lapply(sys_eq$endogenous_variables, function(x) {
    stats::window(ts_data[[x]], end = c(2020, 4))
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
print(estimates)
## 
## ── Estimates ───────────────────────────────────────────────────────────────────
##   consumption ~  - 0.12  +  0.13 * profits - 0.03 * 0.13 * profits.L(1)  +  1.11 * total_wages
##    investment ~  - 0.86  +  0.39 * profits  +  0.16 * 0.39 * profits.L(1)  +  1.51 * capital_stock.L(1)
##         wages ~  - 0.68  +  0.95 * gdp  +  0.43 * 0.95 * gdp.L(1)  +  0 * time_trend
##           gdp == (n_consumption/n_gdp) * consumption  +  (n_investment/n_gdp) * investment  +  (n_government/n_gdp) * government
##       profits == (n_gdp/n_profits) * gdp - (n_taxes/n_profits) * taxes - (n_wages/n_profits) * wages
## capital_stock == 1.001 * capital_stock.L(1)  +  0.0002 * investment
##   total_wages == (n_wages/n_total_wages) * wages  +  (n_government_wages/n_total_wages) * government_wages
```

``` r
summary(estimates)
## 
## =================================================================
##                     consumption     investment     wages         
## -----------------------------------------------------------------
## constant             -0.12           -0.86          -0.68        
##                     [-0.31;  0.08]  [-1.84; 0.02]  [-1.34; -0.06]
## profits.L(1)         -0.03            0.16                       
##                     [-0.05; -0.00]  [ 0.07; 0.24]                
## profits               0.13            0.39                       
##                     [ 0.10;  0.16]  [ 0.21; 0.57]                
## total_wages           1.11                                       
##                     [ 0.97;  1.25]                               
## capital_stock.L(1)                    1.51                       
##                                     [ 0.41; 2.54]                
## gdp.L(1)                                             0.43        
##                                                    [ 0.30;  0.56]
## time_trend                                           0.00        
##                                                    [-0.00;  0.01]
## gdp                                                  0.95        
##                                                    [ 0.80;  1.10]
## =================================================================
## Posterior mean (90% credible interval: [5.0%,  95.0%])
```

To see how to run the estimation in parallel, refer to the [parallel
estimation vignette](https://timothymerlin.github.io/koma/parallel.md).

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
## ✔ Forecasting completed.
print(forecasts)
##         consumption  investment      wages         gdp   profits capital_stock
## 2021 Q1  -1.2940063 -0.99556028 -0.3387788 -0.82305516 -7.322447      2.503406
## 2021 Q2  -0.2981365  1.91448090 -0.4060502 -0.04343018  0.315740      2.506293
## 2021 Q3  -1.2892002  1.09045377 -0.7286282 -0.73506909 -4.884415      2.509017
## 2021 Q4  -0.9010011  1.68225126 -0.6182451 -0.31225408 -1.282972      2.511862
## 2022 Q1  -0.7045260  2.27464617 -0.3376342 -0.21102473 -1.210574      2.514829
## 2022 Q2  -0.9725958  2.00662704 -0.4284649 -0.35510899 -1.942482      2.517745
## 2022 Q3  -1.6702770  0.55227115 -1.0553909 -0.95145610 -5.411500      2.520374
## 2022 Q4  -2.0561957 -0.17733502 -1.5230350 -1.17774912 -5.912272      2.522858
## 2023 Q1  -2.0613849 -0.13030837 -1.6256864 -1.18724541 -5.599824      2.525355
## 2023 Q2  -1.7606452  0.28464001 -1.4495471 -1.00158969 -4.664612      2.527938
## 2023 Q3  -2.1741368 -0.45391948 -1.6479523 -1.29640757 -6.954730      2.530375
## 2023 Q4  -1.8209727 -0.07385907 -1.5616212 -1.07691071 -5.065443      2.532890
##         total_wages government government_wages      taxes time_trend
## 2021 Q1  -0.3791242  1.2757877       -0.5981850  0.8997407        205
## 2021 Q2  -0.3776446 -1.0791366       -0.2234124  9.1808930        206
## 2021 Q3  -0.4831044 -0.3873293        0.8500016  0.3181641        207
## 2021 Q4  -0.6770865 -0.0707721       -0.9965740  4.6250778        208
## 2022 Q1  -0.4214105 -0.8533318       -0.8762856  3.5974820        209
## 2022 Q2  -0.5764775 -0.3751771       -1.3801329 -1.3602277        210
## 2022 Q3  -0.8201585  0.3882606        0.4570688 -5.3550978        211
## 2022 Q4  -1.1938060  1.3228264        0.5937889 -6.2183336        212
## 2023 Q1  -1.2469188  1.2402222        0.8096532 -6.4787268        213
## 2023 Q2  -1.0767545  0.7228893        0.9473758 -2.6579290        214
## 2023 Q3  -1.1609521  1.3819458        1.4832848 -2.8205388        215
## 2023 Q4  -1.1171900  0.8931133        1.2959118 -2.1251498        216
```

``` r
plot(forecasts, variables = "gdp")
```
