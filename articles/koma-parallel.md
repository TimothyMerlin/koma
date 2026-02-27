# Executing \`koma\` in Parallel

``` r
library(koma)
```

## Introduction

This vignette demonstrates how to use the
[`future::plan`](https://future.futureverse.org/reference/plan.html)
function to run `koma` package functions either sequentially or in
parallel, depending on your operating system and other preferences.

For more details, see the future package documentation:
<https://cran.r-project.org/web/packages/future/future.pdf>

## Preliminaries

Install the `future` and `parallelly` packages:

``` r
install.packages("future")
install.packages("parallelly")
```

We will use the simulated data contained in `koma` to illustrate the use
of [`future::plan`](https://future.futureverse.org/reference/plan.html).
The setup is the following:

``` r
library(koma)

equations <- "consumption ~ gdp + consumption.L(1),
investment ~ investment.L(1),
exports ~ world_gdp + exports.L(1),
imports ~ domestic_demand + imports.L(1),
prices ~ exchange_rate + oil_price + prices.L(1),
interest_rate ~ prices + interest_rate_germany + prices.L(1),
gdp == 0.6*consumption + 0.6*domestic_demand + 0.5*exports - 0.4*imports,
domestic_demand == 0.6*consumption + 0.4*investment"

exogenous_variables <- c("world_gdp", "interest_rate_germany", "exchange_rate", "oil_price")

dates <- list(
    estimation = list(start = c(1996, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 1), end = c(2023, 4))
)

sys_eq <- system_of_equations(equations, exogenous_variables)

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

## Sequential Execution

By default, R executes code sequentially. For example:

``` r
estimates <- estimate(ts_data, sys_eq, dates)
```

## Parallel Execution with `future::plan`

To enable parallel execution, you can use
[`future::plan`](https://future.futureverse.org/reference/plan.html).
The type of parallel execution depends on your operating system or
whether you are running the code on a cluster.

#### Worker Setup

You need to define the number of workers that your parallel job to run
on. Here we use all cores except one.

``` r
# Get the available workers
workers <- parallelly::availableCores(omit = 1)
```

### Windows

For Windows, you can use **multisession**:

``` r
future::plan("future::multisession", workers = workers)

estimates <- estimate(ts_data, sys_eq, dates)
```

### Unix-like systems

For Unix-like systems, you can use **multicore**:

``` r
future::plan("future::multicore", workers = workers)

estimates <- estimate(ts_data, sys_eq, dates)
```
