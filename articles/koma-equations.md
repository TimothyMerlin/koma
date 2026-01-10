# Equation Syntax Reference

## Overview

This vignette describes the syntax for specifying stochastic and
identity equations, priors, and lags in the `koma` package.

### 1. Stochastic Equations

Stochastic (regression) equations model a dependent variable with an
error term. An intercept is included by default.

``` r
# With default intercept:
consumption ~ gdp + consumption.L(1)

# Without intercept:
consumption ~ gdp + consumption.L(1) - 1

# Explicit intercept:
consumption ~ 1 + gdp + consumption.L(1)
```

### 2. Identity Equations

Identity equations enforce exact relationships.

``` r
# Identity equations with explicitly defined weights:
# To aggregate the component growth rates into a growth rate for GDP we need to define weights.
# This is done by specifying the weights in the equation. 
# You can, e.g. use the nominal level weights of the last observed period.
gdp == 0.7*consumption + 0.2*investment + 0.2*government - 0.1*net_exports 
```

### 3. Injected Parameters

``` r
# Ratios computed from data:
gdp == (nom_cons/nom_gdp) * cons
```

### 4. Lag Notation

Lags are specified with `L()` or
[`lag()`](https://rdrr.io/r/stats/lag.html) notation. Ranges and
combinations are supported.

``` r
# Single lag:
x.L(1)
lag(x, 1)

# Range of lags:
x.L(1:4)
lag(x, 1:4)

# Mix range and specific lags:
x.L(1:3, 5)
```

### 5. Priors

Specify priors before coefficients using curly braces
[`{}`](https://rdrr.io/r/base/Paren.html).

``` r
# normal(1,0.1):
{1, 0.1} x3
```

### 6. Equation-specific Tau

You can override the default tau in your `gibbs_settings` for a single
equation by appending `[tau = value]` after its equation. If the
acceptance rate falls outside 30%-60 %, a warning is emitted.

``` r
"consumption ~ constant + gdp + consumption.L(1) + consumption.L(2) [tau = 1.2]"
```
