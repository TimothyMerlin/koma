# Klein macroeconomic time series (1970 Q1 onward)

A list of U.S. macro series from FRED (14 quarterly series).

## Usage

``` r
klein
```

## Format

A `list` of 14 `ts` objects:

- gdp:

  Real GDP (bil. 2017 USD, SAAR; FRED: GDPC1)

- consumption:

  Real PCE (bil. 2017 USD, SAAR; FRED: PCECC96)

- investment:

  Real private investment (bil. 2017 USD, SAAR; FRED: GPDIC1)

- government:

  Real government spending (bil. 2017 USD, SAAR; FRED: GCEC1)

- net_exports:

  Real net exports (bil. 2017 USD, SAAR; FRED: NETEXC)

- n_gdp:

  Nominal GDP (bil. USD, SAAR; FRED: GDP)

- n_consumption:

  Nominal PCE (bil. USD, SAAR; FRED: PCEC)

- n_investment:

  Nominal private investment (bil. USD, SAAR; FRED: GPDI)

- n_government:

  Nominal government spending (bil. USD, SAAR; FRED: GCE)

- n_profits:

  Nominal profits after tax (bil. USD, SAAR; FRED: CP)

- n_wages:

  Nominal private wages & salaries (bil. USD; FRED: A132RC1Q027SBEA)

- n_government_wages:

  Nominal government wages & salaries (bil. USD; FRED: B202RC1Q027SBEA)

- n_taxes:

  Nominal fed tax receipts (bil. USD; FRED: W007RC1Q027SBEA)

- gdp_deflator:

  GDP implicit price deflator (2017=100, SAAR; FRED: GDPDEF)

## Source

<https://fred.stlouisfed.org/>
