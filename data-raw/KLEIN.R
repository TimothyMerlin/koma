## Setup
# 1. Get a free FRED API key from https://fred.stlouisfed.org/docs/api/api_key.html
# 2. Save it in a file called fred_key.R in the data-raw directory as:
# fred_api_key <- "your_api_key_here"

source("data-raw/fred_key.R")
# fredr::fredr_set_key(fred_api_key)

quarterly_series_ids <- c(
  #  Real Gross Domestic Product, Billions of Chained 2017 Dollars, SAAR
  gdp = "GDPC1",
  # Real Personal Consumption Expenditures, Billions of Chained 2017 Dollars, SAAR
  consumption = "PCECC96",
  # Real Gross Private Domestic Investment, Billions of Chained 2017 Dollars, SAAR
  investment = "GPDIC1",
  # Real Government Consumption Expenditures & Gross Investment, Billions of Chained 2017 Dollars, SAAR
  government = "GCEC1",
  # Real Net Exports of Goods & Services, Billions of Chained 2017 Dollars, SAAR
  net_exports = "NETEXC",
  # Gross Domestic Product, current‐cost (nominal) Dollars, (SAAR)
  n_gdp = "GDP",
  # Nominal Personal Consumption Expenditures, Billions of Dollars, SAAR
  n_consumption = "PCEC",
  # Nominal Gross Private Domestic Investment, Billions of Dollars, SAAR
  n_investment = "GPDI",
  # Nominal Government Consumption Expenditures & Gross Investment, Billions of Dollars, SAAR
  n_government = "GCE",
  # Corporate Profits After Tax (without IVA & CCAdj), Billions of Dollars, SAAR
  n_profits = "CP",
  # Compensation of Employees: Wages & Salaries: Private Industries, Quarterly
  n_wages = "A132RC1Q027SBEA",
  # Compensation of Employees: Wages & Salaries: Government, Quarterly .
  n_government_wages = "B202RC1Q027SBEA",
  # Federal Government Current Tax Receipts: Taxes on Production & Imports, Quarterly
  n_taxes = "W007RC1Q027SBEA",
  # Gross Domestic Product: Implicit Price Deflator, Index 2017=100 SAAR
  d_gdp = "GDPDEF"
)
yearly_series_ids <- c(
  # Current-Cost Net Stock of Fixed Assets: Private, Annual
  n_capital_stock = "K1PTOTL1ES000"
)

start_date <- as.Date("1970-01-01")

quarterly_fred <- lapply(quarterly_series_ids, function(id) {
  x <- fredr::fredr(
    series_id = id,
    observation_start = start_date
  )
  # Extract value vector and date vector
  value <- x$value
  dates <- x$date

  yr <- as.integer(format(min(dates), "%Y"))
  qtr <- as.integer((as.integer(format(min(dates), "%m")) - 1) / 3) + 1

  ts(value, start = c(yr, qtr), frequency = 4, )
})

yearly_fred <- lapply(yearly_series_ids, function(id) {
  x <- fredr::fredr(
    series_id = id,
    observation_start = start_date
  )
  # Extract value vector and date vector
  value <- x$value
  dates <- x$date

  yr <- as.integer(format(min(dates), "%Y"))
  qtr <- as.integer((as.integer(format(min(dates), "%m")) - 1) / 3) + 1

  ts(value, start = c(yr, qtr), frequency = 1, )
})

# capital stock is annual data
# temp disagg to convert annual data to quarterly
td_mod <- tempdisagg::td(yearly_fred$n_capital_stock ~ 1,
  to = "quarterly",
  method = "denton-cholette"
)
n_capital_stock <- predict(td_mod)

klein <- list(
  gdp = quarterly_fred$gdp,
  consumption = quarterly_fred$consumption,
  investment = quarterly_fred$investment,
  government = quarterly_fred$government,
  net_exports = quarterly_fred$net_exports,
  n_gdp = quarterly_fred$n_gdp,
  n_consumption = quarterly_fred$n_consumption,
  n_investment = quarterly_fred$n_investment,
  n_government = quarterly_fred$n_government,
  n_profits = quarterly_fred$n_profits,
  n_wages = quarterly_fred$n_wages,
  n_government_wages = quarterly_fred$n_government_wages,
  n_taxes = quarterly_fred$n_taxes,
  n_capital_stock = n_capital_stock,
  d_gdp = quarterly_fred$d_gdp
)
usethis::use_data(klein, overwrite = TRUE)
