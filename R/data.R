#' Klein macroeconomic time series (1970 Q1 onward)
#'
#' A list of U.S. macro series from FRED (14 quarterly series).
#'
#' @format A \code{list} of 14 \code{ts} objects:
#' \describe{
#'   \item{gdp}{Real GDP (bil. 2017 USD, SAAR; FRED: GDPC1)}
#'   \item{consumption}{Real PCE (bil. 2017 USD, SAAR; FRED: PCECC96)}
#'   \item{investment}{Real private investment (bil. 2017 USD, SAAR;
#'     FRED: GPDIC1)}
#'   \item{government}{Real government spending (bil. 2017 USD, SAAR;
#'     FRED: GCEC1)}
#'   \item{net_exports}{Real net exports (bil. 2017 USD, SAAR;
#'     FRED: NETEXC)}
#'   \item{n_gdp}{Nominal GDP (bil. USD, SAAR; FRED: GDP)}
#'   \item{n_consumption}{Nominal PCE (bil. USD, SAAR; FRED: PCEC)}
#'   \item{n_investment}{Nominal private investment (bil. USD, SAAR;
#'     FRED: GPDI)}
#'   \item{n_government}{Nominal government spending (bil. USD, SAAR;
#'     FRED: GCE)}
#'   \item{n_profits}{Nominal profits after tax (bil. USD, SAAR;
#'     FRED: CP)}
#'   \item{n_wages}{Nominal private wages & salaries (bil. USD;
#'     FRED: A132RC1Q027SBEA)}
#'   \item{n_government_wages}{Nominal government wages & salaries (bil. USD;
#'     FRED: B202RC1Q027SBEA)}
#'   \item{n_taxes}{Nominal fed tax receipts (bil. USD;
#'     FRED: W007RC1Q027SBEA)}
#'   \item{gdp_deflator}{GDP implicit price deflator (2017=100, SAAR;
#'     FRED: GDPDEF)}
#' }
#' @source \url{https://fred.stlouisfed.org/}
"klein"

#' Quarterly time series of key macro variables for a small open economy (Switzerland)
#'
#' @format A \code{list} of 12 \code{ts} objects (quarterly):
#' \describe{
#'   \item{consumption}{Private consumption \(C_t\).}
#'   \item{investment}{Gross fixed capital formation \(I_t\).}
#'   \item{exports}{Exports \(X_t\).}
#'   \item{imports}{Imports \(M_t\).}
#'   \item{prices}{Price level \(P_t\).}
#'   \item{interest_rate}{Short‐term domestic interest rate \(R_t\).}
#'   \item{gdp}{Equilibrium output \(GDP_t\).}
#'   \item{domestic_demand}{Domestic demand \(D_t\).}
#'   \item{world_gdp}{World GDP (exogenous) \(WGDP_t\).}
#'   \item{interest_rate_germany}{German short‐term rate (exog) \eqn{R_{GE,t}}.}
#'   \item{exchange_rate}{Nominal exchange rate (exog) \(ER_t\).}
#'   \item{oil_price}{Oil price (exogenous) \(OP_t\).}
#' }
#' @source \url{https://www.seco.admin.ch/}, \url{https://kof.ethz.ch/}
"small_open_economy"
