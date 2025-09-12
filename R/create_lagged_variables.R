#' Create Lagged Variables for Time Series Data
#'
#' This function creates lagged variables for the given time series data,
#' considering the provided endogenous and exogenous variables along with
#' predetermined variables. The lagged variables are created based on the
#' pattern of the predetermined variables.
#'
#' @param ts_data A named list of time series objects.
#' @param endogenous_variables A character vector of names of endogenous
#' variables.
#' @param exogenous_variables A character vector of names of exogenous
#' variables.
#' @param predetermined_variables A character vector of names of predetermined
#' variables, which contains lag information in the format "var.L(k)" where
#' "var" is the variable name and "k" is the lag (e.g., "var.L(1)" for the
#' first lag of the variable "var").
#'
#' @return A named list of time series objects with the lagged variables
#' added according to the predetermined pattern.
#' @keywords internal
create_lagged_variables <- function(ts_data, endogenous_variables,
                                    exogenous_variables,
                                    predetermined_variables) {
  for (v in c(endogenous_variables, exogenous_variables)) {
    pat <- paste0("^", v, "\\.L\\((\\d+)\\)$")
    m <- grep(pat, predetermined_variables, value = TRUE)
    if (length(m)) {
      lags <- -as.numeric(sub(pat, "\\1", m))
      for (i in seq_along(m)) {
        ts_data[[m[i]]] <- stats::lag(ts_data[[v]], lags[i])
      }
    }
  }

  ts_data
}
