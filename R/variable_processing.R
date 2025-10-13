#' Detect Lag Number from Variable Name
#'
#' This function takes a variable name as input and extracts the lag number
#' from it.
#' The lag number is assumed to be a positive integer preceded by the letter
#' 'L'.
#'
#' @param variable_name A character string representing the variable name.
#'
#' @return An integer representing the detected lag number from the variable
#'  name. Returns NA if no lag detected.
#' @keywords internal
detect_lag <- function(variable_name) {
  if (!is.character(variable_name) || length(variable_name) != 1) {
    stop("The input should be a character string with the variable names.")
  }

  pattern <- "(?<=\\.L\\()[^)]*(?=\\))"
  lag_text <- regmatches(variable_name, regexpr(pattern, variable_name, perl = TRUE))
  out <- tryCatch(
    {
      eval(parse(text = lag_text))
    },
    error = function(e) {
      NA
    }
  )
  is_whole_number <- function(x) {
    is.numeric(x) && all(abs(x - round(x)) < .Machine$double.eps^0.5)
  }

  if (all(is.na(out))) {
    return(NA)
  } else if (is.null(out) || any(out < 1) || !any(sapply(out, is_whole_number))) {
    cli::cli_abort(
      "Detected lag number is {out}. Lag number should be a positive integer."
    )
  }

  sapply(out, as.integer)
}

#' Determine the maximum lag of variables
#'
#' This function calculates the maximum lag among a set of variables.
#'
#' @param variable_names A character vector specifying the names of
#' the variables to analyze.
#'
#' @return The maximum lag among the variables. If there are no lags detected
#' NA is returned.
#' @keywords internal
get_max_lag <- function(variable_names) {
  if (is.null(variable_names)) {
    return(NA)
  }

  lags <- sapply(variable_names, detect_lag)
  maxlag <- ifelse(all(is.na(lags)) | length(lags) == 0, NA, max(lags, na.rm = TRUE))

  as.integer(maxlag)
}
