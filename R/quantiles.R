#' Compute Quantiles of Coefficients from Estimations
#'
#' This function takes the results of an estimation process, typically stored
#' in a data matrix or array, and computes specified quantiles. It supports
#' 1D, 2D, and 3D data structures and makes necessary adjustments before
#' calculations. For multi-dimensional data, the function performs the
#' percentile computation across each column, retaining only the first
#' column if the data is 3D.
#'
#' @param data A list or array. If multi-dimensional, it should be either
#' 2D or 3D.
#' @param include_mean Logical. If TRUE, the mean of the coefficients will
#' also be computed and returned along with the quantiles. Default is FALSE.
#' @param probs A numerical vector specifying the probabilities for which
#' percentiles should be computed. Default is obtained from `get_quantiles()`.
#'
#' @return A named list of computed percentiles for each coefficient. The names
#' are prefixed with "q_" and suffixed with the percentile value multiplied by
#' 100 (e.g., "q_5", "q_50", "q_95"). If `include_mean` is TRUE, "q_mean" is
#' also included in the list.
#' @keywords internal
quantiles_from_estimates <- function(data, include_mean = FALSE,
                                     probs = NULL) {
  if (is.null(probs)) probs <- get_quantiles()

  names_q <- paste0("q_", 100 * probs)

  # convert data to matrix
  if (length(data[[1]]) == 1) {
    data <- t(as.matrix(simplify2array(data)))
  } else {
    data <- simplify2array(data)
  }
  # keep only the first column (relevant for omega because we are only
  # interested the variances)
  if (length(dim(data)) == 3) data <- data[, 1, ]

  # apply over rows (= 1)
  q_array <- apply(
    data, 1,
    stats::quantile,
    probs = probs,
    na.rm = TRUE
  )

  if (include_mean) {
    data_mean <- apply(data, 1, mean)
    q_array <- rbind(q_array, data_mean)
    names_q <- c(names_q, "q_mean")
  }

  out <- lapply(
    seq_len(nrow(q_array)),
    function(i) q_array[i, ]
  )
  out <- stats::setNames(out, names_q)

  out
}

#' Calculate Quantiles from Multiple Forecast Matrices
#'
#' This function calculates specified quantiles  for each variable-time
#' combination across all draws.
#'
#' @param forecasts A list of matrices. Each matrix should represent a forecasts
#' with the same dimensions (rows and columns) for comparison.
#' @param freq The frequency of the data.
#' @param probs A numeric vector specifying which quantiles to compute.
#' Default is c(0.25, 0.5, 0.75, 1).
#' @return A named list of matrices, where each matrix corresponds to an
#' original matrix in `forecasts`. Each output matrix has an additional
#' dimension corresponding to the computed quantiles, with dimnames
#' indicating the quantile levels (e.g., "q_25" for the 25th percentile).
#' @keywords internal
quantiles_from_forecasts <- function(forecasts, freq, probs = NULL) {
  if (is.null(probs)) probs <- get_quantiles()

  names_quantiles <- paste0("q_", 100 * probs)

  # time vector of first draw and first forecast type
  start_forecast <- stats::start(forecasts[[1]])

  # Initialize matrix to store quantiles
  quantile_matrix <- array(NA,
    dim = c(
      nrow(forecasts[[1]]),
      ncol(forecasts[[1]]),
      length(names_quantiles)
    ),
    dimnames = list(
      rownames(forecasts[[1]]),
      colnames(forecasts[[1]]),
      names_quantiles
    )
  )

  # Calculate quantiles per horizon h and variable
  for (h in seq_len(nrow(forecasts[[1]]))) {
    for (variable in seq_len(ncol(forecasts[[1]]))) {
      forecasts_for_quantile <- sapply(forecasts, function(x) x[h, variable])
      if (!any(is.na(forecasts_for_quantile))) {
        quantile_values <-
          stats::quantile(forecasts_for_quantile, probs = probs)
      } else {
        quantile_values <- rep(NA, length(probs))
      }
      quantile_matrix[h, variable, ] <- quantile_values
    }
  }

  # Initialize an empty list to store the time series
  out <- list()

  # Loop through the 3D matrix to create time series for each 2D slice
  for (i in seq_len(dim(quantile_matrix)[3])) {
    slice <- quantile_matrix[, , i]
    slice_ts <- stats::ts(
      slice,
      start = start_forecast, frequency = freq
    )
    quantile_name <- dimnames(quantile_matrix)[[3]][i]
    out[[quantile_name]] <- slice_ts
  }

  out
}

#' Parse Quantile Names into Probabilities
#'
#' @param name A quantile name like "q_5" or a numeric string.
#'
#' @return A probability in (0, 1] or `NA_real_` on failure.
#' @keywords internal
parse_quantile_name <- function(name) {
  if (!grepl("^q_", name)) {
    value <- suppressWarnings(as.numeric(name))
    if (is.na(value)) {
      return(NA_real_)
    }
    return(if (value > 1) value / 100 else value)
  }
  value <- suppressWarnings(as.numeric(sub("^q_", "", name)))
  if (is.na(value)) {
    return(NA_real_)
  }
  if (value > 1) value / 100 else value
}

#' Normalize Quantile Probabilities
#'
#' @param fan_quantiles Numeric probabilities in (0, 1] or percentages in
#'   \code{[0, 100]}.
#'
#' @return A numeric vector of probabilities in (0, 1].
#' @keywords internal
normalize_quantile_probs <- function(fan_quantiles) {
  if (is.null(fan_quantiles)) {
    return(NULL)
  }
  if (!is.numeric(fan_quantiles)) {
    cli::cli_abort("fan_quantiles must be numeric probabilities.")
  }
  probs <- as.numeric(fan_quantiles)
  probs <- probs[!is.na(probs)]
  if (!length(probs)) {
    return(NULL)
  }
  if (any(probs > 1)) {
    if (any(probs > 100)) {
      cli::cli_abort("fan_quantiles must be in (0, 1] or [0, 100].")
    }
    cli::cli_warn("Interpreting fan_quantiles > 1 as percent values.")
    probs[probs > 1] <- probs[probs > 1] / 100
  }
  probs
}

#' Build Quantile Names from Probabilities
#'
#' @param probs Numeric probabilities.
#'
#' @return A character vector of quantile names.
#' @keywords internal
quantile_names_from_probs <- function(probs) {
  paste0("q_", 100 * probs)
}
