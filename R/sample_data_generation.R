#' Generate Data for the Simultaneous Equations Model
#'
#' It creates a sample of data where the relationship between the variables
#' is determined by the defined parameters.
#'
#' @description Compute Y and X matrix (data)
#' Use reduced form of the model to compute Y
#' \eqn{Y = X B \Gamma^{-1} + U \Gamma^{-1}}
#'
#' @param sample_size The number of observations.
#' @param sample_start A vector of format \code{c(YEAR, QUARTER)} representing
#  the start date of the sample.
#' @param burnin The number of observations to discard to mitigate the
#' dependency on starting values.
#' @param gamma_matrix A matrix that defines the relationships between the
#' variables.
#' @param beta_matrix A matrix that defines the coefficients of the equation.
#' @param sigma_matrix A matrix that defines the standard deviation of the
#' error terms.
#' @param endogenous_variables A vector of endogenous variables.
#' @param exogenous_variables A vector of exogenous variables.
#' @param predetermined_variables A vector of predetermined variables.
#'
#' @return A list containing three elements:
#' * y_matrix: The dependent variable matrix with newly generated data.
#' * x_matrix: The matrix of exogenous and predetermined variables with
#' newly generated data.
#' * ts_data: A list containing time series in growth rates.
#'
#' @export
generate_sample_data <- function(sample_size, sample_start, burnin,
                                 gamma_matrix, beta_matrix,
                                 sigma_matrix, endogenous_variables,
                                 exogenous_variables,
                                 predetermined_variables) {
  total_exogenous_variables <- c(
    predetermined_variables,
    exogenous_variables
  )

  total_exogenous_variables <- c("constant", total_exogenous_variables)

  # Initialize matrices
  x_matrix <- matrix(0, sample_size + burnin, length(total_exogenous_variables))
  y_matrix <- matrix(0, sample_size + burnin, length(endogenous_variables))
  colnames(x_matrix) <- total_exogenous_variables
  colnames(y_matrix) <- endogenous_variables
  predetermined_matrix <- matrix(
    0,
    sample_size + burnin,
    length(predetermined_variables)
  )
  colnames(predetermined_matrix) <- predetermined_variables

  n <- ncol(y_matrix)
  max_lag <- get_max_lag(predetermined_variables)

  predetermined_matrix <- matrix(
    0,
    sample_size + burnin + max_lag,
    length(predetermined_variables)
  )
  colnames(predetermined_matrix) <- predetermined_variables

  for (tx in 1:(sample_size + burnin)) {
    exogenous_values <- multivariate_norm(
      n = 1,
      matrix(0, length(exogenous_variables), 1),
      diag(length(exogenous_variables))
    )

    x_matrix[tx, ] <- c(
      1,
      predetermined_matrix[tx, ],
      exogenous_values
    )

    y_matrix[tx, ] <-
      x_matrix[tx, ] %*% beta_matrix %*% solve(gamma_matrix) +
      t(multivariate_norm(n = 1, matrix(0, n, 1), sigma_matrix)) %*%
      solve(gamma_matrix)

    predetermined_matrix <- get_lagged_values(
      predetermined_matrix,
      y_matrix,
      tx
    )
  }

  # Remove initial observations from y_matrix and x_matrix to mitigate
  # dependency on starting values.
  y_matrix <- stats::ts(
    y_matrix[(burnin + 1):(sample_size + burnin), , drop = FALSE],
    start = sample_start,
    frequency = 4
  )
  x_matrix <- stats::ts(
    x_matrix[(burnin + 1):(sample_size + burnin), , drop = FALSE],
    start = sample_start,
    frequency = 4
  )

  # Construct tslist from x and y matrix of simulated data
  tslist_growth_x <- lapply(
    colnames(x_matrix),
    function(x) {
      stats::ts(
        x_matrix[, x],
        frequency = 4, start = sample_start
      )
    }
  )

  tslist_growth_y <- lapply(
    colnames(y_matrix),
    function(x) {
      stats::ts(
        y_matrix[, x],
        frequency = 4, start = sample_start
      )
    }
  )

  ts_data <- c(tslist_growth_x, tslist_growth_y)
  names(ts_data) <- c(
    colnames(x_matrix),
    colnames(y_matrix)
  )

  # remove lagged variables and constant
  ts_data <- ts_data[!names(ts_data) %in% predetermined_variables]
  ts_data["constant"] <- NULL

  list(
    y_matrix = y_matrix,
    x_matrix = x_matrix,
    ts_data = ts_data
  )
}

#' Extract Lagged Values from Y Matrix
#'
#' This function populates the matrix containing the predetermined
#' variables, i.e. the lagged variables, with the lagged values
#' contained in the Y matrix.
#'
#' @param predetermined_matrix The predetermined matrix to be populated with
#' lagged values.
#' @param y_matrix The y matrix containing the original values.
#' @param tx The time index for which the lagged values should be obtained.
#'
#' @return The updated predetermined matrix with lagged values.
#' @keywords internal
get_lagged_values <- function(predetermined_matrix, y_matrix, tx) {
  variable_names <- colnames(predetermined_matrix)

  for (w in seq_len(ncol(predetermined_matrix))) {
    lag_number <- detect_lag(variable_names[w])

    var <- variable_names[w]
    if (grepl("\\.L\\(\\d+\\)$", var)) {
      pattern <- "^(.*)(?=\\.L\\(\\d+\\)$)"
      var <- regmatches(var, regexpr(pattern, var, perl = TRUE))
    }
    column_in_y_matrix <- which(colnames(y_matrix) == var)

    predetermined_matrix[tx + lag_number, w] <- y_matrix[tx, column_in_y_matrix]
  }
  predetermined_matrix
}
