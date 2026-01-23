#' Construct Balanced Data
#'
#' @param ts_data Time series data.
#' @param endogenous_variables Endogenous variables.
#' @param total_exogenous_variables Exogenous and / or predetermined variables
#' to include in x matrix.
#' @param start Start date for truncation.
#' @param end End date for truncation.
#' @inheritParams forecast_draw
#'
#' @return A list containing the truncated time series data, y_matrix, x_matrix
#' the number of observations, the date of last observation and the frequency.
#' @keywords internal
construct_balanced_data <- function(ts_data, endogenous_variables,
                                    total_exogenous_variables,
                                    start, end, state = NULL) {
  variables_to_include <- intersect(
    names(ts_data), c(endogenous_variables, total_exogenous_variables)
  )
  ts_data <- ts_data[variables_to_include]

  start_edge <- detect_edge(ts_data, start, end, direction = "start")
  end_edge <- detect_edge(ts_data, start, end, direction = "end")

  suppressWarnings(
    truncated_ts_data <- stats::window(as_mets(ts_data),
      start = start_edge$date,
      end   = end_edge$date
    )
  )
  freq <- stats::frequency(truncated_ts_data)

  if (start_edge$date > start && is.null(state$warning_issued)) {
    cli::cli_warn(c(
      "Estimation start moved to {.val {dates_to_str(num_to_dates(start_edge$date, freq), freq)}}.",
      "i" = "Periods removed due to missing or lag-induced NAs."
    ))
  }
  ##### Construct Y and X matrix
  y_matrix <- as.matrix(truncated_ts_data[, endogenous_variables])

  number_of_observations <- dim(y_matrix)[1] # length of time series

  constant_matrix <- stats::ts(
    matrix(1, number_of_observations, 1,
      dimnames = list(NULL, "constant")
    ),
    start = stats::start(truncated_ts_data),
    end = stats::end(truncated_ts_data),
    frequency = freq
  )
  x_matrix_without_constant <- truncated_ts_data[, total_exogenous_variables[-1]]

  x_matrix <- as.matrix(cbind(constant_matrix, x_matrix_without_constant))
  colnames(x_matrix) <- total_exogenous_variables

  validate_matrices(x_matrix, y_matrix)

  list(
    truncated_ts_data = truncated_ts_data,
    y_matrix = y_matrix,
    x_matrix = x_matrix,
    number_of_observations = number_of_observations,
    edge = end_edge,
    freq = freq
  )
}

validate_matrices <- function(x_matrix, y_matrix) {
  call <- rlang::caller_env()

  bad <- list(
    x_matrix = colnames(x_matrix)[colSums(is.na(x_matrix)) > 0],
    y_matrix = colnames(y_matrix)[colSums(is.na(y_matrix)) > 0]
  )
  bad <- bad[lengths(bad) > 0]
  if (length(bad)) {
    msg <- c(glue::glue("Missing values in:"))
    for (nm in names(bad)) {
      msg <- c(
        msg,
        "*" = glue::glue("{nm}: {paste(bad[[nm]], collapse = ', ')}")
      )
    }
    cli::cli_abort(msg, call = call)
  }
  invisible(TRUE)
}
