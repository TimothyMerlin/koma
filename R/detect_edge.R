#' Detect the edge of a list of time series
#'
#' Identify either the earliest end or the latest start date across a set of
#' `ts` objects, relative to given `start` and `end` bounds.
#'
#' @param ts_data Named list of `ts` objects to inspect.
#' @param start Numeric lower bound for trimming each series.
#' @param end   Numeric upper bound for trimming each series.
#' @param direction Character; either `"end"` (default) to find the earliest
#'   end date, or `"start"` to find the latest start date.
#'
#' @return A list with components:
#'   - `date`: The detected edge date (numeric).
#'   - `variable_names`: Names of the series achieving that edge.
#'
#' @keywords internal
detect_edge <- function(ts_data, start, end, direction = c("end", "start")) {
  direction <- match.arg(direction)

  edges <- vapply(ts_data, function(x) {
    w <- suppressWarnings(window(x, start = start, end = end))
    stats::tsp(na.omit(w))[if (direction == "end") 2 else 1]
  }, numeric(1))

  bad_idx <- if (direction == "end") {
    which(end <= vapply(ts_data, function(x) stats::tsp(x)[1], 1))
  } else {
    which(start >= vapply(ts_data, function(x) stats::tsp(x)[2], 1))
  }
  if (length(bad_idx) > 0) {
    cli::cli_abort(c(
      "x" = "{direction} date out of range for: ",
      ">" = paste(names(ts_data)[bad_idx], collapse = ", ")
    ), call = rlang::caller_env())
  }

  extreme <- if (direction == "end") min(edges) else max(edges)
  vars <- names(edges)[edges == extreme]

  if ((direction == "end" && end <= extreme) ||
    (direction == "start" && start >= extreme)) {
    list(
      date = if (direction == "end") end else start,
      variable_names = names(ts_data)
    )
  } else {
    list(date = extreme, variable_names = vars)
  }
}
