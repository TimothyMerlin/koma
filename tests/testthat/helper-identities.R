# Expects every draw of a forecast to satisfy the identities: each identity
# is recomputed from its components and weights, with the weights of each
# period for time-varying weights. Lagged components are taken from the draw
# and, before the forecast, from `result$y_matrix`; for a ragged edge filled
# in the draws, the filled periods are not in `result`, so lagged components
# cannot be checked there. Components that are not endogenous are taken from
# `result$x_matrix` and must not be lagged.
expect_identities_hold <- function(result, identities, tolerance = 1e-8) {
  expect_gt(length(result$forecasts), 0)
  lag_pattern <- "\\.L\\(([0-9]+)\\)$"

  for (draw in result$forecasts) {
    frequency <- stats::frequency(draw)
    history <- stats::window(result$y_matrix,
      end = stats::tsp(draw)[1] - 1 / frequency
    )
    path <- rbind(
      as.matrix(history),
      as.matrix(draw)[, colnames(history), drop = FALSE]
    )
    rows <- nrow(history) + seq_len(nrow(draw))

    value <- function(component) {
      variable <- sub(lag_pattern, "", component)
      lag <- if (grepl(lag_pattern, component)) {
        as.integer(sub(paste0(".*", lag_pattern), "\\1", component))
      } else {
        0
      }
      if (variable %in% colnames(path)) {
        return(path[rows - lag, variable])
      }
      stopifnot(lag == 0)
      as.matrix(result$x_matrix)[, variable]
    }

    for (lhs in names(identities)) {
      identity <- identities[[lhs]]
      varying <- attr(draw, "weights")[[lhs]]
      rhs <- if (is.null(varying)) 0 else as.numeric(varying[, "link"])
      for (component in names(identity$components)) {
        weight <- if (!is.null(varying) && component %in% colnames(varying)) {
          as.numeric(varying[, component])
        } else {
          identity$weights[[identity$components[[component]]]]
        }
        rhs <- rhs + weight * value(component)
      }
      expect_equal(unname(path[rows, lhs]), unname(rhs),
        tolerance = tolerance,
        label = paste0("identity ", lhs)
      )
    }
  }
}
