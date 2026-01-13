#' Get System of Equations Weights
#'
#' Calculates dynamic weights for a system of equations and updates the
#' identities with these weights. The function processes a list of time
#' series data (`ts_data`), a list of identities (`identities`), and a set of
#' dates (`dates`) to compute and update weights dynamically.
#'
#' @param ts_data A named list of `stats::ts` objects. This list should contain
#' the time series data for both the aggregate values and their components.
#' @param identities A named list of identities, each containing components
#' and their respective weights. The list should include elements with
#' "lhs_weight" in their names.
#' @param dates A list with start and end dates for dynamic weight calculations.
#' The dates list should have a sub-list `dynamic_weights` with `start` and
#' `end` entries, each being a numeric representation of a date, it should be
#' in the format produced by `dates_to_num`.
#'
#' @return The function updates the `identities` list with the calculated
#' dynamic weights and returns nothing explicitly.
#' @keywords internal
get_seq_weights <- function(ts_data, identities, dates) {
  # Calculate dynamic weights
  weights <- c()

  for (ix in seq_along(identities)) {
    is_na <- is.na(suppressWarnings(as.numeric(identities[ix][[1]]$weights)))

    if (any(is_na)) {
      validate_dynamic_weights_dates(dates)
      dynamic_start <- dates_to_num(dates$dynamic_weights$start, frequency = 4)
      dynamic_end <- dates_to_num(dates$dynamic_weights$end, frequency = 4)
      out <- calculate_eq_weights(
        ts_data, identities[ix],
        dynamic_start,
        dynamic_end
      )
      weights <- c(weights, out)
    }
  }

  # Update identities with dynamic weights
  update_identity_weights(weights, identities)
}

validate_dynamic_weights_dates <- function(dates, frequency = 4) {
  if (is.null(dates$dynamic_weights) ||
    is.null(dates$dynamic_weights$start) ||
    is.null(dates$dynamic_weights$end) ||
    length(dates$dynamic_weights$start) == 0L ||
    length(dates$dynamic_weights$end) == 0L
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$dynamic_weights}:",
      "x" = "{.field start} and {.field end} must be provided"
    ))
  }
  if (!is.numeric(dates$dynamic_weights$start) ||
    !is.numeric(dates$dynamic_weights$end)
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$dynamic_weights}:",
      "x" = "{.field start} and {.field end} must be numeric"
    ))
  }
  if (!length(dates$dynamic_weights$start) %in% c(1L, 2L) ||
    !length(dates$dynamic_weights$end) %in% c(1L, 2L)
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$dynamic_weights}:",
      "x" = "{.field start} and {.field end} must be length 1 or 2"
    ))
  }
  if (anyNA(dates$dynamic_weights$start) ||
    anyNA(dates$dynamic_weights$end) ||
    any(!is.finite(dates$dynamic_weights$start), na.rm = TRUE) ||
    any(!is.finite(dates$dynamic_weights$end), na.rm = TRUE)
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$dynamic_weights}:",
      "x" = "{.field start} and {.field end} must be finite"
    ))
  }
  if (length(dates$dynamic_weights$start) == 2L &&
    (dates$dynamic_weights$start[2] < 1L ||
      dates$dynamic_weights$start[2] > frequency)
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$dynamic_weights}:",
      "x" = "{.field start} period must be between 1 and {frequency}"
    ))
  }
  if (length(dates$dynamic_weights$end) == 2L &&
    (dates$dynamic_weights$end[2] < 1L ||
      dates$dynamic_weights$end[2] > frequency)
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$dynamic_weights}:",
      "x" = "{.field end} period must be between 1 and {frequency}"
    ))
  }

  dynamic_start <- dates_to_num(dates$dynamic_weights$start, frequency = frequency)
  dynamic_end <- dates_to_num(dates$dynamic_weights$end, frequency = frequency)
  if (length(dynamic_start) != 1L || length(dynamic_end) != 1L) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$dynamic_weights}:",
      "x" = "{.field start} and {.field end} must be scalar dates"
    ))
  }
  if (dynamic_start > dynamic_end) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$dynamic_weights}:",
      "x" = "{.field start} must be before {.field end}"
    ))
  }

  invisible(NULL)
}

#' Calculate Dynamic Weights for Identity Equations
#'
#' Computes the weights required for identity equations using time series data.
#' The weights represent how components collectively sum up to or are related to
#' an aggregate value over the specified time period.
#'
#' @param ts_data A named list of `stats::ts` objects. It should contain the
#' time series data for both the aggregate value and its components. Ensure that
#' `ts_data` contains the time series listed in `iden`.
#' @param start A numeric value representing the start date for the time series
#' data subset. This value should be in the numeric format used by the function
#' `dates_to_num`.
#' @param end A numeric value representing the end date for the time series data
#' subset, in the same numeric format as `start`.
#'
#' @return A list of `stats::ts` objects containing the calculated weights for
#' the given aggregate value and its components. The top-level name of the
#' list is the aggregate value, and its sub-level names correspond to the
#' component names.
#' @keywords internal
calculate_eq_weights <- function(ts_data, iden, start, end) {
  agg_value <- names(iden)
  components <- unlist(iden[[1]]$weights)
  names(components) <- names(iden[[1]]$components)

  # Filter components that are not numeric or cannot be converted to numeric
  components <- components[is.na(suppressWarnings(as.numeric(components)))]

  # Split by mathematical operators
  ts_names <- unique(unlist(strsplit(
    components,
    "[+*/-]"
  )))
  ts_names <- ts_names[ts_names != ""]
  # Trim whitespace
  ts_names <- trimws(ts_names)

  ts_present <- ts_names %in% names(ts_data)
  if (!all(ts_present)) {
    # Check for negative weight indicators and update ts_names if needed
    missing_ts_names <- ts_names[!ts_present]

    if (any(grepl("^-", missing_ts_names))) {
      ts_names <- gsub("^-", "", ts_names)
      components <- gsub("^-", "", components)
      ts_present <- ts_names %in% names(ts_data)
    }

    if (!all(ts_present)) {
      missing_ts <- ts_names[!ts_present]
      stop(
        "The following time series are missing from 'ts_data': ",
        paste(missing_ts, collapse = ", "), "."
      )
    }
  }

  # annualize the all time series
  annualized_ts <- lapply(
    ts_names,
    function(comp, start, end) {
      aggregated_ts <- tempdisagg::ta(ts_data[[comp]],
        conversion = "sum", to = "annual"
      )
      # back shift times series by 1 year
      lagged_agg_ts <- stats::lag(aggregated_ts, -1)
      suppressWarnings(
        subset_lagged_agg_ts <- stats::window(
          lagged_agg_ts,
          start = c(floor(start)), end = c(floor(end))
        )
      )
      return(subset_lagged_agg_ts)
    },
    start = floor(start), end = floor(end)
  )
  names(annualized_ts) <- ts_names

  evaluate_component <- function(component, annualized_ts) {
    # Replace variables with annualized_ts$variable format
    expr <- gsub("(\\b[0-9a-zA-Z_]+\\b)", "annualized_ts$\\1", component)
    parsed_expr <- parse(text = expr)
    eval(parsed_expr)
  }

  # Calculate weights
  ts_weights <- list()

  # Apply evaluate_component to each component
  ts_weights[[agg_value]] <- lapply(
    components, evaluate_component, annualized_ts
  )

  ts_weights
}

#' Update Identity Weights
#'
#' Iteratively updates the weights in an identity list (`identities`) based on
#' the last value of the corresponding weights found in the `weights` list.
#'
#' @param weights A named list where each entry represents an equation (e.g.,
#' "gdp"). Each equation is itself a named list, containing variable names
#' (e.g., "manufacturing") and their dynamic weight values.
#'
#' Example:
#' \code{list(gdp = list(manufacturing = c(0.2, 0.3), service = c(0.7, 0.6)))}
#'
#' @param identities A named list, where each entry represents an equation
#' similar to `weights`. The equation has sublists "components" for variable
#' names and "weights" for identity weights.
#'
#' Example:
#' \code{list(gdp = list(components = list(manufacturing = "theta6_4"),
#'                 weights = list(theta6_4 = NULL)))}
#'
#' @return A named list containing the updated identity weights for each
#' equation and its variables, in the same structure as the input `identities`.
#' @keywords internal
update_identity_weights <- function(weights, identities) {
  for (ix in seq_along(weights)) {
    lhs <- names(weights[ix])
    rhs <- names(weights[[ix]])

    for (var in rhs) {
      weight <- weights[[ix]][[var]]
      if (!is.numeric(weight)) {
        stop(
          "In weight calculation for identity: ", identities[lhs], "\n",
          "Expected a numeric value, but got: '", weight, "'."
        )
      }
      last_weight <- weight[length(weight)]
      character_weight <- identities[[lhs]][["components"]][[var]]
      identities[[lhs]][["weights"]][[character_weight]] <- unname(last_weight)
    }
  }

  identities
}
