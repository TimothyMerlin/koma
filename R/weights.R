#' Get System of Equations Weights
#'
#' Calculates the weights of identities from the data and updates the
#' identities with them. Each weight is the share of the last year that is
#' complete for all series of the identity.
#'
#' @param ts_data A named list of `stats::ts` objects. This list should contain
#' the time series data for both the aggregate values and their components.
#' @param identities A named list of identities, each containing components
#' and their respective weights.
#'
#' @return The `identities` list with the calculated weights.
#' @keywords internal
get_seq_weights <- function(ts_data, identities) {
  weights <- c()

  for (ix in seq_along(identities)) {
    is_na <- is.na(suppressWarnings(as.numeric(identities[ix][[1]]$weights)))

    if (any(is_na)) {
      weights <- c(weights, calculate_eq_weights(ts_data, identities[ix]))
    }
  }

  update_identity_weights(weights, identities)
}

#' Calculate Weights for Identity Equations
#'
#' Computes the weights of an identity from annual sums of the series in its
#' weight expressions. The weight for year \eqn{y} uses the data of year
#' \eqn{y-1}, so the series are labelled one year after the data. They end
#' with the last year that is complete for all series of the identity.
#'
#' @param ts_data A named list of `stats::ts` objects. It should contain the
#' time series data for both the aggregate value and its components. Ensure that
#' `ts_data` contains the time series listed in `iden`.
#' @param iden A named list with one identity, as in `sys_eq$identities`.
#'
#' @return A list of `stats::ts` objects containing the calculated weights for
#' the given aggregate value and its components. The top-level name of the
#' list is the aggregate value, and its sub-level names correspond to the
#' component names.
#' @keywords internal
calculate_eq_weights <- function(ts_data, iden) {
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

  # Annual sums of complete years, labelled with the year they weight
  annualized_ts <- lapply(ts_names, function(comp) {
    annual <- tempdisagg::ta(ts_data[[comp]], conversion = "sum", to = "annual")
    observed <- which(!is.na(annual))
    annual <- stats::window(annual, end = stats::time(annual)[max(observed)])
    stats::lag(annual, -1)
  })
  names(annualized_ts) <- ts_names

  # All weights come from the same years.
  common_end <- min(vapply(annualized_ts, function(x) stats::tsp(x)[2], numeric(1)))
  annualized_ts <- lapply(annualized_ts, stats::window, end = common_end)

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
#' \code{list(gdp = list(components = list(manufacturing = "theta_gamma6_4"),
#'                 weights = list(theta_gamma6_4 = NULL)))}
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
