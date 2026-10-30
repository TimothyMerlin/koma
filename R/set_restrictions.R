#' Set Restrictions for Observed Variables in Time Series Data
#'
#' This function generates a list of restrictions for specified variables in a
#' time series data frame, based on their values between specified start and
#' end dates.
#'
#' @param ts_data A list containing the time series data.
#' @param variables_to_restrict A vector of strings specifying the names of the
#' variables in `ts_data` that should be restricted.
#' @param start The starting date for the restrictions.
#' @param end The ending date for the restrictions.
#'
#' @return A named list of restrictions, where the names are the names of the
#' variables in `variables_to_restrict`. Each element is a list containing
#' `horizon`, a sequence from 1 to the length of `value_at_date`, and `value`,
#' the truncated values of the variable between `start` and
#' `end`.
#' @keywords internal
set_restrictions <- function(ts_data, variables_to_restrict,
                             start, end) {
  restrictions <- lapply(
    variables_to_restrict,
    function(variable_name, ts_data) {
      value_at_date <- suppressWarnings(stats::window(ts_data[[variable_name]],
        start = start, end = end
      ))

      horizons <- seq_len(length(value_at_date))

      variable_restriction <- list(
        horizon = horizons,
        value = value_at_date[horizons]
      )
      return(variable_restriction)
    },
    ts_data
  )
  restrictions <- stats::setNames(restrictions, variables_to_restrict)

  return(restrictions)
}

#' Find Restrictions on Identities Determined by Other Restrictions
#'
#' An identity is determined in a period if all its components are known
#' there: restricted, exogenous, lagged from a period that is known, or
#' identities determined themselves. A known identity with a single unknown
#' component determines that component. A restriction on a determined identity
#' adds no information, and together with the others it makes the
#' restrictions singular. Restricted identities that are not determined are
#' known as well and can determine further identities; they are added one at
#' a time, so that of two identities that determine each other only one is
#' kept.
#'
#' @param restrictions Named list of restrictions with `horizon` and `value`.
#' @param sys_eq A system of equations (see [system_of_equations()]).
#'
#' @return A named list with, for each identity whose restriction is
#' determined in some period, the horizons of these periods.
#' @keywords internal
find_determined_identities <- function(restrictions, sys_eq) {
  identities <- sys_eq$identities
  restricted_identities <- intersect(names(restrictions), names(identities))
  if (!length(restricted_identities)) {
    return(list())
  }

  horizons <- unlist(lapply(restrictions, `[[`, "horizon"))
  lag_pattern <- "^(.+)\\.L\\(([0-9]+)\\)$"
  known <- vector("list", max(horizons))
  determined <- list()

  for (h in seq_along(known)) {
    restricted <- names(restrictions)[vapply(
      restrictions, function(x) h %in% x$horizon, logical(1)
    )]
    is_known <- function(component) {
      if (component %in% known[[h]]) {
        return(TRUE)
      }
      if (grepl(lag_pattern, component)) {
        variable <- sub(lag_pattern, "\\1", component)
        lag <- as.integer(sub(lag_pattern, "\\2", component))
        # periods before the first horizon are observed
        return(h - lag < 1 || variable %in% known[[h - lag]] ||
          !variable %in% sys_eq$endogenous_variables)
      }
      !component %in% sys_eq$endogenous_variables
    }
    unknown_components <- function(identity) {
      components <- names(identities[[identity]]$components)
      components[!vapply(components, is_known, logical(1))]
    }
    # adds the identities determined by the known variables, and the one
    # unknown component of a known identity
    close_known <- function() {
      repeat {
        candidates <- setdiff(names(identities), known[[h]])
        new <- candidates[vapply(candidates, function(identity) {
          !length(unknown_components(identity))
        }, logical(1))]
        for (identity in intersect(known[[h]], names(identities))) {
          unknown <- unknown_components(identity)
          if (length(unknown) == 1 && unknown %in% sys_eq$endogenous_variables) {
            new <- c(new, unknown)
          }
        }
        new <- unique(new)
        if (!length(new)) {
          return(invisible())
        }
        # restricted variables are known, so only restricted identities can
        # be new
        for (identity in intersect(new, restricted)) {
          determined[[identity]] <<- c(determined[[identity]], h)
        }
        known[[h]] <<- c(known[[h]], new)
      }
    }

    known[[h]] <- setdiff(restricted, names(identities))
    close_known()
    for (identity in intersect(restricted, names(identities))) {
      if (!identity %in% known[[h]]) {
        known[[h]] <- c(known[[h]], identity)
        close_known()
      }
    }
  }

  determined
}

#' Drop Restrictions on Determined Identities
#'
#' Drops the restrictions on identities in the periods in which their
#' components determine them, see [find_determined_identities()]. Used for the
#' restrictions that fill the ragged edge with observed values: with an
#' identity and all its components observed, the restrictions would otherwise
#' be singular.
#'
#' @inheritParams find_determined_identities
#'
#' @return The restrictions without those on determined identities.
#' @keywords internal
drop_determined_identities <- function(restrictions, sys_eq) {
  determined <- find_determined_identities(restrictions, sys_eq)
  for (identity in names(determined)) {
    keep <- !restrictions[[identity]]$horizon %in% determined[[identity]]
    restrictions[[identity]]$horizon <- restrictions[[identity]]$horizon[keep]
    restrictions[[identity]]$value <- restrictions[[identity]]$value[keep]
    if (!any(keep)) {
      restrictions[[identity]] <- NULL
    }
  }
  restrictions
}

#' Check Restrictions on Identities and All Their Components
#'
#' Restrictions of the user on an identity in a period in which its
#' components determine it are redundant or contradict each other, and make
#' the conditional forecast singular. They fail with an error that names the
#' identity, its components and the horizons.
#'
#' @inheritParams find_determined_identities
#' @param offset Number of periods before the forecast that `restrictions`
#' start with, see [fill_in_draws()]. The error reports the horizons of the
#' forecast.
#' @param call The environment from which the error is called.
#'
#' @return `restrictions`, invisibly.
#' @keywords internal
check_identity_restrictions <- function(restrictions, sys_eq, offset = 0,
                                        call = rlang::caller_env()) {
  determined <- find_determined_identities(restrictions, sys_eq)
  if (!length(determined)) {
    return(invisible(restrictions))
  }
  bullets <- vapply(names(determined), function(identity) {
    components <- intersect(
      names(sys_eq$identities[[identity]]$components),
      sys_eq$endogenous_variables
    )
    cli::format_inline(
      "{.val {identity}} and its components {.val {components}} at horizon {determined[[identity]] - offset}"
    )
  }, character(1))
  cli::cli_abort(c(
    "x" = "Restrictions on identities that their components determine:",
    stats::setNames(bullets, rep("*", length(bullets))),
    "i" = "Drop the restriction of the identity or of one of its components."
  ), call = call)
}

#' Convert Dated Entries to Forecast Horizons
#'
#' Restrictions and adjustments can be given as a `ts`, whose time index is
#' the forecast period, or as a list with `date` and `value`. Both are
#' converted to the list with `horizon` and `value` that the forecast uses;
#' other fields of a dated list, e.g. `decay`, are kept. Entries without a
#' date are returned unchanged and checked by the caller, e.g. by
#' [validate_restrictions()].
#'
#' A `date` is a numeric date (e.g. `2025.25`), a `c(year, period)` vector
#' (e.g. `c(2025, 2)`), or a list of these. A numeric vector of length two
#' whose second element is a whole number between 1 and `frequency` is read
#' as `c(year, period)`; otherwise each element is a numeric date. Missing
#' values in a `ts` mean that the period is not restricted.
#'
#' @param entries `NULL` or a named list of restrictions or adjustments.
#' @param forecast_start Start of the forecast, as numeric date or
#' `c(year, period)`.
#' @param horizon Integer forecast horizon.
#' @param frequency Number of periods per year.
#' @param what Kind of entry named in error messages, e.g. `"restriction"` or
#' `"adjustment"`.
#' @param call The environment from which the error is called.
#'
#' @return The entries with `horizon` instead of `date` for every dated entry.
#' @keywords internal
dates_to_horizon <- function(entries, forecast_start, horizon, frequency,
                             what = "restriction",
                             call = rlang::caller_env()) {
  if (!is.list(entries) || is.data.frame(entries)) {
    return(entries)
  }

  forecast_start <- dates_to_num(forecast_start, frequency)
  forecast_end <- iterate_n_periods(forecast_start, horizon - 1, frequency)
  period_str <- function(x) dates_to_str(num_to_dates(x, frequency), frequency)

  for (i in seq_along(entries)) {
    variable <- if (is.null(names(entries))) i else names(entries)[i]
    restriction <- entries[[i]]
    abort <- function(message) {
      cli::cli_abort(c(
        "!" = "Invalid {what} for {.val {variable}}:",
        "x" = message
      ), call = call, .envir = parent.frame())
    }

    if (stats::is.ts(restriction)) {
      if (is_ets(restriction) && identical(attr(restriction, "series_type"), "level")) {
        abort("Values must be in rates, not levels. Use {.fn rate} first.")
      }
      if (NCOL(restriction) != 1L) {
        abort("Must be a single time series.")
      }
      if (stats::frequency(restriction) != frequency) {
        abort(paste(
          "The frequency of the time series ({stats::frequency(restriction)})",
          "does not match the frequency of the data ({frequency})."
        ))
      }
      value <- as.numeric(restriction)
      observed <- !is.na(value)
      if (!any(observed)) {
        abort("The time series has no values.")
      }
      restriction <- list(
        date = as.numeric(stats::time(restriction))[observed],
        value = value[observed]
      )
    } else if (!is.list(restriction) || !"date" %in% names(restriction)) {
      next
    }

    if ("horizon" %in% names(restriction)) {
      abort("Use either {.field date} or {.field horizon}, not both.")
    }

    date <- restriction[["date"]]
    if (is.list(date)) {
      if (!all(vapply(date, function(d) is.numeric(d) && length(d) %in% 1:2, logical(1)))) {
        abort("Every {.field date} must be a numeric date or {.code c(year, period)}.")
      }
      date <- vapply(date, dates_to_num, numeric(1), frequency = frequency)
    } else if (!is.numeric(date) || length(date) == 0L) {
      abort("{.field date} must be numeric dates or {.code c(year, period)}.")
    } else if (length(date) == 2L && isTRUE(date[2] == round(date[2])) &&
      isTRUE(date[2] >= 1 && date[2] <= frequency)) {
      date <- dates_to_num(date, frequency)
    }
    if (any(!is.finite(date))) {
      abort("{.field date} must not contain missing or infinite values.")
    }
    if (length(date) != length(restriction[["value"]])) {
      abort("{.field date} and {.field value} must have the same length.")
    }

    hx <- (date - forecast_start) * frequency + 1
    off_grid <- abs(hx - round(hx)) > 1e-6
    if (any(off_grid)) {
      abort("{.field date} {.val {date[off_grid]}} does not fall on a period of the data.")
    }
    hx <- as.integer(round(hx))
    outside <- hx < 1L | hx > horizon
    if (any(outside)) {
      outside_str <- vapply(date[outside], period_str, character(1))
      abort(paste(
        "{.val {outside_str}} must lie between {period_str(forecast_start)}",
        "and {period_str(forecast_end)}."
      ))
    }

    restriction[["date"]] <- NULL
    entries[[i]] <- c(list(horizon = hx), restriction)
  }

  entries
}
