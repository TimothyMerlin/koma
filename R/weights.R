#' Get System of Equations Weights
#'
#' Calculates the weights of identities from the data and updates the
#' identities with them. Each weight is the share of one period: by default
#' the last year that is complete for all series of the identity.
#'
#' @param ts_data A named list of `stats::ts` objects. This list should contain
#' the time series data for both the aggregate values and their components.
#' @param identities A named list of identities, each containing components
#' and their respective weights.
#' @param period The period of the shares, `dates$weights`: a year, e.g.
#' `2017`, for the shares of the annual averages, or a year and period, e.g.
#' `c(2017, 3)`, for the shares of that period. If `NULL`, the last year that
#' is complete for all series of the identity.
#'
#' @return The `identities` list with the calculated weights. Identities with
#' weights computed from data keep the weight expressions in `expressions`,
#' named by component.
#' @keywords internal
get_seq_weights <- function(ts_data, identities, period = NULL) {
  weights <- c()

  for (ix in seq_along(identities)) {
    is_na <- is.na(suppressWarnings(as.numeric(identities[ix][[1]]$weights)))

    if (any(is_na)) {
      weights <- c(weights, calculate_eq_weights(ts_data, identities[ix], period))
      iden <- identities[[ix]]
      expressions <- iden$weights[unlist(iden$components)]
      names(expressions) <- names(iden$components)
      from_data <- is.na(suppressWarnings(as.numeric(expressions)))
      identities[[ix]]$expressions <- expressions[from_data]
    }
  }

  update_identity_weights(weights, identities)
}

#' Calculate Weights for Identity Equations
#'
#' Computes the weights of an identity from the series in its weight
#' expressions, as the shares of one period.
#'
#' @param ts_data A named list of `stats::ts` objects. It should contain the
#' time series data for both the aggregate value and its components. Ensure that
#' `ts_data` contains the time series listed in `iden`.
#' @param iden A named list with one identity, as in `sys_eq$identities`.
#' @inheritParams get_seq_weights
#'
#' @return A list with the calculated weights for the given aggregate value and
#' its components. The top-level name of the list is the aggregate value, and
#' its sub-level names correspond to the component names.
#' @keywords internal
calculate_eq_weights <- function(ts_data, iden, period = NULL) {
  agg_value <- names(iden)
  components <- unlist(iden[[1]]$weights)
  names(components) <- names(iden[[1]]$components)

  # Filter components that are not numeric or cannot be converted to numeric
  components <- components[is.na(suppressWarnings(as.numeric(components)))]

  ts_names <- weight_series(components)

  missing_ts <- setdiff(ts_names, names(ts_data))
  if (length(missing_ts)) {
    stop(
      "The following time series are missing from 'ts_data': ",
      paste(missing_ts, collapse = ", "), "."
    )
  }

  frequency <- stats::frequency(ts_data[[ts_names[1]]])
  period <- validate_weights_period(period, frequency)

  if (length(period) == 2L) {
    period_ts <- ts_data[ts_names]
  } else {
    # Annual averages; a year with missing values is NA
    period_ts <- lapply(ts_data[ts_names], function(x) {
      tempdisagg::ta(x, conversion = "average", to = "annual")
    })
  }

  if (is.null(period)) {
    # The last year that is complete for all series
    period <- min(vapply(period_ts, function(x) {
      max(stats::time(x)[!is.na(x)])
    }, numeric(1)))
  }

  # The values of all series in the period
  period_num <- dates_to_num(period, frequency = frequency)
  period_values <- lapply(period_ts, function(x) {
    as.numeric(x)[abs(stats::time(x) - period_num) < 1e-8]
  })
  if (any(lengths(period_values) != 1L) || anyNA(unlist(period_values))) {
    cli::cli_abort(c(
      "x" = "Weights for {.field {agg_value}} need the data of
        {dates_to_str(period, frequency)}.",
      "i" = "Not all of {.val {ts_names}} are complete in that period."
    ))
  }

  weights <- list()
  weights[[agg_value]] <- lapply(components, evaluate_weight, period_values)

  weights
}

#' Series in Weight Expressions
#'
#' @param expressions Weight expressions, e.g. `"n_a/n_y"` or
#' `"-0.5*n_m/n_y"`.
#'
#' @return A character vector with the names of the series in the
#' expressions, without numbers.
#' @keywords internal
weight_series <- function(expressions) {
  as.character(unique(unlist(lapply(expressions, function(expression) {
    all.vars(str2lang(expression))
  }))))
}

#' Evaluate a Weight Expression
#'
#' @param expression A weight expression, e.g. `"n_a/n_y"`.
#' @param values A named list with the values of the series in `expression`.
#'
#' @return The value of the expression.
#' @keywords internal
evaluate_weight <- function(expression, values) {
  eval(str2lang(expression), values, baseenv())
}

#' Validate the Period of Identity Weights
#'
#' @inheritParams get_seq_weights
#' @param frequency Frequency of the time series.
#'
#' @return `period` as a double vector, or `NULL`.
#' @keywords internal
validate_weights_period <- function(period, frequency) {
  if (is.null(period)) {
    return(NULL)
  }
  if (!is.numeric(period) || !length(period) %in% c(1L, 2L) ||
    any(!is.finite(period)) ||
    (length(period) == 2L && !period[2] %in% seq_len(frequency))
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$weights}:",
      "x" = "Use a year, e.g. {.code 2017}, or a year and period, e.g.
        {.code c(2017, 3)}."
    ))
  }
  as.numeric(period)
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

#' Prices of the Base Year
#'
#' In the forecast, the components are valued at the prices of the last
#' observed year, the base year \eqn{b}. The price of component \eqn{i} is
#' \eqn{p_i = s_{i,b}/\bar{x}_{i,b}}, proportional to its deflator in the base
#' year, where \eqn{s_{i,b}} is its nominal share and \eqn{\bar{x}_{i,b}} its
#' average volume.
#'
#' @param levels A named list of `stats::ts` objects with the volumes (real
#' levels) of the components, all of the same frequency.
#' @param shares A named list of annual `stats::ts` objects with the same names
#' as `levels`, with the nominal share of each year. Subtracted components
#' have negative shares.
#' @param base_year The base year.
#'
#' @return A named numeric vector with the prices of the components.
#' @keywords internal
base_prices <- function(levels, shares, base_year) {
  prices <- vapply(names(levels), function(comp) {
    share_of_year(shares[[comp]], base_year) /
      complete_year_means(levels[[comp]])[as.character(base_year)]
  }, numeric(1))
  if (anyNA(prices)) {
    cli::cli_abort(c(
      "!" = "Cannot compute the prices of base year {base_year}.",
      "x" = "Shares or complete levels are missing for
        {.val {names(prices)[is.na(prices)]}}."
    ))
  }
  prices
}

#' Weights at Fixed Prices
#'
#' The weight of component \eqn{i} in period \eqn{h} is its share of the
#' previous period at fixed prices,
#' \deqn{w_{i,h} = \frac{p_i\, x_{i,h-1}}{\sum_j p_j\, x_{j,h-1}}.}
#' With the prices of the base year, these weights aggregate the components'
#' simple growth rates to the growth of the chain-linked aggregate in all
#' periods after the first quarter after the base year. That quarter links the
#' prices of the base year to those of the year before; see
#' [link_quarter_weights()].
#'
#' @param previous_levels A named numeric vector with the levels of the
#' components in the previous period.
#' @param prices A named numeric vector with the prices of the components, as
#' returned by [base_prices()].
#'
#' @return A named numeric vector with the weights.
#' @keywords internal
fixed_price_weights <- function(previous_levels, prices) {
  values <- prices * previous_levels[names(prices)]
  values / sum(values)
}

#' Weights of the Link Quarter
#'
#' The first quarter after the base year \eqn{b} links the prices \eqn{p'} of
#' year \eqn{b-1}, at which the fourth quarter of \eqn{b} is valued, to the
#' prices \eqn{p} of year \eqn{b},
#' \deqn{w_{i} = \frac{p_i\, x_{i,h-1}}{\sum_j p_j\, \bar{x}_{j,b}}
#' \frac{\sum_j p'_j\, \bar{x}_{j,b}}{\sum_j p'_j\, x_{j,h-1}},}
#' where \eqn{x_{h-1}} are the levels of the fourth quarter and bars denote
#' averages of year \eqn{b}. This gives the growth of the aggregate
#' chain-linked with annual overlap. The weights add up to 1 only if the
#' relative prices do not change; the difference links the price bases.
#'
#' @param last_levels A named numeric vector with the levels of the
#' components in the fourth quarter of the base year.
#' @param mean_levels A named numeric vector with the average levels of the
#' components in the base year.
#' @param prices,previous_prices Named numeric vectors with the prices of the
#' components in the base year and the year before, as returned by
#' [base_prices()].
#'
#' @return A named numeric vector with the weights.
#' @keywords internal
link_quarter_weights <- function(last_levels, mean_levels, prices,
                                 previous_prices) {
  prices * last_levels / sum(prices * mean_levels) *
    sum(previous_prices * mean_levels) / sum(previous_prices * last_levels)
}

#' Annual Nominal Shares from Weight Expressions
#'
#' Evaluates the weight expressions of an identity on the annual averages of
#' the series in them, e.g. `n_a/n_y` gives the nominal share of `a` in each
#' year.
#'
#' @param ts_data A named list of `koma_ts` with the series in the
#' expressions, as levels or rates.
#' @param expressions A named list of weight expressions, as kept by
#' [get_seq_weights()] in the identity.
#'
#' @return A named list of annual `stats::ts` objects with the share of each
#' year. Years that are not complete are `NA`.
#' @keywords internal
annual_shares <- function(ts_data, expressions) {
  ts_names <- weight_series(expressions)
  annual <- lapply(stats::as.ts(level(ts_data[ts_names])), tempdisagg::ta,
    conversion = "average", to = "annual"
  )
  lapply(expressions, evaluate_weight, annual)
}

#' Start of Time-Varying Identity Weights
#'
#' Computes the weights of an identity's components for the first forecast
#' period, with the prices of the base year \eqn{b}: the last year in which the
#' nominal shares and the real components are complete. The weights are
#' [fixed_price_weights()], unless the data ends in the fourth quarter of
#' the base year. Then the first forecast period links the prices of the year
#' before to those of the base year, see [link_quarter_weights()].
#'
#' The weights add up to the sum of the base year's shares, as the weights of
#' the "last" scheme, except in a link quarter, where `link` adds the change
#' of the price base to the aggregate's growth.
#'
#' The components need method `"percentage"` or `"diff_log"`, so that their
#' growth factors follow from their rates, and must not be lagged.
#'
#' @param ts_data A named list of `koma_ts` with the components and the series
#' in the weight expressions, up to the period before the forecast.
#' @param iden An identity of `sys_eq$identities` with `expressions`.
#' @param forecast_start The first forecast period as a number, e.g. `1994`
#' for the first quarter of 1994.
#'
#' @return A list with `weights`, the weights of the components, `link`, the
#' constant added to the aggregate's rate in the first forecast period,
#' `base_year`, and `share_total`, the sum of the base year's shares.
#' @keywords internal
chain_link_start <- function(ts_data, iden, forecast_start) {
  components <- names(iden$expressions)
  lagged <- components[grepl("\\.L\\(\\d+\\)$", components)]
  if (length(lagged)) {
    cli::cli_abort(c(
      "!" = "Time-varying weights need components of the same period.",
      "x" = "{.val {lagged}} {?is/are} lagged."
    ))
  }
  unsupported <- components[!vapply(components, function(comp) {
    method <- attr(ts_data[[comp]], "method")
    is.character(method) && method %in% c("percentage", "diff_log")
  }, logical(1))]
  if (length(unsupported)) {
    cli::cli_abort(c(
      "!" = "Time-varying weights need components with method
        {.val percentage} or {.val diff_log}.",
      "x" = "{.val {unsupported}} {?has/have} another method."
    ))
  }

  frequency <- stats::frequency(ts_data[[components[1]]])
  previous <- forecast_start - 1 / frequency
  levels <- lapply(ts_data[components], function(x) {
    stats::window(stats::as.ts(level(x)), end = previous)
  })
  shares <- annual_shares(ts_data, iden$expressions)

  complete_years <- lapply(components, function(comp) {
    means <- complete_year_means(levels[[comp]])
    years <- as.numeric(names(means)[!is.na(means)])
    share_years <- round(stats::time(shares[[comp]]))[!is.na(shares[[comp]])]
    intersect(years, share_years)
  })
  base_year <- max(Reduce(intersect, complete_years), -Inf)
  if (!is.finite(base_year)) {
    cli::cli_abort(c(
      "!" = "Cannot compute time-varying weights.",
      "x" = "No year is complete for all components and their nominal shares."
    ))
  }
  share_total <- sum(vapply(shares, share_of_year, numeric(1), base_year))

  prices <- base_prices(levels, shares, base_year)
  last_levels <- vapply(levels, function(x) as.numeric(utils::tail(x, 1)), numeric(1))
  link_quarter <- abs(previous - (base_year + (frequency - 1) / frequency)) < 1e-8
  if (link_quarter) {
    mean_levels <- vapply(levels, function(x) {
      complete_year_means(x)[[as.character(base_year)]]
    }, numeric(1))
    weights <- link_quarter_weights(
      last_levels, mean_levels, prices,
      base_prices(levels, shares, base_year - 1)
    )
    link <- 100 * share_total * (sum(weights) - 1)
  } else {
    weights <- fixed_price_weights(last_levels, prices)
    link <- 0
  }

  list(
    weights = share_total * weights, link = link, base_year = base_year,
    share_total = share_total
  )
}

#' Growth Factor of a Rate
#'
#' Computes \eqn{1 + g_h = x_h / x_{h-1}} from the rate of period \eqn{h},
#' with the conversion of [level()] for the method.
#'
#' @param rate The rate of period \eqn{h}.
#' @param method `"percentage"` or `"diff_log"`.
#'
#' @return The growth factor.
#' @keywords internal
growth_factor <- function(rate, method) {
  switch(method,
    percentage = 1 + rate / 100,
    diff_log = exp(rate / 100)
  )
}

#' Set Up Time-Varying Identity Weights for a Forecast
#'
#' Prepares the chain-linked weights of each identity with weights computed
#' from data: the weights of the first forecast period from
#' [chain_link_start()], and the methods of the components, with which
#' [growth_factor()] converts their forecast rates for [next_chain_weights()].
#'
#' @param ts_data A named list of `koma_ts` with the components and the series
#' in the weight expressions, up to the period before the forecast.
#' @param identities The identities of `sys_eq`, with the `expressions` kept by
#' [get_seq_weights()].
#' @inheritParams chain_link_start
#'
#' @return A named list with one element per identity with weights from data,
#' as returned by [chain_link_start()], with `methods`, the methods of the
#' components.
#' @keywords internal
setup_varying_weights <- function(ts_data, identities, forecast_start) {
  identities <- Filter(function(iden) length(iden$expressions) > 0, identities)
  lapply(identities, function(iden) {
    # chain_link_start() checks the methods of the components first.
    start <- chain_link_start(ts_data, iden, forecast_start)
    components <- names(iden$expressions)
    methods <- vapply(ts_data[components], attr, character(1), "method")
    c(start, list(methods = methods))
  })
}

#' Weights of the Next Forecast Period
#'
#' Continues the chain-linked weights at the prices of the base year:
#' \deqn{w_{i,h+1} = c\,\frac{w_{i,h}(1 + g_{i,h})}{\sum_j w_{j,h}(1 + g_{j,h})},}
#' where \eqn{1 + g_{i,h}} is the growth factor of component \eqn{i} in period
#' \eqn{h} and \eqn{c} the sum of the base year's shares.
#'
#' @param weights A named numeric vector with the weights of period \eqn{h}.
#' @param growth A numeric vector with the growth factors of period \eqn{h},
#' in the order of `weights`.
#' @param share_total The sum of the base year's shares.
#'
#' @return A named numeric vector with the weights of period \eqn{h + 1}.
#' @keywords internal
next_chain_weights <- function(weights, growth, share_total) {
  values <- weights * growth
  share_total * values / sum(values)
}

complete_year_means <- function(x) {
  frequency <- stats::frequency(x)
  year <- floor(stats::time(x) + 1e-8)
  means <- tapply(as.numeric(x), year, mean)
  means[tapply(as.numeric(x), year, length) < frequency] <- NA
  means
}

share_of_year <- function(share, year) {
  as.numeric(share)[match(year, round(stats::time(share)))]
}
