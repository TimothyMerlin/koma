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
#' `2017`, for the shares of the annual sums, or a year and period, e.g.
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
    # Annual sums; a year with missing values is NA
    period_ts <- lapply(ts_data[ts_names], function(x) {
      tempdisagg::ta(x, conversion = "sum", to = "annual")
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

#' Chain-Link an Aggregate with Annual Overlap
#'
#' Builds the volume of an aggregate from the volumes of its components, the
#' way national accounts chain-link with annual overlap: within year \eqn{y},
#' the components are valued at the average prices of year \eqn{y-1}, and each
#' year is linked to the average of the previous year,
#' \deqn{\frac{x_{q,y}}{\bar{x}_{y-1}} = \frac{\sum_i s_{i,y-1}\,
#' x_{i,q,y}/\bar{x}_{i,y-1}}{\sum_i s_{i,y-1}},}
#' where \eqn{s_{i,y-1}} is the nominal share of component \eqn{i} in year
#' \eqn{y-1} and bars denote annual averages. Dividing by the sum of the shares
#' links each year exactly, also when the shares do not add up to 1.
#'
#' @param levels A named list of `stats::ts` objects with the volumes (real
#' levels) of the components, all of the same frequency.
#' @param shares A named list of annual `stats::ts` objects with the same names
#' as `levels`, with the nominal share of each year. Subtracted components
#' have negative shares.
#'
#' @return A `stats::ts` object with the volume of the aggregate. It starts in
#' the first year whose previous year has shares and complete levels of all
#' components; the average of that previous year is 100. Only its growth rates
#' are meaningful.
#' @keywords internal
chain_link <- function(levels, shares) {
  components <- names(levels)
  if (!setequal(components, names(shares))) {
    cli::cli_abort(c(
      "!" = "Components of {.arg levels} and {.arg shares} must match.",
      "x" = "{.arg levels}: {.val {components}}; {.arg shares}: {.val {names(shares)}}."
    ))
  }

  frequency <- stats::frequency(levels[[1]])
  start <- max(vapply(levels, function(x) stats::tsp(x)[1], numeric(1)))
  end <- min(vapply(levels, function(x) stats::tsp(x)[2], numeric(1)))
  periods <- round(seq(start, end, by = 1 / frequency) * frequency)
  year <- periods %/% frequency

  ratio_terms <- lapply(components, function(comp) {
    x <- as.numeric(stats::window(levels[[comp]], start, end))
    # Averages of complete years only
    annual_mean <- tapply(x, year, mean)
    annual_mean[tapply(x, year, length) < frequency] <- NA
    previous_mean <- as.numeric(annual_mean)[match(year - 1, names(annual_mean))]

    share <- share_of_year(shares[[comp]], year - 1)
    list(weighted = share * x / previous_mean, share = share)
  })
  weighted <- Reduce(`+`, lapply(ratio_terms, `[[`, "weighted"))
  share_sum <- Reduce(`+`, lapply(ratio_terms, `[[`, "share"))
  ratio <- weighted / share_sum

  valid <- which(!is.na(ratio))
  if (!length(valid)) {
    cli::cli_abort(c(
      "!" = "Cannot chain-link the aggregate.",
      "x" = "No year has shares and a complete previous year of all components."
    ))
  }
  if (any(diff(valid) != 1L)) {
    cli::cli_abort(c(
      "!" = "Cannot chain-link the aggregate.",
      "x" = "Shares or component levels are missing within the linked years."
    ))
  }
  ratio <- ratio[valid]
  year <- year[valid]

  # The average of each year is the base of the next one.
  annual_ratio <- tapply(ratio, year, mean)
  base <- 100 * cumprod(c(1, annual_ratio[-length(annual_ratio)]))
  names(base) <- names(annual_ratio)

  first <- periods[valid[1]]
  stats::ts(
    unname(base[as.character(year)] * ratio),
    start = c(first %/% frequency, first %% frequency + 1),
    frequency = frequency
  )
}

#' Weights of a Chain-Linked Aggregate
#'
#' Computes the weights that aggregate the growth rates of the components to
#' the growth rate of the aggregate chain-linked by [chain_link()]. For period
#' \eqn{h} in year \eqn{y},
#' \deqn{w_{i,h} = \frac{s_{i,y-1}}{\sum_j s_{j,y-1}}
#' \frac{x_{i,h-1}/\bar{x}_{i,y-1}}{x_{h-1}/\bar{x}_{y-1}},}
#' where \eqn{x} is the chain-linked aggregate. The simple growth rates then
#' satisfy
#' \deqn{g_h = \sum_i w_{i,h}\, g_{i,h} + \Big(\sum_i w_{i,h} - 1\Big).}
#' Within a year the weights add up to 1. In the first quarter of a year, the
#' previous quarter is valued at the prices of year \eqn{y-2}, and the second
#' term links it to the prices of year \eqn{y-1}.
#'
#' The weights for period \eqn{h} only use the levels up to \eqn{h-1}, so the
#' last weights are for the period after the levels.
#'
#' @inheritParams chain_link
#'
#' @return A named list of `stats::ts` objects with the weights of the
#' components. They start in the first year with a complete previous year of
#' the chain-linked aggregate.
#' @keywords internal
chain_link_weights <- function(levels, shares) {
  aggregate <- chain_link(levels, shares)
  frequency <- stats::frequency(aggregate)
  # The weights for period h use the levels of h - 1.
  start <- stats::tsp(aggregate)[1]
  end <- stats::tsp(aggregate)[2]
  year <- floor(stats::time(aggregate) + 1 / frequency + 1e-8)

  previous_aggregate <- as.numeric(aggregate) /
    complete_year_means(aggregate)[as.character(year - 1)]
  share_sum <- Reduce(`+`, lapply(shares, share_of_year, year - 1))

  weights <- lapply(names(levels), function(comp) {
    x <- stats::window(levels[[comp]], start, end)
    previous_component <- as.numeric(x) /
      complete_year_means(levels[[comp]])[as.character(year - 1)]
    share_of_year(shares[[comp]], year - 1) / share_sum *
      previous_component / previous_aggregate
  })
  names(weights) <- names(levels)

  valid <- which(!is.na(Reduce(`+`, weights)))
  if (!length(valid)) {
    cli::cli_abort(c(
      "!" = "Cannot compute chain-link weights.",
      "x" = "No period has shares and a complete previous year of the aggregate."
    ))
  }
  first <- round(start * frequency) + valid[1]
  lapply(weights, function(w) {
    stats::ts(unname(w[valid]),
      start = c(first %/% frequency, first %% frequency + 1),
      frequency = frequency
    )
  })
}

#' Prices of the Base Year
#'
#' In the forecast, the components are valued at the prices of the last
#' observed year, the base year \eqn{b}. The price of component \eqn{i} is
#' \eqn{p_i = s_{i,b}/\bar{x}_{i,b}}, proportional to its deflator in the base
#' year, where \eqn{s_{i,b}} is its nominal share and \eqn{\bar{x}_{i,b}} its
#' average volume.
#'
#' @inheritParams chain_link
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
#' [chain_link_weights()].
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

#' Annual Nominal Shares from Weight Expressions
#'
#' Evaluates the weight expressions of an identity on the annual sums of the
#' series in them, e.g. `n_a/n_y` gives the nominal share of `a` in each year.
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
    conversion = "sum", to = "annual"
  )
  lapply(expressions, evaluate_weight, annual)
}

#' Start of Time-Varying Identity Weights
#'
#' Computes the weights of an identity's components for the first forecast
#' period, with the prices of the base year: the last year in which the
#' nominal shares and the real components are complete. If the data ends in
#' the fourth quarter of the base year, the first forecast period links the
#' prices of the year before to those of the base year, see
#' [chain_link_weights()]. Otherwise the weights are [fixed_price_weights()].
#'
#' The weights add up to the sum of the base year's shares, as the weights of
#' the "last" scheme, except for a link, which adds `shift` to the
#' aggregate's growth.
#'
#' @param ts_data A named list of `koma_ts` with the components and the series
#' in the weight expressions, up to the period before the forecast.
#' @param iden An identity of `sys_eq$identities` with `expressions`.
#' @param forecast_start The first forecast period as a number, e.g. `1994`
#' for the first quarter of 1994.
#'
#' @return A list with `weights`, the weights of the components, `shift`, the
#' constant added to the aggregate's rate in the first forecast period, and
#' `base_year`.
#' @keywords internal
chain_link_start <- function(ts_data, iden, forecast_start) {
  components <- names(iden$expressions)
  no_conversion <- components[vapply(components, function(comp) {
    identical(attr(ts_data[[comp]], "method"), "none")
  }, logical(1))]
  if (length(no_conversion)) {
    cli::cli_abort(c(
      "!" = "Time-varying weights need the levels of the components.",
      "x" = "{.val {no_conversion}} {?has/have} method {.val none}."
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

  link <- abs(previous - (base_year + (frequency - 1) / frequency)) < 1e-8
  if (link) {
    weights <- chain_link_weights(levels, shares)
    weights <- vapply(weights, function(w) as.numeric(utils::tail(w, 1)), numeric(1))
    shift <- 100 * share_total * (sum(weights) - 1)
  } else {
    last_levels <- vapply(levels, function(x) as.numeric(utils::tail(x, 1)), numeric(1))
    weights <- fixed_price_weights(last_levels, base_prices(levels, shares, base_year))
    shift <- 0
  }

  list(weights = share_total * weights, shift = shift, base_year = base_year)
}

#' Growth Factors of a Rate Series
#'
#' Computes \eqn{1 + g_h = x_h / x_{h-1}} from the levels that [level()] gives
#' for the rates, so that the conversion of the series' method is used,
#' including a custom one.
#'
#' @param rates A `koma_ts` with `series_type = "rate"`.
#'
#' @return A `stats::ts` with the growth factors, on the periods of `rates`.
#' `NA` where the previous level is not available.
#' @keywords internal
growth_factors <- function(rates) {
  if (identical(attr(rates, "method"), "none")) {
    cli::cli_abort(c(
      "!" = "Cannot compute growth factors of a series with method {.val none}.",
      "i" = "Its rates have no conversion to levels."
    ))
  }
  levels <- level(rates)
  factors <- levels / stats::lag(levels, -1)
  out <- as.numeric(factors)[match(
    round(stats::time(rates) * stats::frequency(rates)),
    round(stats::time(factors) * stats::frequency(factors))
  )]
  stats::ts(out, start = stats::start(rates), frequency = stats::frequency(rates))
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
