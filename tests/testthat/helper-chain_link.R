# Reference for the time-varying identity weights: the volume of an aggregate
# chain-linked with annual overlap, the way national accounts do it. Within
# year y the components are valued at the average prices of year y-1, and
# each year is linked to the average of the previous year,
#   x_{q,y} / mean(x_{y-1}) =
#     sum_i s_{i,y-1} x_{i,q,y} / mean(x_{i,y-1}) / sum_i s_{i,y-1},
# where s_{i,y-1} is the nominal share of component i in year y-1. Dividing
# by the sum of the shares links each year exactly, also when the shares do
# not add up to 1.
#
# `levels` is a named list of `ts` with the volumes of the components,
# `shares` a named list of annual `ts` with their nominal shares (negative for
# subtracted components). The result starts in the first year whose previous
# year has shares and complete levels of all components; the average of that
# previous year is 100.
chain_link <- function(levels, shares) {
  components <- names(levels)
  if (!setequal(components, names(shares))) {
    stop("Components of `levels` and `shares` must match.")
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
  if (!length(valid) || any(diff(valid) != 1L)) {
    stop("Cannot chain-link the aggregate.")
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
