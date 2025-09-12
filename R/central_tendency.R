#' Get Central Tendency Value from a Named List
#'
#' This function takes a named list `quantiles` and a string `central_tendency`
#' indicating which central tendency measure (mean or median) to retrieve.
#'
#' @param quantiles A named list containing central tendency values.
#' @param central_tendency A string, either "mean" or "median".
#'
#' @return The central tendency value from the list `quantiles` based on the
#' `central_tendency` argument.
#' @keywords internal
get_central_tendency <- function(central_tendency, quantiles) {
  switch(central_tendency,
    mean = quantiles$q_mean,
    median = quantiles$q_50,
    cli::cli_abort(c(
      "Central tendency must be either {.val mean} or {.val median}.",
      "x" = "Provided argument was: {.val {central_tendency}}."
    ), call = rlang::caller_env())
  )
}
