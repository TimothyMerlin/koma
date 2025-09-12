#' @keywords internal
setup_global_progress_handler <- function() {
  # Set cli as default if no global handler has been set and R is used in
  # interactive mode
  if (!progressr::handlers(global = NA)) {
    set_global_handler <- interactive() && !isCallingHandler() &&
      # Avoid setting global handler if inside a testthat test
      # (testthat is only a suggested dependency)
      (!rlang::is_installed("testthat") || !testthat::is_testing())

    # Avoid setting global handler if inside a tryCatch or withCallingHandlers
    if (set_global_handler) {
      progressr::handlers(global = TRUE)
      cli::cli_inform(c(
        "i" = "Progress bar enabled.",
        "!" = "No global progress handlers were active.
        The `cli` handler has been enabled for this R session.",
        "i" = "For more information, see the `progressr` vignette:
          `progressr-intro`.",
        " " = "You can access it with `vignette('progressr-intro',
          package = 'progressr')`."
      ))
      return(TRUE)
    }
  }
  FALSE
}

#' @keywords internal
set_progress_handler <- function(operation = "estimation") {
  # Only set operation-specific handler if global handlers are enabled
  if (progressr::handlers(global = NA)) {
    # Get operation-specific configuration
    config <- get_progress_config(operation)

    progressr::handlers(
      progressr::handler_cli(
        format = config$format,
        format_done = config$format_done,
        format_failed = config$format_failed,
        clear = FALSE
      )
    )
    return(TRUE)
  }
  FALSE
}

#' @keywords internal
get_default_progress_configs <- function() {
  list(
    estimation = list(
      format = paste0(
        "{cli::pb_spin} {cli::pb_bar} {cli::pb_percent} | ",
        "elapsed {.timestamp {cli::pb_elapsed}} | ",
        "{.field {cli::pb_status}} | ",
        "{cli::pb_current}/{cli::pb_total}"
      ),
      format_done = paste0(
        "{.alert-success SEM estimation completed in {.timestamp {cli::pb_elapsed}}}."
      ),
      format_failed = "{.alert-danger Processing failed {.timestamp {cli::pb_elapsed}}}."
    ),
    forecasting = list(
      format = paste0(
        "{cli::pb_spin} {cli::pb_bar} {cli::pb_percent} | ",
        "elapsed {.timestamp {cli::pb_elapsed}} | ",
        "{cli::pb_current}/{cli::pb_total} draws"
      ),
      format_done = paste0(
        "{.alert-success Forecasting completed in {.timestamp {cli::pb_elapsed}}}."
      ),
      format_failed = "{.alert-danger Forecasting failed {.timestamp {cli::pb_elapsed}}}."
    )
  )
}

#' @keywords internal
get_progress_config <- function(operation) {
  configs <- get_default_progress_configs()
  if (!is.null(configs[[operation]])) {
    configs[[operation]]
  } else {
    configs[["estimation"]]
  }
}

# Helper function to check if we're within a calling handler
isCallingHandler <- function() {
  # Check the call stack to see if we're inside tryCatch or withCallingHandlers
  stack <- sys.calls()
  any(vapply(stack, function(call) {
    is.call(call) && (deparse(call[[1]]) %in% c(
      "tryCatch", "withCallingHandlers"
    ))
  }, logical(1)))
}
