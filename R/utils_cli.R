check_dots_used <- function(..., call = rlang::caller_env()) {
  rlang::check_dots_used(error = function(cnd) {
    # Extract the names of the unused arguments
    unused_args <- names(list(...))

    # This is a warning rather than an error because it is only triggered on
    # exit.
    # The user may have waited for computations, and it's important they still
    # have access to the results even if some arguments were unused.
    cli::cli_warn(c(
      "!" = "Arguments in `...` must be used.",
      "i" = "Problematic argument: {.val {unused_args}}",
      "i" = "Did you misspell an argument name?"
    ))
  })
}

fr <- function(x) format(x, justify = "right")
fl <- function(x) format(x, justify = "left")
