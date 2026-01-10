## Gibbs sampler specifications

#' Set Gibbs Sampler Specifications
#'
#' This class updates the Gibbs sampler specifications in the global
#' environment variable `the$gibbs_sampler`.
#'
#' @inheritSection get_default_gibbs_spec Gibbs Sampler Specifications
#'
#' @param ndraws Integer specifying the number of Gibbs sampler draws.
#' @param burnin_ratio Numeric specifying the ratio for the burn-in period.
#' @param nstore Integer specifying the frequency of stored draws.
#' @param tau Numeric tuning parameter for enforcing an acceptance rate.
#' @param ... Unused arguments that get discarded.
#'
#' @return Invisible NULL. The function updates `the$gibbs_sampler` in place.
#' @keywords internal
set_gibbs_spec <- function(
    ndraws = NULL, burnin_ratio = NULL, nstore = NULL, tau = NULL, ...) {
  default <- get_default_gibbs_spec()
  ndraws <- if (is.null(ndraws)) default$ndraws else ndraws
  burnin_ratio <- if (is.null(burnin_ratio)) default$burnin_ratio else burnin_ratio
  nstore <- if (is.null(nstore)) default$nstore else nstore
  tau <- if (is.null(tau)) default$tau else tau

  new_gibbs_spec(ndraws, burnin_ratio, nstore, tau)
}

#' Default Gibbs Specfications
#'
#' @section Gibbs Sampler Specifications:
#' - `ndraws`: Integer specifying the number of Gibbs sampler draws. Default is
#'    2000.
#' - `burnin_ratio`: Numeric specifying the ratio for the burn-in period.
#'    Default is 0.5.
#' - `nstore`: Integer specifying the frequency of stored draws. Default is 1.
#' - `tau`: Numeric tuning parameter for enforcing an acceptance rate.
#'    Default is 1.1.
#' @keywords internal
get_default_gibbs_spec <- function() {
  structure(list(
    ndraws = 2000L,
    burnin_ratio = 0.5,
    nstore = 1,
    tau = 1.1
  ))
}

new_gibbs_spec <- function(ndraws, burnin_ratio, nstore, tau, ...) {
  rlang::caller_env()

  validate_integerish(ndraws, "ndraws")
  validate_integerish(nstore, "nstore")

  ## check that each arg is length 1 or same as the longest
  lens <- lengths(list(ndraws, burnin_ratio, nstore, tau))
  maxlen <- max(lens)
  if (!all(lens %in% c(1, maxlen))) {
    cli::cli_abort("All args must have length 1 or {maxlen}.")
  }

  if (length(ndraws) == 1) ndraws <- rep(ndraws, maxlen)
  if (length(burnin_ratio) == 1) burnin_ratio <- rep(burnin_ratio, maxlen)
  if (length(nstore) == 1) nstore <- rep(nstore, maxlen)
  if (length(tau) == 1) tau <- rep(tau, maxlen)

  burnin <- ndraws * burnin_ratio
  nsave <- (ndraws - burnin) / nstore

  structure(
    list(
      ndraws       = as.integer(ndraws),
      burnin_ratio = burnin_ratio,
      nstore       = as.integer(nstore),
      tau          = tau,
      burnin       = as.integer(burnin),
      nsave        = as.integer(nsave)
    ),
    class = "gibbs_spec"
  )
}

validate_integerish <- function(x, name) {
  if (!is.numeric(x) || any(is.na(x)) || any(x %% 1 != 0) || any(x < 0)) {
    cli::cli_abort("{name} must be a non-negative integer",
      call = rlang::caller_env()
    )
  }
}


#' Set Gibbs Settings
#'
#' @param settings List with system-wide/global settings.
#' @param equation_settings List with equation specific settings.
#' @param envir Environment to create the gibbs_sampler in. Defaults to "the".
#' @keywords internal
set_gibbs_settings <- function(settings, equation_settings, envir = the) {
  default <- get_default_gibbs_spec()

  gibbs_settings <- build_settings(
    default = default,
    settings = settings,
    equation_settings = equation_settings
  )

  if (!is.null(settings)) {
    default <- utils::modifyList(default, settings)
  }
  global_settings <- do.call(set_gibbs_spec, default)

  # Check if gibbs_settings is NULL or empty
  if (!is.null(gibbs_settings) && length(gibbs_settings) > 0) {
    specs <- lapply(gibbs_settings, function(opt) {
      do.call(set_gibbs_spec, opt)
    })
    names(specs) <- names(gibbs_settings)
    assign("gibbs_sampler", specs, envir = envir)
  }

  print_gibbs_settings(global_settings)

  invisible(NULL)
}

get_gibbs_settings <- function(equation = NULL, envir = the) {
  out <- get("gibbs_sampler",
    envir = envir,
    inherits = FALSE
  )
  if (!is.null(equation)) out <- out[[equation]]

  out
}

#' Display Current Gibbs Sampler Specifications
#'
#' This function prints the current settings of the Gibbs sampler in a
#' readable format. It includes information such as the number of draws,
#' burn-in ratio, number of saved draws, and tuning parameter tau.
#'
#' @return Prints a formatted message with the current Gibbs sampler
#' settings. This function does not return any values.
#'
#' @keywords internal
print_gibbs_settings <- function(global_settings) {
  rlang::caller_env()

  eq_gibbs_settings <- get("gibbs_sampler", envir = the)

  # locally removes margins between headers, list and paragraphs
  cli::cli_div(
    theme = list(
      h2 = list("margin-top" = 0.7, "margin-bottom" = 0.7)
    )
  )

  cli::cli_h1("Gibbs Sampler Settings")
  cli::cli_h2("System Wide Settings")
  cli::cli_ul()
  cli::cli_li("Number of draws ({.var ndraws}): {global_settings$ndraws}")
  cli::cli_li("Burn-in ratio ({.var burnin_ratio}): {global_settings$burnin_ratio}")
  cli::cli_li("Burn-in ({.var burnin}): {global_settings$burnin}")
  cli::cli_li("Store frequency ({.var nstore}): {global_settings$nstore}")
  cli::cli_li("Number of saved draws ({.var nsave}): {global_settings$nsave}")
  cli::cli_li("Tau ({.var tau}): {global_settings$tau}")
  cli::cli_end() # closes the ul
  cli::cli_text("\n")

  params <- names(global_settings)

  for (nm in names(eq_gibbs_settings)) {
    diffs <- stats::setNames(
      lapply(params, function(p) {
        val <- eq_gibbs_settings[[nm]][[p]]
        if (!identical(val, global_settings[[p]])) val
      }),
      params
    )
    diffs <- Filter(Negate(is.null), diffs)

    if (length(diffs)) {
      cli::cli_h2("Equation: {.field {nm}}")
      for (p in names(diffs)) {
        cli::cli_li("{.var {p}}: {diffs[[p]]}")
      }
      cli::cli_end()
    }
  }

  cli::cli_end() # closes the <h1>
  cli::cli_end() # closes the cli_div()
}
