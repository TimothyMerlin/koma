#' Estimate Parameters in a System of Equations
#'
#' This function estimates parameters in a given system of equations using
#' either a single thread or parallel computing.
#'
#' @param y_matrix A \eqn{(T \times n)} matrix \eqn{Y}, where \eqn{T} is the
#' number of observations and \eqn{n} the number of equations, i.e. endogenous
#' variables.
#' @param x_matrix A \eqn{(T \times k)} matrix \eqn{X} of observations on
#' \eqn{k} exogenous variables.
#' @param eq_jx A numeric vector indicating the indices of the endogenous
#' equations \eqn{j} to be estimated. If NULL, all endogenous equations are
#' estimated. The vector should contain positive integers corresponding to the
#' positions of the equations within the `endogenous_variables` list.
#' @inheritParams estimate
#'
#' @details This function provides the option for parallel computing through
#' the `future::plan()` function. For more details, see the
#' \href{https://CRAN.R-project.org/package=future}{future package documentation}.
#'
#' @return List of estimates for the endogenous variables.
#' @keywords internal
estimate_sem <- function(sys_eq, y_matrix, x_matrix, eq_jx = NULL) {
  stopifnot(inherits(sys_eq, "koma_seq"))

  if (Sys.info()[["sysname"]] == "Darwin" &&
      inherits(future::plan(), "multicore")) {
    cli::cli_abort(c(
      "!" = "{.code future::multicore} is not safe on macOS.",
      "i" = "Apple's Accelerate framework (used by {.fn eigen}) is not fork-safe and will cause a segfault.",
      "i" = "Switch to {.code future::plan(\"future::multisession\", workers = workers)} before calling {.fn estimate}."
    ))
  }

  stochastic_equations <- sys_eq$stochastic_equations
  character_gamma_matrix <- sys_eq$character_gamma_matrix
  character_beta_matrix <- sys_eq$character_beta_matrix
  priors <- sys_eq$priors

  `%dofuture%` <- doFuture::`%dofuture%`

  stochastic_positions <- which(
    !sys_eq$endogenous_variables %in% names(sys_eq$identities)
  )

  if (is.null(eq_jx)) {
    eq_jx <- seq_along(stochastic_equations)
  } else {
    # Verify eq_jx is a vector with numerics
    stopifnot(is.vector(eq_jx), all(sapply(eq_jx, is.numeric)))
  }
  col_positions <- stochastic_positions[eq_jx]
  equation_names <- colnames(character_gamma_matrix)

  gibbs_settings <- get_gibbs_settings()

  # Progress is reported per Gibbs draw, so the total is the number of draws
  # of all equations estimated here.
  set_progress_handler(operation = "estimation")
  p <- progressr::progressor(
    steps = sum(vapply(
      equation_names[col_positions],
      function(eq) gibbs_settings[[eq]]$ndraws,
      numeric(1)
    ))
  )

  # Evaluate lazy arguments before the closure is shipped to future workers.
  # An unevaluated promise keeps the caller's environment alive, so the whole
  # estimate() frame (ts_data, previous estimates, ...) would be serialized to
  # every worker.
  force(y_matrix)
  force(x_matrix)

  safe_draw_parameters <- purrr::safely(function(eq_jx) {
    gibbs_sampler <- gibbs_settings[[colnames(character_gamma_matrix)[eq_jx]]]
    progress <- function(amount) p(amount = amount)

    if (length(priors[[eq_jx]]) == 0) {
      draw_parameters_j(
        y_matrix, x_matrix, character_gamma_matrix,
        character_beta_matrix, eq_jx, gibbs_sampler, progress
      )
    } else {
      draw_parameters_j_informative(
        y_matrix, x_matrix, character_gamma_matrix,
        character_beta_matrix, eq_jx, gibbs_sampler, priors, progress
      )
    }
  }, quiet = TRUE)

  globals_to_export <- c(
    "p",
    "safe_draw_parameters",
    "equation_names"
  )

  suppressPackageStartupMessages(
    estimates <- foreach::foreach(
      eq_jx = col_positions,
      .options.future = list(
        packages = c("koma"),
        globals = globals_to_export,
        seed = TRUE # Enable future seed
      )
    ) %dofuture% {
      p(
        amount = 0,
        message = equation_names[eq_jx]
      )
      safe_draw_parameters(eq_jx)
    }
  )

  names(estimates) <- equation_names[col_positions]

  errors <- purrr::compact(purrr::map(estimates, "error"))
  if (length(errors) > 0) {
    messages <- paste0(names(errors), ": ", purrr::map_chr(errors, conditionMessage))
    # Escape braces so cli does not interpolate the error messages
    messages <- gsub("\\{", "{{", messages)
    messages <- gsub("\\}", "}}", messages)
    cli::cli_abort(c(
      "!" = "Estimation failed for {length(errors)} equation{?s}:",
      stats::setNames(messages, rep("x", length(messages)))
    ))
  }

  purrr::map(estimates, "result")
}
