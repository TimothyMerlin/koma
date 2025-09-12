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
#' the `future::plan()` function. For more details, see the future package
#' documentation:
#' https://cran.r-project.org/web/packages/future/future.pdf
#'
#' @return List of estimates for the endogenous variables.
#' @keywords internal
estimate_sem <- function(sys_eq, y_matrix, x_matrix, eq_jx = NULL) {
  stopifnot(inherits(sys_eq, "koma_seq"))

  stochastic_equations <- sys_eq$stochastic_equations
  character_gamma_matrix <- sys_eq$character_gamma_matrix
  character_beta_matrix <- sys_eq$character_beta_matrix
  priors <- sys_eq$priors

  `%dofuture%` <- doFuture::`%dofuture%`

  if (is.null(eq_jx)) {
    eq_jx <- seq_along(stochastic_equations)
  } else {
    # Verify eq_jx is a vector with numerics
    stopifnot(is.vector(eq_jx), all(sapply(eq_jx, is.numeric)))
  }

  safe_draw_parameters <- purrr::safely(function(eq_jx) {
    if (length(priors[[eq_jx]]) == 0) {
      draw_parameters_j(
        y_matrix, x_matrix, character_gamma_matrix,
        character_beta_matrix, eq_jx
      )
    } else {
      draw_parameters_j_informative(
        y_matrix, x_matrix, character_gamma_matrix,
        character_beta_matrix, eq_jx, priors
      )
    }
  }, quiet = FALSE)

  set_progress_handler(operation = "estimation")
  p <- progressr::progressor(
    steps = length(stochastic_equations)
  )

  suppressPackageStartupMessages(
    estimates <- foreach::foreach(
      eq_jx = eq_jx,
      .options.future = list(
        packages = c("koma"),
        globals = c(
          "p", # Export the progressor function
          "stochastic_equations", # Export the stochastic equations
          "safe_draw_parameters" # Export the safe_draw_parameters function
        ),
        seed = TRUE # Enable future seed
      )
    ) %dofuture% {
      p(
        amount = 0,
        message = stochastic_equations[eq_jx]
      )
      out <- safe_draw_parameters(eq_jx)
      p(amount = 1)
      out
    }
  )

  names(estimates) <- stochastic_equations[eq_jx]
  out <- purrr::map(estimates, "result")

  if (all(sapply(out, is.null))) {
    cli::cli_abort("All equations failed to estimate.")
    return(NULL)
  }
  out
}
