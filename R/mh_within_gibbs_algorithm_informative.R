#' Draw Parameters for equation j
#'
#' `draw_parameters_j_informative` simulates from the posterior of the model's
#' parameters for each equation \eqn{j} separately using an informative prior.
#' The posterior is simulated using a Metropolis-within-Gibbs sampling
#' procedure.
#'
#' The sampler works as follows:
#' 1. Initialize sampler: \eqn{delta_{\gamma}^{(0)}} and \eqn{\Omega^{(0)}}
#' 2. Conditional on \eqn{delta_{\gamma}^{(w-1)}} and \eqn{\Omega^{(w-1)}}
#' and the data draw \eqn{delta_{\theta}}.
#' 3. Conditional on \eqn{delta_{\gamma}^{(w-1)}}, \eqn{delta_{\theta}^{(w)}}
#' draw \eqn{\Omega^{(w)}}.
#' 4. Conditional on \eqn{delta_{\theta}^{(w)}}, \eqn{Omega^{(w)}} draw
#' \eqn{\delta_{\gamma}^{(w)}}.
#' 5. Go back to step 2.
#' @param y_matrix A \eqn{(T \times n)} matrix \eqn{Y}, where \eqn{T} is the
#' number of observations and \eqn{n} the number of equations, i.e. endogenous
#' variables.
#' @param x_matrix A \eqn{(T \times k)} matrix \eqn{X} of observations on
#' \eqn{k} exogenous variables.
#' @param character_gamma_matrix A matrix \eqn{\Gamma} that holds the
#' coefficients in character form for all equations. The dimensions of the
#' matrix are \eqn{(T \times n)}, where \eqn{T} is the number of
#' observations and \eqn{n} the number of equations.
#' @param character_beta_matrix A matrix \eqn{\beta} that holds the
#' coefficients in character form for all equations. The dimensions of the
#' matrix are \eqn{(k \times n)}, where \eqn{k} is the number of
#' exogenous variables and \eqn{n} the number of equations.
#' @param jx The index of equation \eqn{j}.
#' @param gibbs_sampler An object of class `gibbs_sampler` that holds an
#' equations gibbs settings.
#' @param priors The priors for \eqn{\theta} in equation \eqn{j}.
#' @inheritParams draw_parameters_j
#'
#' @return A list containing matrices for the saved draws of parameters and
#' additional diagnostic information.
#' @keywords internal
draw_parameters_j_informative <- function(y_matrix, x_matrix,
                                          character_gamma_matrix,
                                          character_beta_matrix, jx,
                                          gibbs_sampler, priors,
                                          progress = function(amount) invisible()) {
  priors_j <- construct_priors_j(
    priors, character_gamma_matrix, character_beta_matrix, jx
  )
  # These prior terms are fixed for the equation across all Gibbs draws.
  priors_j$theta_precision <- solve(priors_j$theta_vcv)
  priors_j$theta_precision_mean <-
    priors_j$theta_precision %*% priors_j$theta_mean

  # pre-define matrices for saving
  out <- list()
  out$beta_jw <- vector("list", gibbs_sampler$nsave)
  out$theta_jw <- vector("list", gibbs_sampler$nsave)
  out$gamma_jw <- vector("list", gibbs_sampler$nsave)
  out$omega_jw <- vector("list", gibbs_sampler$nsave)
  out$omega_tilde_jw <- vector("list", gibbs_sampler$nsave)
  count_accepted <- matrix(0, gibbs_sampler$ndraws, 1)

  # cbind() and `[` are much slower on time series than on plain matrices and
  # are called in every draw below, so drop the time series class once here.
  y_matrix <- unclass(y_matrix)
  attr(y_matrix, "tsp") <- NULL
  x_matrix <- unclass(x_matrix)
  attr(x_matrix, "tsp") <- NULL

  # x_matrix is fixed across all draws of this equation, so x'x (and the
  # restricted-column x_b'x_b used for beta_hat) are invariant across the
  # whole loop below. Compute them once here instead of on every call.
  equation_data <- construct_equation_data(
    y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix, jx
  )
  xtx <- crossprod(x_matrix)
  xbtxb <- crossprod(equation_data$x_b)
  theta_permutation <- construct_theta_permutation(
    character_beta_matrix, jx,
    nrow(character_beta_matrix) *
      (equation_data$gamma_count + 1)
  )

  ##### 1. Initialize sampler:
  # get starting value for Metropolis-Hastings algorithm
  initial_parameter <- initialize_sampler(
    y_matrix,
    x_matrix,
    character_gamma_matrix,
    character_beta_matrix,
    jx,
    xtx,
    xbtxb,
    equation_data = equation_data
  )

  gamma_jw <- initial_parameter$gamma_parameters_j
  gamma_jw_1 <- gamma_jw

  cholesky_of_inverse_hessian <- initial_parameter$cholesky_of_inverse_hessian

  # Get additional starting value for the first step for omega_jw
  omega_jw <- initial_omega_j(
    y_matrix, x_matrix, character_gamma_matrix,
    character_beta_matrix, jx, gamma_jw, xtx, xbtxb, equation_data
  )

  gx <- 1 # initial value for saved draws
  # Report progress at most every 0.5 seconds; each update has a cost, and
  # with parallel workers the main process handles the updates of all workers.
  # The clock is compared as a plain number: a difftime costs ~15x more.
  pending_draws <- 0L
  last_report <- unclass(Sys.time())

  #### Start Gibbs sampler
  for (wx in 1:gibbs_sampler$ndraws) {
    ##### 2. Draw Theta_j from multivariate normal distribution
    results_draw_theta_j <- draw_theta_j_informative(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
      jx, gamma_jw, omega_jw, priors_j, xtx, theta_permutation, equation_data
    )

    # Get theta matrix
    theta_jw <- results_draw_theta_j$theta_mat_jw

    ##### 3. Draw Omega_j from inverse Wishart distribution
    results_draw_omega_j <- draw_omega_j_informative(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
      jx, gamma_jw, theta_jw, priors_j, equation_data
    )

    # Get omega_jw
    omega_jw <- results_draw_omega_j$omega_jw

    ##### 4. Draw gamma_j from Metropolis-Hastings algorithm
    gamma_jw <- draw_gamma_j_informative(
      y_matrix,
      x_matrix,
      character_gamma_matrix,
      character_beta_matrix,
      jx,
      gamma_jw_1,
      gibbs_sampler$tau,
      cholesky_of_inverse_hessian,
      omega_jw,
      theta_jw,
      priors_j,
      equation_data = equation_data
    )

    # Set count to 1 if the step has been accepted
    if (!identical(gamma_jw, gamma_jw_1)) {
      count_accepted[wx] <- 1
    }
    # Set gamma_jw_1 for next iteration
    gamma_jw_1 <- gamma_jw

    ##### Save draws
    if (wx > gibbs_sampler$burnin &&
      (wx - gibbs_sampler$burnin) %% gibbs_sampler$nstore == 0) {
      # Omega_tilde depends on gamma, but was computed in step 3 before gamma
      # was drawn in step 4. Recompute it with the gamma that is saved.
      omega_tilde_jw <- results_draw_omega_j$omega_tilde_jw
      if (nrow(omega_jw) > 1) {
        a_matrix_j <- diag(nrow(omega_jw))
        a_matrix_j[, 1] <- c(1, -gamma_jw)
        omega_tilde_jw <- t(a_matrix_j) %*% omega_jw %*% a_matrix_j
      }

      out$beta_jw[[gx]] <- results_draw_theta_j$beta_jw
      out$theta_jw[[gx]] <- results_draw_theta_j$theta_jw
      out$gamma_jw[[gx]] <- gamma_jw
      out$omega_jw[[gx]] <- results_draw_omega_j$omega_jw
      out$omega_tilde_jw[[gx]] <- omega_tilde_jw
      gx <- gx + 1
    }

    pending_draws <- pending_draws + 1L
    if (unclass(Sys.time()) - last_report > 0.5) {
      progress(pending_draws)
      pending_draws <- 0L
      last_report <- unclass(Sys.time())
    }
  }
  if (pending_draws > 0L) progress(pending_draws)

  if (all(is.na(out$gamma_jw))) count_accepted <- NA
  out$count_accepted <- count_accepted

  out
}

#' Draw gamma parameters for equation j
#'
#' `draw_gamma_j_informative` draws the \eqn{\gamma} parameters from a given
#' posterior distribution for an equation \eqn{j}, in the Metropolis-Hastings
#' algorithm.
#'
#' @inheritParams draw_parameters_j_informative
#' @param gamma_jw A \eqn{(n_j \times 1)} vector with the parameters
#' of the gamma matrix, where \eqn{n_j} is the number of endogenous variables in
#' equation \eqn{j}.
#' @param tau A tuning scalar \eqn{\tau} to adjust the acceptance rate.
#' @param cholesky_of_inverse_hessian The Cholesky factor \eqn{L} of the
#' inverse Hessian matrix \eqn{M^{-1}} used to generate candidate draws.
#' @param omega_jw \eqn{{\Omega}_j^{(w)}}
#' @param theta_jw A \eqn{(k \times nj+1)}, where \eqn{n_j} is the number of
#' endogenous variables in equation \eqn{j}.
#'
#' @return A \eqn{(n_j \times 1)} matrix with the either accepted candidate or
#' previous gamma parameters. Returns 0 if there are no endogenous
#' variables in equation \eqn{j}.
#' @param equation_data Fixed equation subsets and counts returned by
#'   [construct_equation_data()]. The samplers compute this once per equation.
#' @keywords internal
draw_gamma_j_informative <- function(y_matrix, x_matrix, character_gamma_matrix,
                                     character_beta_matrix, jx,
                                     gamma_jw, tau,
                                     cholesky_of_inverse_hessian, omega_jw,
                                     theta_jw, priors_j,
                                     equation_data) {
  number_endogenous_in_j <- equation_data$gamma_count
  if (number_endogenous_in_j == 0) {
    gamma_jw <- NA
    return(gamma_jw)
  } else {
    # Generate candidate draw
    # - tau is a scalar that tunes the acceptance rate towards xx%
    # - L is th cholesky factor of the inverse Hessian
    # - rt() is a r_gamma times 1 vector of Student t distributed
    #   random variables
    candidate_gamma_parameters_j <- gamma_jw +
      tau * cholesky_of_inverse_hessian %*%
        stats::rt(n = number_endogenous_in_j, 2)

    # Evaluate target function at candidate and previous parameter vector
    # *(-1) because sign of the target function inverted
    # (maximize instead of minimize).
    target_evaluation_candidate <- -target_j_informative(
      y_matrix = y_matrix,
      x_matrix = x_matrix,
      character_gamma_matrix = character_gamma_matrix,
      character_beta_matrix = character_beta_matrix,
      jx = jx,
      gamma_jw = candidate_gamma_parameters_j,
      omega_jw,
      theta_jw,
      priors_j,
      equation_data = equation_data
    )
    target_evaluation_previous <- -target_j_informative(
      y_matrix = y_matrix,
      x_matrix = x_matrix,
      character_gamma_matrix = character_gamma_matrix,
      character_beta_matrix = character_beta_matrix,
      jx = jx,
      gamma_jw = gamma_jw,
      omega_jw,
      theta_jw,
      priors_j,
      equation_data = equation_data
    )
    if (!is.finite(target_evaluation_previous)) {
      cli::cli_abort(c(
        "The Metropolis-Hastings target is not finite at the current value
        of gamma.",
        "i" = "The chain cannot move from here. Check the equation for
        constant or collinear series."
      ))
    }
    # Acceptance probability
    alpha <- min(
      1,
      exp(target_evaluation_candidate - target_evaluation_previous)
    )

    # Accept-reject step
    if (!is.na(alpha) && alpha > stats::runif(n = 1)) {
      gamma_jw <- candidate_gamma_parameters_j
    } else {
      gamma_jw <- gamma_jw
    }
    gamma_jw
  }
}
#' Draw Omega from inverse Wishart distribution for equation j
#'
#' `draw_omega_j_informative` draws the variance-covariance matrix
#' \eqn{\tilde{\Omega}_j} for each row of \eqn{[ u_j,  V_j ]}.
#'
#' @inheritParams draw_parameters_j_informative
#' @inheritParams draw_gamma_j_informative
#' @param priors_j The priors for \eqn{\omega} in equation \eqn{j}.
#'
#' @return List containing \eqn{{\tilde{\Omega}}_j^{(w)}} as `omega_tilde_jw`
#' and \eqn{\Omega_j^{(w)}} as `omega_jw`.
#' @param equation_data Fixed equation subsets and counts returned by
#'   [construct_equation_data()]. The samplers compute this once per equation.
#' @keywords internal
draw_omega_j_informative <- function(y_matrix, x_matrix, character_gamma_matrix,
                                     character_beta_matrix, jx, gamma_jw,
                                     theta_jw, priors_j,
                                     equation_data) {
  number_endogenous_in_j <- equation_data$gamma_count
  # number of exogenous, predetermined variables + intercept
  number_of_observations <- equation_data$number_of_observations

  if (number_endogenous_in_j == 0) {
    z_matrix_j <- as.matrix(y_matrix[, jx])

    # if number_endogenous_in_j=0 use identity when constructing Aj matrix
    a_matrix_j <- 1
  } else {
    y_matrix_j <- equation_data$y_matrix_j
    z_matrix_j <- construct_z_matrix_j(
      gamma_jw, y_matrix, y_matrix_j, jx
    )

    a_matrix_j <- diag((number_endogenous_in_j + 1))
    a_matrix_j[, 1] <- c(1, -gamma_jw)
  }

  # Compute scale parameter matrix
  omega_hat <- t(solve(a_matrix_j)) %*%
    t(z_matrix_j - x_matrix %*% theta_jw) %*%
    (z_matrix_j - x_matrix %*% theta_jw) %*% solve(a_matrix_j)

  omega_bar <- omega_hat + priors_j[["omega_scale"]]

  # Draw Omega_j
  omega_jw <- riwish(
    number_of_observations + priors_j[["omega_df"]],
    omega_bar
  )

  # Compute Omega_tilde
  omega_tilde_jw <- t(a_matrix_j) %*% omega_jw %*% a_matrix_j

  list(
    omega_tilde_jw = omega_tilde_jw,
    omega_jw = omega_jw
  )
}

#' Draw Theta from multivariate normal distribution for equation j
#' `draw_theta_j_informative` draw \eqn{\theta} parameters from a given
#' posterior distribution for an equation \eqn{j}.
#'
#' @inheritParams draw_parameters_j_informative
#' @inheritParams draw_gamma_j_informative
#' @param xtx Precomputed \eqn{x_matrix'x_matrix}. This is invariant across
#' Gibbs draws, so it is computed once instead of on every call.
#' @param theta_permutation Precomputed order of the elements of theta, as
#' returned by [construct_theta_permutation()]. Same rationale as `xtx`.
#'
#' @return List containing theta_jw and beta_jw
#' @param equation_data Fixed equation subsets and counts returned by
#'   [construct_equation_data()]. The samplers compute this once per equation.
#' @keywords internal
draw_theta_j_informative <- function(y_matrix, x_matrix, character_gamma_matrix,
                                     character_beta_matrix, jx, gamma_jw,
                                     omega_jw, priors_j, xtx,
                                     theta_permutation,
                                     equation_data) {
  number_endogenous_in_j <- equation_data$gamma_count
  number_of_exogenous <- equation_data$number_of_exogenous

  if (number_endogenous_in_j == 0) {
    z_matrix_j <- as.matrix(y_matrix[, jx])
    # if number_endogenous_in_j=0 use identity when constructing Aj matrix
    a_matrix_j <- 1
  } else {
    y_matrix_j <- equation_data$y_matrix_j
    z_matrix_j <- construct_z_matrix_j(
      gamma_jw, y_matrix, y_matrix_j, jx
    )
    a_matrix_j <- diag((number_endogenous_in_j + 1))
    a_matrix_j[, 1] <- c(1, -gamma_jw)
  }

  # Compute Omega_tilde
  omega_tilde_jw <- t(a_matrix_j) %*% omega_jw %*% a_matrix_j

  # Compute posterior mean and VCV for theta
  theta_mat <- c(
    construct_theta_bar_j(x_matrix, z_matrix_j, priors_j, omega_tilde_jw, xtx)
  )
  theta_bar <- theta_mat$theta_bar
  xi_bar <- theta_mat$xi_bar

  # Permute theta_hat such that the first block consists of all free
  # parameters and the second block contains all parameters that are
  # restricted to zero.
  # Zero restrictions only on betas, i.e. first column of theta_hat
  permutation <- theta_permutation$permutation
  seperate_blocks_at <- theta_permutation$seperate_blocks_at
  if (length(permutation) != length(theta_bar)) {
    cli::cli_abort(
      "The theta permutation has {length(permutation)} elements, but there
      are {length(theta_bar)} parameters."
    )
  }

  # Permute theta_bar
  theta_p <- theta_bar[permutation]

  # Construct the two blocks
  free <- seq_len(seperate_blocks_at)
  restricted <- setdiff(seq_along(theta_p), free)
  theta_p1 <- theta_p[free]
  theta_p2 <- theta_p[restricted]

  # Compute unrestricted posterior variance
  xi <- xi_bar

  # Permute xi
  xi_p <- xi[permutation, permutation, drop = FALSE]

  # Construct the two blocks
  xi_p11 <- xi_p[free, free, drop = FALSE]
  if (length(theta_p2) != 0) {
    xi_p12 <- xi_p[free, restricted, drop = FALSE]
    xi_p21 <- t(xi_p12)
    xi_p22 <- xi_p[restricted, restricted, drop = FALSE]
    # Compute update of posterior mean and posterior variance
    inverse_xi_p22 <- solve(xi_p22)
    theta_tilde <- theta_p1 - xi_p12 %*% inverse_xi_p22 %*% theta_p2
    xi_tilde <- xi_p11 - xi_p12 %*% inverse_xi_p22 %*% xi_p21
  } else {
    theta_tilde <- theta_p1
    xi_tilde <- xi_p11
  }

  theta_pw1 <- if (length(theta_tilde) == 0) {
    numeric(0)
  } else {
    multivariate_norm(n = 1, theta_tilde, xi_tilde)
  }

  # Construct full theta_p vector
  theta_pw <- c(theta_pw1, matrix(0, length(theta_p2), 1))

  # Permute back to original ordering
  theta_jw <- matrix(0, length(theta_pw), 1)
  theta_jw[permutation] <- theta_pw

  # Select beta vector
  beta_jw <- theta_pw1[seq_along(equation_data$beta_positions)]

  theta_mat_jw <- matrix(
    theta_jw, number_of_exogenous, (number_endogenous_in_j + 1)
  )
  list(theta_jw = theta_jw, theta_mat_jw = theta_mat_jw, beta_jw = beta_jw)
}

#' Compute the target function for the jth equation for informative priors
#'
#' `target_j_informative` calculates the target function used in the
#' Metropolis-Hastings (MH) algorithm for a given equation \eqn{j}.
#' The target function is used in the MH algorithm to accept or reject proposed
#' new states in the Markov chain.
#'
#' @inheritParams draw_parameters_j_informative
#' @inheritParams draw_gamma_j_informative
#'
#' @return The function returns the evaluation of the target function,
#' which is used to decide whether to accept or reject proposed states
#' in the MH algorithm. Returns NA if there are no gamma parameters.
#' @param equation_data Fixed equation subsets and counts returned by
#'   [construct_equation_data()]. The samplers compute this once per equation.
#' @keywords internal
target_j_informative <- function(y_matrix, x_matrix, character_gamma_matrix,
                                 character_beta_matrix, jx, gamma_jw, omega_jw,
                                 theta_jw, priors_j,
                                 equation_data) {
  if (equation_data$gamma_count == 0) {
    cli::cli_warn("Equation {jx} does not contain any gamma parameters. Returning NA.")
    return(NA)
  }
  y_matrix_j <- equation_data$y_matrix_j
  if (anyNA(y_matrix_j)) {
    return(NA)
  }

  gamma_count <- equation_data$gamma_count
  # Check number of expected gamma_jw
  if (gamma_count != length(gamma_jw)) {
    stop("The number of gamma parameters does not match the number
        of expected parameters.")
  }

  # Get number of endogenous variables in equation j
  number_endogenous_in_j <- equation_data$gamma_count

  if (number_endogenous_in_j == 0) {
    z_matrix_j <- as.matrix(y_matrix[, jx])

    # if number_endogenous_in_j=0 use identity when constructing Aj matrix
    a_matrix_j <- 1
  } else {
    y_matrix_j <- equation_data$y_matrix_j
    z_matrix_j <- construct_z_matrix_j(
      gamma_jw, y_matrix, y_matrix_j, jx
    )

    a_matrix_j <- diag((number_endogenous_in_j + 1))
    a_matrix_j[, 1] <- c(1, -gamma_jw)
  }

  # Evaluate log of target function
  # (multiply by -1: maximize instead of minimize)
  # Likelihood term
  target_result <-
    0.5 * sum(diag(t(solve(a_matrix_j)) %*%
      t(z_matrix_j - x_matrix %*% theta_jw) %*%
      (z_matrix_j - x_matrix %*% theta_jw) %*%
      solve(a_matrix_j) %*% solve(omega_jw)))

  # Prior term, only if explicit gamma priors are specified
  if (!is.null(priors_j[["gamma_mean"]]) && !is.null(priors_j[["gamma_vcv"]])) {
    target_result <- target_result -
      multivariate_norm_pdf(
        gamma_jw,
        mu = priors_j[["gamma_mean"]], sigma = priors_j[["gamma_vcv"]],
        log = TRUE
      )
  }
  target_result
}

#' Compute initial omega for the jth equation
#'
#' `initial_omega_j` computes OLS quantity for initial omega_j using initialized
#' gamma_j'
#'
#' @inheritParams draw_parameters_j_informative
#' @inheritParams draw_gamma_j_informative
#' @param xtx Precomputed \eqn{x_matrix'x_matrix}. This is invariant across
#' Gibbs draws, so it is computed once instead of on every call.
#' @param xbtxb Precomputed \eqn{x_b'x_b}, where \eqn{x_b} is
#' \eqn{x_matrix} restricted to the columns kept for equation \eqn{j}. Same
#' rationale as `xtx`.
#'
#' @return A \eqn{((1 + n_j) \times (1 + n_j))} matrix with the initial value
#' for \eqn{\Omega_j}, the residual covariance at the initial
#' \eqn{\gamma_j}. Returns NA if the endogenous regressors contain NA.
#' @param equation_data Fixed equation subsets and counts returned by
#'   [construct_equation_data()]. The samplers compute this once per equation.
#' @keywords internal
initial_omega_j <- function(y_matrix, x_matrix, character_gamma_matrix,
                            character_beta_matrix, jx, gamma_jw,
                            xtx, xbtxb,
                            equation_data) {
  gamma_count <- equation_data$gamma_count

  if (gamma_count == 0) {
    z_matrix_j <- as.matrix(y_matrix[, jx])
  } else {
    y_matrix_j <- equation_data$y_matrix_j

    if (anyNA(y_matrix_j)) {
      return(NA)
    }


    # Check number of expected gamma_parameters_j
    if (gamma_count != length(gamma_jw)) {
      stop("The number of gamma parameters does not match the number
        of expected parameters.")
    }

    z_matrix_j <- construct_z_matrix_j(
      gamma_jw, y_matrix, y_matrix_j, jx
    )
  }
  beta_hat_j <- construct_beta_hat_j_matrix(
    x_matrix, z_matrix_j, character_beta_matrix, jx, xbtxb, equation_data
  )

  pi_hat_0 <- construct_pi_hat_0(x_matrix, z_matrix_j, xtx)

  # Compute theta_hat matrix
  theta_hat <- cbind(beta_hat_j, pi_hat_0)

  # Residual covariance of Z_j, an estimate of Omega_tilde = A' Omega A
  omega_tilde_hat <- crossprod(z_matrix_j - x_matrix %*% theta_hat) /
    nrow(x_matrix)

  if (gamma_count == 0) {
    # A is the identity, so Omega_tilde is Omega
    return(omega_tilde_hat)
  }

  # Map back to Omega = A^-1' Omega_tilde A^-1
  a_matrix_j <- diag(gamma_count + 1)
  a_matrix_j[, 1] <- c(1, -gamma_jw)
  t(solve(a_matrix_j)) %*% omega_tilde_hat %*% solve(a_matrix_j)
}

#' Construct priors for a single equation
#'
#' `construct_priors_j` generates prior hyperparameters for equation `jx`
#' based on user-specified priors and the model's coefficient matrices.
#'
#' @inheritParams draw_parameters_j_informative
#' @return A list with elements:
#'   * `theta_mean`: prior means for exogenous coefficients and intercept.
#'   * `theta_vcv`: prior variance-covariance matrix for exogenous terms.
#'   * `omega_df`: degrees of freedom for error covariance prior.
#'   * `omega_scale`: scale matrix for error covariance prior.
#'   * `gamma_mean`: prior means for endogenous coefficients (if set).
#'   * `gamma_vcv`: prior variance-covariance for endogenous terms.
#' @keywords internal
construct_priors_j <- function(priors, character_gamma_matrix,
                               character_beta_matrix, jx) {
  str_priors_j <- priors[[jx]]

  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))
  endogenous_in_j <-
    rownames(character_gamma_matrix)[grep("gamma", character_gamma_matrix[, jx])]

  number_of_exogenous <- nrow(character_beta_matrix)
  priors_j <- list()

  # Priors for theta
  # (for coefficients related to exogenous variables and intercept)
  priors_j[["theta_mean"]] <- matrix(
    0, number_of_exogenous * (number_endogenous_in_j + 1), 1
  )
  priors_j[["theta_vcv"]] <- diag(
    1000, number_of_exogenous * (number_endogenous_in_j + 1)
  )

  # Priors for omega
  # (for covariance matrix of the error terms)
  # degrees of freedom
  priors_j[["omega_df"]] <- number_endogenous_in_j + 2 # diffuse prior
  # scale
  priors_j[["omega_scale"]] <- diag(0.001, number_endogenous_in_j + 1)

  # Initialize priors for gamma only if gamma priors have been set
  if (any(names(str_priors_j) %in% endogenous_in_j)) {
    priors_j[["gamma_mean"]] <- matrix(0, number_endogenous_in_j, 1)
    priors_j[["gamma_vcv"]] <- diag(100, number_endogenous_in_j)
  }

  for (var in names(str_priors_j)) {
    if (var %in% rownames(character_beta_matrix)) {
      pos <- which(var == rownames(character_beta_matrix))
      priors_j$theta_mean[pos] <- str_priors_j[[var]][[1]]
      priors_j$theta_vcv[pos, pos] <- str_priors_j[[var]][[2]]
    } else if (var %in% rownames(character_gamma_matrix)) {
      pos <- which(var == endogenous_in_j)
      # Priors for gamma
      # (for coefficients of the endogenous variables)
      # only add gamma priors if they are explicitly defined

      priors_j$gamma_mean[pos] <- str_priors_j[[var]][[1]]
      priors_j$gamma_vcv[pos, pos] <- str_priors_j[[var]][[2]]
    } else if (var == "epsilon") {
      priors_j$omega_df <- str_priors_j[[var]][[1]]
      priors_j$omega_scale <-
        diag(str_priors_j[[var]][[2]], number_endogenous_in_j + 1)
    }
  }

  priors_j
}
