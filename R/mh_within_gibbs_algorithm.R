#' Draw Parameters for equation j
#'
#' `draw_parameters_j` simulates from the posterior of the model's parameters
#' for each equation \eqn{j} separately.
#' The posterior is simulated using a Metropolis-within-Gibbs sampling
#' procedure.
#'
#' The sampler works as follows:
#' 1. Initialize sampler
#' 2. Conditional on \eqn{\Omega^{(w-1)}} and the data draw
#' \eqn{delta_{\gamma}}.
#' 3. Conditional on \eqn{delta_{\gamma}^{(w)}} draw \eqn{\Omega^{(w)}}.
#' 4. Conditional on \eqn{Omega^{(w)}} and \eqn{\delta_{\gamma}^{(w)}} draw
#' \eqn{\delta_\beta^{(w)}}.
#' 5. Go back to step 2.
#'
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
#' @param progress Function called with the number of completed draws since
#' its last call, at most about every 0.5 seconds. Used to update the progress
#' bar.
#'
#' @return A list containing matrices for the saved draws of parameters and
#' additional diagnostic information.
#' @keywords internal
draw_parameters_j <- function(y_matrix, x_matrix, character_gamma_matrix,
                              character_beta_matrix, jx, gibbs_sampler,
                              progress = function(amount) invisible()) {
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
  inverse_xtx <- solve(xtx)
  theta_permutation <- construct_theta_permutation(
    character_beta_matrix, jx,
    nrow(character_beta_matrix) *
      (equation_data$gamma_count + 1)
  )
  xbtxb <- crossprod(equation_data$x_b)

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

  gamma_jw_1 <- initial_parameter$gamma_parameters_j
  choelsky_of_inverse_hessian <- initial_parameter$cholesky_of_inverse_hessian

  gx <- 1 # initial value for saved draws
  # Report progress at most every 0.5 seconds; each update has a cost, and
  # with parallel workers the main process handles the updates of all workers.
  # The clock is compared as a plain number: a difftime costs ~15x more.
  pending_draws <- 0L
  last_report <- unclass(Sys.time())

  #### Start Gibbs sampler
  for (wx in 1:gibbs_sampler$ndraws) {
    ##### 2. Draw gamma_j from Metropolis-Hastings algorithm
    gamma_jw <- draw_gamma_j(
      y_matrix,
      x_matrix,
      character_gamma_matrix,
      character_beta_matrix,
      jx,
      gamma_jw_1,
      gibbs_sampler$tau,
      choelsky_of_inverse_hessian,
      xtx,
      xbtxb,
      equation_data = equation_data
    )

    # Set count to 1 if the step has been accepted
    if (!identical(gamma_jw, gamma_jw_1)) {
      count_accepted[wx] <- 1
    }
    # Set gamma_jw_1 for next iteration
    gamma_jw_1 <- gamma_jw

    ##### 3. Draw Omega_j from inverse Wishart distribution
    results_draw_omega_j <- draw_omega_j(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
      jx, gamma_jw, xtx, xbtxb, equation_data
    )

    ##### Draw 4. Theta_j from multivariate normal distribution
    results_draw_theta_j <- draw_theta_j(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
      jx, gamma_jw, results_draw_omega_j$omega_tilde_jw, xtx, inverse_xtx,
      theta_permutation, equation_data
    )

    ##### Save draws
    if (wx > gibbs_sampler$burnin &&
      (wx - gibbs_sampler$burnin) %% gibbs_sampler$nstore == 0) {
      out$beta_jw[[gx]] <- results_draw_theta_j$beta_jw
      out$theta_jw[[gx]] <- results_draw_theta_j$theta_jw
      out$gamma_jw[[gx]] <- gamma_jw
      out$omega_jw[[gx]] <- results_draw_omega_j$omega_jw
      out$omega_tilde_jw[[gx]] <- results_draw_omega_j$omega_tilde_jw
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

#' Initialize the sampler
#'
#' `initialize_sampler` initializes the sampler for the Metropolis Hastings
#' algorithm. It obtains the initial \eqn{gamma} parameters by numerically
#' maximizing the target function. It then calculates the Cholesky factor
#' \eqn{L} of the inverse of the Hessian \eqn{M^{-1}} of the target function.
#' This Cholesky factor, along with the gamma parameter for equation \eqn{j}, is
#' returned. The Cholesky factor is used to draw the candidate gamma in the MH
#' algorithm.
#'
#' @inheritParams draw_parameters_j
#' @param xtx Precomputed \eqn{x_matrix'x_matrix}. This is invariant across
#' Gibbs draws, so it is computed once instead of on every call.
#' @param xbtxb Precomputed \eqn{x_b'x_b}, where \eqn{x_b} is
#' \eqn{x_matrix} restricted to the columns kept for equation \eqn{j}. Same
#' rationale as `xtx`.
#'
#' @return A list containing the initial parameters for gamma
#' (`gamma_parameters_j`) and the Cholesky factor of the
#' inverse of the Hessian (`cholesky_of_inverse_hessian`) if
#' `number_endogenous_in_j` is greater than 0.
#' If not, only the `gamma_parameters_j` parameter is returned as 0.
#' @param equation_data Fixed equation subsets and counts returned by
#'   [construct_equation_data()]. The samplers compute this once per equation.
#' @keywords internal
initialize_sampler <- function(y_matrix, x_matrix, character_gamma_matrix,
                               character_beta_matrix, jx,
                               xtx, xbtxb,
                               equation_data) {
  number_endogenous_in_j <- equation_data$gamma_count
  if (number_endogenous_in_j == 0) {
    return(list(
      gamma_parameters_j = 0,
      cholesky_of_inverse_hessian = NA
    ))
  } else {
    # Maximize target function to obtain initial conditions for MH-algorithm
    optimize_residuals <- stats::optim(
      par = matrix(0, number_endogenous_in_j, 1),
      fn = target_j,
      y_matrix = y_matrix,
      x_matrix = x_matrix,
      character_gamma_matrix = character_gamma_matrix,
      character_beta_matrix = character_beta_matrix,
      jx = jx,
      xtx = xtx,
      xbtxb = xbtxb,
      equation_data = equation_data,
      hessian = TRUE,
      method = "BFGS"
    )

    # Use maximum as initial condition
    gamma_parameters_j <- optimize_residuals$par
    cholesky_of_inverse_hessian <-
      construct_cholesky_of_inverse_hessian(optimize_residuals$hessian)

    list(
      gamma_parameters_j = gamma_parameters_j,
      cholesky_of_inverse_hessian = cholesky_of_inverse_hessian
    )
  }
}

#' Cholesky factor of the inverse Hessian
#'
#' Computes the Cholesky factor \eqn{L} of the inverse of the Hessian
#' \eqn{M^{-1}} of the target function at its optimum, used to draw the
#' candidate gamma in the MH algorithm.
#'
#' @param hessian The Hessian of the target function at its optimum, as
#' returned by [stats::optim()].
#'
#' @return The lower triangular Cholesky factor of the inverse Hessian. Stops
#' with an error if the Hessian is not positive definite, i.e. the target has
#' no proper optimum to start the sampler from.
#' @keywords internal
construct_cholesky_of_inverse_hessian <- function(hessian) {
  eigenvalues <- if (all(is.finite(hessian))) {
    eigen(hessian, symmetric = TRUE, only.values = TRUE)$values
  }
  if (is.null(eigenvalues) || any(eigenvalues <= 0)) {
    cli::cli_abort(c(
      "The Metropolis-Hastings sampler cannot be started.",
      "x" = "The target is flat or not at a maximum in some direction of
      gamma, so its curvature cannot scale the proposal.",
      "i" = "Check the equation for constant or collinear series, and
      whether its gamma parameters are identified by the data."
    ))
  }
  # Use inverse of Hessian to approximate dispersion of target function
  t(chol(solve(hessian)))
}

#' Draw gamma parameters for equation j
#'
#' `draw_gamma_j` draws the \eqn{\gamma} parameters from a given posterior
#' distribution for an equation \eqn{j}, in the Metropolis-Hastings algorithm.
#'
#' @inheritParams draw_parameters_j
#' @param gamma_parameters_j A \eqn{(n_j \times 1)} matrix with the parameters
#' of the gamma matrix, where \eqn{n_j} is the number of endogenous variables in
#' equation \eqn{j}.
#' @param tau A tuning scalar \eqn{\tau} to adjust the acceptance rate.
#' @param cholesky_of_inverse_hessian The Cholesky factor \eqn{L} of the
#' inverse Hessian matrix \eqn{M^{-1}} used to generate candidate draws.
#' @param xtx Precomputed \eqn{x_matrix'x_matrix}. This is invariant across
#' Gibbs draws, so it is computed once instead of on every call.
#' @param xbtxb Precomputed \eqn{x_b'x_b}, where \eqn{x_b} is
#' \eqn{x_matrix} restricted to the columns kept for equation \eqn{j}. Same
#' rationale as `xtx`.
#'
#' @return A \eqn{(n_j \times 1)} matrix with the either accepted candidate or
#' previous gamma parameters. Returns 0 if there are no endogenous
#' variables in equation \eqn{j}.
#' @param equation_data Fixed equation subsets and counts returned by
#'   [construct_equation_data()]. The samplers compute this once per equation.
#' @keywords internal
draw_gamma_j <- function(y_matrix, x_matrix, character_gamma_matrix,
                         character_beta_matrix, jx,
                         gamma_parameters_j, tau,
                         cholesky_of_inverse_hessian,
                         xtx, xbtxb,
                         equation_data) {
  number_endogenous_in_j <- equation_data$gamma_count
  if (number_endogenous_in_j == 0) {
    gamma_parameters_j <- NA
    return(gamma_parameters_j)
  } else {
    # Generate candidate draw
    # - tau is a scalar that tunes the acceptance rate towards xx%
    # - L is th cholesky factor of the inverse Hessian
    # - rt() is a r_gamma times 1 vector of Student t distributed
    #   random variables
    candidate_gamma_parameters_j <- gamma_parameters_j +
      tau * cholesky_of_inverse_hessian %*%
        stats::rt(n = number_endogenous_in_j, 2)

    # Evaluate target function at candidate and previous parameter vector
    # *(-1) because sign of the target function inverted
    # (maximize instead of minimize).
    target_evaluation_candidate <- -target_j(
      y_matrix = y_matrix,
      x_matrix = x_matrix,
      character_gamma_matrix = character_gamma_matrix,
      character_beta_matrix = character_beta_matrix,
      jx = jx,
      gamma_parameters_j = candidate_gamma_parameters_j,
      xtx = xtx,
      xbtxb = xbtxb,
      equation_data = equation_data
    )
    target_evaluation_previous <- -target_j(
      y_matrix = y_matrix,
      x_matrix = x_matrix,
      character_gamma_matrix = character_gamma_matrix,
      character_beta_matrix = character_beta_matrix,
      jx = jx,
      gamma_parameters_j = gamma_parameters_j,
      xtx = xtx,
      xbtxb = xbtxb,
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
      gamma_parameters_j <- candidate_gamma_parameters_j
    } else {
      gamma_parameters_j <- gamma_parameters_j
    }
    as.matrix(gamma_parameters_j)
  }
}
#' Draw Omega from inverse Wishart distribution for equation j
#'
#' `draw_omega_j` draws the variance-covariance matrix \eqn{\tilde{\Omega}_j}
#' for each row of \eqn{[ u_j,  V_j ]}.
#'
#' @inheritParams draw_parameters_j
#' @param xtx Precomputed \eqn{x_matrix'x_matrix}. This is invariant across
#' Gibbs draws, so it is computed once instead of on every call.
#' @param xbtxb Precomputed \eqn{x_b'x_b}, where \eqn{x_b} is
#' \eqn{x_matrix} restricted to the columns kept for equation \eqn{j}. Same
#' rationale as `xtx`.
#'
#' @return List containing \eqn{{\tilde{\Omega}}_j^{(w)}} as `omega_tilde_jw`
#' and \eqn{\Omega_j^{(w)}} as `omega_jw`.
#' @param equation_data Fixed equation subsets and counts returned by
#'   [construct_equation_data()]. The samplers compute this once per equation.
#' @keywords internal
draw_omega_j <- function(y_matrix, x_matrix, character_gamma_matrix,
                         character_beta_matrix, jx, gamma_parameters_j,
                         xtx, xbtxb,
                         equation_data) {
  number_endogenous_in_j <- equation_data$gamma_count
  # number of exogenous, predetermined variables + intercept
  number_of_exogenous <- equation_data$number_of_exogenous
  number_of_observations <- equation_data$number_of_observations

  if (number_endogenous_in_j == 0) {
    z_matrix_j <- as.matrix(y_matrix[, jx])
    beta_hat_j <- construct_beta_hat_j_matrix(
      x_matrix, z_matrix_j, character_beta_matrix, jx, xbtxb, equation_data
    )

    theta_hat <- beta_hat_j
    # if number_endogenous_in_j=0 use identity when constructing Aj matrix
    a_matrix_j <- 1
  } else {
    y_matrix_j <- equation_data$y_matrix_j
    z_matrix_j <- construct_z_matrix_j(
      gamma_parameters_j, y_matrix, y_matrix_j, jx
    )
    beta_hat_j <- construct_beta_hat_j_matrix(
      x_matrix, z_matrix_j, character_beta_matrix, jx, xbtxb, equation_data
    )
    pi_hat_0 <- construct_pi_hat_0(x_matrix, z_matrix_j, xtx)

    theta_hat <- cbind(beta_hat_j, pi_hat_0)
    a_matrix_j <- diag((number_endogenous_in_j + 1))
    a_matrix_j[, 1] <- c(1, -gamma_parameters_j)
  }

  # Compute scale parameter matrix
  omega_hat <- t(solve(a_matrix_j)) %*%
    t(z_matrix_j - x_matrix %*% theta_hat) %*%
    (z_matrix_j - x_matrix %*% theta_hat) %*% solve(a_matrix_j)

  # Draw Omega_j
  omega_jw <- riwish(
    number_of_observations - number_of_exogenous,
    omega_hat
  )

  # Compute Omega_tilde
  omega_tilde_jw <- t(a_matrix_j) %*% omega_jw %*% a_matrix_j

  list(
    omega_tilde_jw = omega_tilde_jw,
    omega_jw = omega_jw
  )
}

#' Draw Theta from multivariate normal distribution for equation j
#'
#' @inheritParams draw_parameters_j
#' @inheritParams draw_gamma_j
#' @param inverse_xtx Precomputed inverse of \eqn{x_matrix'x_matrix}. Same
#' rationale as `xtx`.
#' @param theta_permutation Precomputed order of the elements of theta, as
#' returned by [construct_theta_permutation()]. Same rationale as `xtx`.
#'
#' @return List containing theta_jw and beta_jw
#' @param equation_data Fixed equation subsets and counts returned by
#'   [construct_equation_data()]. The samplers compute this once per equation.
#' @keywords internal
draw_theta_j <- function(y_matrix, x_matrix, character_gamma_matrix,
                         character_beta_matrix, jx, gamma_parameters_j,
                         omega_tilde_jw, xtx, inverse_xtx = solve(xtx),
                         theta_permutation,
                         equation_data) {
  number_endogenous_in_j <- equation_data$gamma_count

  if (number_endogenous_in_j == 0) {
    z_matrix_j <- as.matrix(y_matrix[, jx])
  } else {
    y_matrix_j <- equation_data$y_matrix_j
    z_matrix_j <- construct_z_matrix_j(
      gamma_parameters_j, y_matrix, y_matrix_j, jx
    )
  }

  # Compute unrestricted posterior mean
  # c() vectorizes matrix
  theta_hat <- c(construct_theta_hat_j(x_matrix, z_matrix_j, xtx))

  # Permute theta_hat such that the first block consists of all free
  # parameters and the second block contains all parameters that are
  # restricted to zero.
  # Zero restrictions only on betas, i.e. first column of theta_hat
  permutation <- theta_permutation$permutation
  seperate_blocks_at <- theta_permutation$seperate_blocks_at
  if (length(permutation) != length(theta_hat)) {
    cli::cli_abort(
      "The theta permutation has {length(permutation)} elements, but there
      are {length(theta_hat)} parameters."
    )
  }

  # Permute theta_hat
  theta_p <- theta_hat[permutation]

  # Construct the two blocks
  free <- seq_len(seperate_blocks_at)
  restricted <- setdiff(seq_along(theta_p), free)
  theta_p1 <- theta_p[free]
  theta_p2 <- theta_p[restricted]

  # Compute unrestricted posterior variance
  xi <- kronecker(omega_tilde_jw, inverse_xtx)

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
    theta_bar <- theta_p1 - xi_p12 %*% inverse_xi_p22 %*% theta_p2
    xi_bar <- xi_p11 - xi_p12 %*% inverse_xi_p22 %*% xi_p21
  } else {
    theta_bar <- theta_p1
    xi_bar <- xi_p11
  }

  theta_pw1 <- if (length(theta_bar) == 0) {
    numeric(0)
  } else {
    multivariate_norm(n = 1, theta_bar, xi_bar)
  }

  # Construct full theta_p vector
  theta_pw <- c(theta_pw1, matrix(0, length(theta_p2), 1))

  # Permute back to original ordering
  theta_jw <- matrix(0, length(theta_pw), 1)
  theta_jw[permutation] <- theta_pw

  beta_positions <- equation_data$beta_positions
  # Select beta vector
  beta_jw <- theta_pw1[seq_along(beta_positions)]

  list(theta_jw = theta_jw, beta_jw = beta_jw)
}

#' Compute the target function for the jth equation
#'
#' `target_j` calculates the target function used in the Metropolis-Hastings
#' (MH) algorithm for a given equation \eqn{j}. The target function is used
#' in the MH algorithm to accept or reject proposed new states in the
#' Markov chain.
#'
#' @inheritParams draw_parameters_j
#' @inheritParams draw_gamma_j
#'
#' @return The function returns the evaluation of the target function,
#' which is used to decide whether to accept or reject proposed states
#' in the MH algorithm. Returns NA if there are no gamma parameters.
#' @param equation_data Fixed equation subsets and counts returned by
#'   [construct_equation_data()]. The samplers compute this once per equation.
#' @keywords internal
target_j <- function(y_matrix, x_matrix, character_gamma_matrix,
                     character_beta_matrix, jx, gamma_parameters_j,
                     xtx, xbtxb,
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
  # Check number of expected gamma_parameters_j
  if (gamma_count != length(gamma_parameters_j)) {
    stop("The number of gamma parameters does not match the number
        of expected parameters.")
  }

  number_of_observations <- equation_data$number_of_observations
  # number of exogenous, predetermined variables + intercept
  number_of_exogenous <- equation_data$number_of_exogenous

  z_matrix_j <- construct_z_matrix_j(
    gamma_parameters_j, y_matrix, y_matrix_j, jx
  )

  beta_hat_j <- construct_beta_hat_j_matrix(
    x_matrix, z_matrix_j, character_beta_matrix, jx, xbtxb, equation_data
  )

  pi_hat_0 <- construct_pi_hat_0(x_matrix, z_matrix_j, xtx)

  # Compute theta_hat matrix
  theta_hat <- cbind(beta_hat_j, pi_hat_0)

  # Evaluate log of target function
  # (multiply by -1: maximize instead of minimize)
  target_result <- ((number_of_observations - number_of_exogenous) / 2) *
    log(det(t(z_matrix_j - x_matrix %*% theta_hat) %*%
      (z_matrix_j - x_matrix %*% theta_hat)))

  target_result
}
