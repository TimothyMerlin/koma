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
#'
#' @return A list containing matrices for the saved draws of parameters and
#' additional diagnostic information.
#' @keywords internal
draw_parameters_j <- function(y_matrix, x_matrix, character_gamma_matrix,
                              character_beta_matrix, jx, gibbs_sampler) {
  out <- list()
  out$beta_jw <- vector("list", gibbs_sampler$nsave)
  out$theta_jw <- vector("list", gibbs_sampler$nsave)
  out$gamma_jw <- vector("list", gibbs_sampler$nsave)
  out$omega_jw <- vector("list", gibbs_sampler$nsave)
  out$omega_tilde_jw <- vector("list", gibbs_sampler$nsave)
  count_accepted <- matrix(0, gibbs_sampler$ndraws, 1)

  ##### 1. Initialize sampler:
  # get starting value for Metropolis-Hastings algorithm
  initial_parameter <- initialize_sampler(
    y_matrix,
    x_matrix,
    character_gamma_matrix,
    character_beta_matrix,
    jx
  )

  gamma_jw_1 <- initial_parameter$gamma_parameters_j
  choelsky_of_inverse_hessian <- initial_parameter$cholesky_of_inverse_hessian

  gx <- 1 # initial value for saved draws

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
      choelsky_of_inverse_hessian
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
      jx, gamma_jw
    )

    ##### Draw 4. Theta_j from multivariate normal distribution
    results_draw_theta_j <- draw_theta_j(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
      jx, gamma_jw, results_draw_omega_j$omega_tilde_jw
    )

    ##### Save draws
    if (wx > gibbs_sampler$burnin) {
      out$beta_jw[[gx]] <- results_draw_theta_j$beta_jw
      out$theta_jw[[gx]] <- results_draw_theta_j$theta_jw
      out$gamma_jw[[gx]] <- gamma_jw
      out$omega_jw[[gx]] <- results_draw_omega_j$omega_jw
      out$omega_tilde_jw[[gx]] <- results_draw_omega_j$omega_tilde_jw
      gx <- gx + 1
    }
  }

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
#'
#' @return A list containing the initial parameters for gamma
#' (`gamma_parameters_j`) and the Cholesky factor of the
#' inverse of the Hessian (`cholesky_of_inverse_hessian`) if
#' `number_endogenous_in_j` is greater than 0.
#' If not, only the `gamma_parameters_j` parameter is returned as 0.
#' @keywords internal
initialize_sampler <- function(y_matrix, x_matrix, character_gamma_matrix,
                               character_beta_matrix, jx) {
  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))
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
      hessian = TRUE,
      method = "BFGS"
    )

    # Use maximum as initial condition
    gamma_parameters_j <- optimize_residuals$par
    # Use inverse of Hessian to approximate dispersion of target function
    inverse_hessian <- Matrix::solve(optimize_residuals$hessian)
    # Cholesky factor of inverse of Hessian
    cholesky_of_inverse_hessian <- t(Matrix::chol(inverse_hessian))

    list(
      gamma_parameters_j = gamma_parameters_j,
      cholesky_of_inverse_hessian = cholesky_of_inverse_hessian
    )
  }
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
#'
#' @return A \eqn{(n_j \times 1)} matrix with the either accepted candidate or
#' previous gamma parameters. Returns 0 if there are no endogenous
#' variables in equation \eqn{j}.
#' @keywords internal
draw_gamma_j <- function(y_matrix, x_matrix, character_gamma_matrix,
                         character_beta_matrix, jx,
                         gamma_parameters_j, tau,
                         cholesky_of_inverse_hessian) {
  number_endogenous_in_j <- length(grep("gamma", character_gamma_matrix[, jx]))
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
      gamma_parameters_j = candidate_gamma_parameters_j
    )
    target_evaluation_previous <- -target_j(
      y_matrix = y_matrix,
      x_matrix = x_matrix,
      character_gamma_matrix = character_gamma_matrix,
      character_beta_matrix = character_beta_matrix,
      jx = jx,
      gamma_parameters_j = gamma_parameters_j
    )
    # Acceptance probability
    alpha <- min(
      1,
      exp(target_evaluation_candidate - target_evaluation_previous)
    )

    # Accept-reject step
    if (alpha > stats::runif(n = 1)) {
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
#'
#' @return List containing \eqn{{\tilde{\Omega}}_j^{(w)}} as `omega_tilde_jw`
#' and \eqn{\Omega_j^{(w)}} as `omega_jw`.
#' @keywords internal
draw_omega_j <- function(y_matrix, x_matrix, character_gamma_matrix,
                         character_beta_matrix, jx, gamma_parameters_j) {
  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))
  # number of exogenous, predetermined variables + intercept
  number_of_exogenous <- nrow(character_beta_matrix)
  number_of_observations <- nrow(y_matrix)

  if (number_endogenous_in_j == 0) {
    z_matrix_j <- as.matrix(y_matrix[, jx])
    beta_hat_j <- construct_beta_hat_j_matrix(
      x_matrix, z_matrix_j, character_beta_matrix, jx
    )

    theta_hat <- beta_hat_j
    # if number_endogenous_in_j=0 use identity when constructing Aj matrix
    a_matrix_j <- 1
  } else {
    y_matrix_j <- construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)
    z_matrix_j <- construct_z_matrix_j(
      gamma_parameters_j, y_matrix, y_matrix_j, jx
    )
    beta_hat_j <- construct_beta_hat_j_matrix(
      x_matrix, z_matrix_j, character_beta_matrix, jx
    )
    pi_hat_0 <- construct_pi_hat_0(x_matrix, z_matrix_j)

    theta_hat <- cbind(beta_hat_j, pi_hat_0)
    a_matrix_j <- diag((number_endogenous_in_j + 1))
    a_matrix_j[, 1] <- c(1, -gamma_parameters_j)
  }

  # Compute scale parameter matrix
  omega_hat <- t(Matrix::solve(a_matrix_j)) %*%
    t(z_matrix_j - x_matrix %*% theta_hat) %*%
    (z_matrix_j - x_matrix %*% theta_hat) %*% Matrix::solve(a_matrix_j)

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
#'
#' @return List containing theta_jw and beta_jw
#' @keywords internal
draw_theta_j <- function(y_matrix, x_matrix, character_gamma_matrix,
                         character_beta_matrix, jx, gamma_parameters_j,
                         omega_tilde_jw) {
  number_endogenous_in_j <- length(grep("gamma", character_gamma_matrix[, jx]))

  if (number_endogenous_in_j == 0) {
    z_matrix_j <- as.matrix(y_matrix[, jx])
  } else {
    y_matrix_j <- construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)
    z_matrix_j <- construct_z_matrix_j(
      gamma_parameters_j, y_matrix, y_matrix_j, jx
    )
  }

  # Compute unrestricted posterior mean
  # c() vectorizes matrix
  theta_hat <- c(construct_theta_hat_j(x_matrix, z_matrix_j))

  # Permute theta_hat such that the first block consists of all free
  # parameters and the second block contains all parameters that are
  # restricted to zero.
  # Zero restrictions only on betas, i.e. first column of theta_hat
  # Choose free parameters from first column (this is P')
  permutation_matrix <- matrix(0, length(theta_hat), length(theta_hat))
  # Find the indices of elements in equation j that are betas
  fpos <- grep("^0", character_beta_matrix[, jx], invert = TRUE)
  for (ix in seq_along(fpos)) {
    permutation_matrix[ix, fpos[ix]] <- 1
  }

  # Permute zero restrictions to the end
  # Find the indices of elements in equation j that are 0
  fposend <- grep("\\b0\\b", character_beta_matrix[, jx])

  permute_from_row <- length(fpos) + 1
  seperate_blocks_at <- length(theta_hat) - length(fposend)

  for (ix in seq_along(fposend)) {
    permutation_matrix[(seperate_blocks_at + ix), fposend[ix]] <- 1
  }

  if (number_endogenous_in_j != 0) {
    # Leave free parameters of other columns at their place
    permutation_matrix[
      permute_from_row:seperate_blocks_at,
      (length(character_beta_matrix[, jx]) + 1):length(theta_hat)
    ] <-
      diag(length(theta_hat) - length(character_beta_matrix[, jx]))
  }
  # Permute theta_hat
  theta_p <- permutation_matrix %*% theta_hat

  # Construct the two blocks
  theta_p1 <- theta_p[1:seperate_blocks_at]
  theta_p2 <- theta_p[-(1:seperate_blocks_at)]

  # Compute unrestricted posterior variance
  xi <- kronecker(omega_tilde_jw, Matrix::solve(t(x_matrix) %*% x_matrix))

  # Permute xi
  xi_p <- permutation_matrix %*% xi %*% t(permutation_matrix)

  # Construct the two blocks
  xi_p11 <- xi_p[1:seperate_blocks_at, 1:seperate_blocks_at, drop = FALSE]
  if (length(theta_p2) != 0) {
    xi_p12 <- xi_p[1:seperate_blocks_at, (seperate_blocks_at + 1):length(theta_hat), drop = FALSE]
    xi_p21 <- t(xi_p12)
    xi_p22 <- xi_p[(seperate_blocks_at + 1):length(theta_hat), (seperate_blocks_at + 1):length(theta_hat), drop = FALSE]

    # Compute update of posterior mean and posterior variance
    theta_bar <- theta_p1 - xi_p12 %*% Matrix::solve(xi_p22) %*% theta_p2
    xi_bar <- xi_p11 - xi_p12 %*% Matrix::solve(xi_p22) %*% xi_p21
  } else {
    theta_bar <- theta_p1
    xi_bar <- xi_p11
  }

  theta_pw1 <- multivariate_norm(n = 1, theta_bar, xi_bar)

  # Construct full theta_p vector
  # theta_pw <- c(theta_pw1, matrix(0, length(theta_p2), 1))

  # Permute back to original ordering
  # theta_jw <- t(permutation_matrix) %*% theta_pw

  exo_in_jx <- character_beta_matrix[which(character_beta_matrix[, jx] != 0), jx]
  # Select beta vector
  beta_jw <- theta_pw1[seq_along(exo_in_jx)]

  list(theta_jw = theta_pw1, beta_jw = beta_jw)
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
#' @keywords internal
target_j <- function(y_matrix, x_matrix, character_gamma_matrix,
                     character_beta_matrix, jx, gamma_parameters_j) {
  y_matrix_j <- construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)
  if (anyNA(y_matrix_j)) {
    return(NA)
  }

  gamma_count <- sum(grepl("gamma", character_gamma_matrix[, jx]))
  # Check number of expected gamma_parameters_j
  if (gamma_count != length(gamma_parameters_j)) {
    stop("The number of gamma parameters does not match the number
        of expected parameters.")
  }

  number_of_observations <- nrow(y_matrix)
  # number of exogenous, predetermined variables + intercept
  number_of_exogenous <- nrow(character_beta_matrix)

  z_matrix_j <- construct_z_matrix_j(
    gamma_parameters_j, y_matrix, y_matrix_j, jx
  )

  beta_hat_j <- construct_beta_hat_j_matrix(
    x_matrix, z_matrix_j, character_beta_matrix, jx
  )

  pi_hat_0 <- construct_pi_hat_0(x_matrix, z_matrix_j)

  # Compute theta_hat matrix
  theta_hat <- cbind(beta_hat_j, pi_hat_0)

  # Evaluate log of target function
  # (multiply by -1: maximize instead of minimize)
  target_result <- ((number_of_observations - number_of_exogenous) / 2) *
    log(Matrix::det(t(z_matrix_j - x_matrix %*% theta_hat) %*%
      (z_matrix_j - x_matrix %*% theta_hat)))

  target_result
}
