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
#'
#' @return A list containing matrices for the saved draws of parameters and
#' additional diagnostic information.
#' @keywords internal
draw_parameters_j_informative <- function(y_matrix, x_matrix,
                                          character_gamma_matrix,
                                          character_beta_matrix, jx, 
                                          gibbs_sampler, priors) {
  gibbs_sampler <- get_gibbs_settings(equation = colnames(character_gamma_matrix)[jx])

  priors_j <- construct_priors_j(
    priors, character_gamma_matrix, character_beta_matrix, jx
  )

  # pre-define matrices for saving
  out <- list()
  out$beta_jw <- vector("list", gibbs_sampler$nsave)
  out$theta_jw <- vector("list", gibbs_sampler$nsave)
  out$gamma_jw <- vector("list", gibbs_sampler$nsave)
  out$omega_jw <- vector("list", gibbs_sampler$nsave)
  out$omega_tilde_jw <- vector("list", gibbs_sampler$nsave)
  count_accepted <- matrix(0, gibbs_sampler$ndraws, 1)

  ##### 1. Initialize sampler:
  # get starting value for Metropolis-Hastings algorithm
  initial_parameter <- initialize_sampler_informative(
    y_matrix,
    x_matrix,
    character_gamma_matrix,
    character_beta_matrix,
    jx
  )

  gamma_jw <- initial_parameter$gamma_jw
  gamma_jw_1 <- gamma_jw

  cholesky_of_inverse_hessian <- initial_parameter$cholesky_of_inverse_hessian

  # Get additional starting value for the first step for omega_jw
  omega_jw <- initial_omega_j(
    y_matrix, x_matrix, character_gamma_matrix,
    character_beta_matrix, jx, gamma_jw
  )

  gx <- 1 # initial value for saved draws

  #### Start Gibbs sampler
  for (wx in 1:gibbs_sampler$ndraws) {
    ##### 2. Draw Theta_j from multivariate normal distribution
    results_draw_theta_j <- draw_theta_j_informative(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
      jx, gamma_jw, omega_jw, priors_j
    )

    # Get theta matrix
    theta_jw <- results_draw_theta_j$theta_mat_jw

    ##### 3. Draw Omega_j from inverse Wishart distribution
    results_draw_omega_j <- draw_omega_j_informative(
      y_matrix, x_matrix, character_gamma_matrix, character_beta_matrix,
      jx, gamma_jw, theta_jw, priors_j
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
      priors_j
    )

    # Set count to 1 if the step has been accepted
    if (!identical(gamma_jw, gamma_jw_1)) {
      count_accepted[wx] <- 1
    }
    # Set gamma_jw_1 for next iteration
    gamma_jw_1 <- gamma_jw

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
#' `initialize_sampler_informative` initializes the sampler for the
#' Metropolis-Hastings algorithm. It obtains the initial \eqn{gamma} parameters
#' by numerically maximizing the target function. It then calculates the
#' Cholesky factor \eqn{L} of the inverse of the Hessian \eqn{M^{-1}} of the
#' target function.
#' This Cholesky factor, along with the gamma parameter for equation \eqn{j}, is
#' returned. The Cholesky factor is used to draw the candidate gamma in the MH
#' algorithm.
#'
#' @inheritParams draw_parameters_j_informative
#'
#' @return A list containing the initial parameters for gamma
#' (`gamma_jw`) and the Cholesky factor of the
#' inverse of the Hessian (`cholesky_of_inverse_hessian`) if
#' `number_endogenous_in_j` is greater than 0.
#' If not, only the `gamma_jw` parameter is returned as 0.
#' @keywords internal
initialize_sampler_informative <- function(y_matrix, x_matrix,
                                           character_gamma_matrix,
                                           character_beta_matrix, jx) {
  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))
  if (number_endogenous_in_j == 0) {
    return(list(
      gamma_jw = 0,
      cholesky_of_inverse_hessian = NA
    ))
  } else {
    # Maximize target function to obtain initial conditions for MH-algorithm
    optimize_residuals <- stats::optim(
      par = matrix(0, number_endogenous_in_j, 1),
      fn = target_j_informative_initial,
      y_matrix = y_matrix,
      x_matrix = x_matrix,
      character_gamma_matrix = character_gamma_matrix,
      character_beta_matrix = character_beta_matrix,
      jx = jx,
      hessian = TRUE,
      method = "BFGS"
    )

    # Use maximum as initial condition
    gamma_jw <- optimize_residuals$par
    # Use inverse of Hessian to approximate dispersion of target function
    inverse_hessian <- Matrix::solve(optimize_residuals$hessian)
    # Cholesky factor of inverse of Hessian
    cholesky_of_inverse_hessian <- t(Matrix::chol(inverse_hessian))

    list(
      gamma_jw = gamma_jw,
      cholesky_of_inverse_hessian = cholesky_of_inverse_hessian
    )
  }
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
#' @keywords internal
draw_gamma_j_informative <- function(y_matrix, x_matrix, character_gamma_matrix,
                                     character_beta_matrix, jx,
                                     gamma_jw, tau,
                                     cholesky_of_inverse_hessian, omega_jw,
                                     theta_jw, priors_j) {
  number_endogenous_in_j <- length(grep("gamma", character_gamma_matrix[, jx]))
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
      priors_j
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
      priors_j
    )
    # Acceptance probability
    alpha <- min(
      1,
      exp(target_evaluation_candidate - target_evaluation_previous)
    )

    # Accept-reject step
    if (!is.nan(alpha) && alpha > stats::runif(n = 1)) {
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
#' @keywords internal
draw_omega_j_informative <- function(y_matrix, x_matrix, character_gamma_matrix,
                                     character_beta_matrix, jx, gamma_jw,
                                     theta_jw, priors_j) {
  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))
  # number of exogenous, predetermined variables + intercept
  number_of_observations <- nrow(y_matrix)

  if (number_endogenous_in_j == 0) {
    z_matrix_j <- as.matrix(y_matrix[, jx])

    # if number_endogenous_in_j=0 use identity when constructing Aj matrix
    a_matrix_j <- 1
  } else {
    y_matrix_j <- construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)
    z_matrix_j <- construct_z_matrix_j(
      gamma_jw, y_matrix, y_matrix_j, jx
    )

    a_matrix_j <- diag((number_endogenous_in_j + 1))
    a_matrix_j[, 1] <- c(1, -gamma_jw)
  }

  # Compute scale parameter matrix
  omega_hat <- t(Matrix::solve(a_matrix_j)) %*%
    t(z_matrix_j - x_matrix %*% theta_jw) %*%
    (z_matrix_j - x_matrix %*% theta_jw) %*% Matrix::solve(a_matrix_j)

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
#'
#' @return List containing theta_jw and beta_jw
#' @keywords internal
draw_theta_j_informative <- function(y_matrix, x_matrix, character_gamma_matrix,
                                     character_beta_matrix, jx, gamma_jw,
                                     omega_jw, priors_j) {
  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))
  number_of_exogenous <- nrow(character_beta_matrix)

  if (number_endogenous_in_j == 0) {
    z_matrix_j <- as.matrix(y_matrix[, jx])
    # if number_endogenous_in_j=0 use identity when constructing Aj matrix
    a_matrix_j <- 1
  } else {
    y_matrix_j <- construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)
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
    construct_theta_bar_j(x_matrix, z_matrix_j, priors_j, omega_tilde_jw)
  )
  theta_bar <- theta_mat$theta_bar
  xi_bar <- theta_mat$xi_bar

  # Permute theta_hat such that the first block consists of all free
  # parameters and the second block contains all parameters that are
  # restricted to zero.
  # Zero restrictions only on betas, i.e. first column of theta_hat
  # Choose free parameters from first column (this is P')
  permutation_matrix <- matrix(0, length(theta_bar), length(theta_bar))
  # Find the indices of elements in equation j that are betas
  fpos <- grep("^0", character_beta_matrix[, jx], invert = TRUE)
  for (ix in seq_along(fpos)) {
    permutation_matrix[ix, fpos[ix]] <- 1
  }

  # Permute zero restrictions to the end
  # Find the indices of elements in equation j that are 0
  fposend <- grep("\\b0\\b", character_beta_matrix[, jx])

  permute_from_row <- length(fpos) + 1
  seperate_blocks_at <- length(theta_bar) - length(fposend)

  for (ix in seq_along(fposend)) {
    permutation_matrix[(seperate_blocks_at + ix), fposend[ix]] <- 1
  }

  if (number_endogenous_in_j != 0) {
    # Leave free parameters of other columns at their place
    permutation_matrix[
      permute_from_row:seperate_blocks_at,
      (length(character_beta_matrix[, jx]) + 1):length(theta_bar)
    ] <-
      diag(length(theta_bar) - length(character_beta_matrix[, jx]))
  }
  # Permute theta_bar
  theta_p <- permutation_matrix %*% theta_bar

  # Construct the two blocks
  theta_p1 <- theta_p[1:seperate_blocks_at]
  theta_p2 <- theta_p[-(1:seperate_blocks_at)]

  # Compute unrestricted posterior variance
  xi <- xi_bar

  # Permute xi
  xi_p <- permutation_matrix %*% xi %*% t(permutation_matrix)

  # Construct the two blocks
  xi_p11 <- xi_p[1:seperate_blocks_at, 1:seperate_blocks_at, drop = FALSE]
  if (length(theta_p2) != 0) {
    xi_p12 <- xi_p[
      1:seperate_blocks_at,
      (seperate_blocks_at + 1):length(theta_bar),
      drop = FALSE
    ]
    xi_p21 <- t(xi_p12)
    xi_p22 <- xi_p[
      (seperate_blocks_at + 1):length(theta_bar),
      (seperate_blocks_at + 1):length(theta_bar),
      drop = FALSE
    ]
    # Compute update of posterior mean and posterior variance
    theta_tilde <- theta_p1 - xi_p12 %*% Matrix::solve(xi_p22) %*% theta_p2
    xi_tilde <- xi_p11 - xi_p12 %*% Matrix::solve(xi_p22) %*% xi_p21
  } else {
    theta_tilde <- theta_p1
    xi_tilde <- xi_p11
  }

  theta_pw1 <- multivariate_norm(n = 1, theta_tilde, xi_tilde)

  # Construct full theta_p vector
  theta_pw <- c(theta_pw1, matrix(0, length(theta_p2), 1))

  # Permute back to original ordering
  theta_jw <- t(permutation_matrix) %*% theta_pw

  # Select beta vector
  exo_in_jx <-
    character_beta_matrix[which(character_beta_matrix[, jx] != 0), jx]
  beta_jw <- theta_pw1[seq_along(exo_in_jx)]

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
#' @keywords internal
target_j_informative <- function(y_matrix, x_matrix, character_gamma_matrix,
                                 character_beta_matrix, jx, gamma_jw, omega_jw,
                                 theta_jw, priors_j) {
  y_matrix_j <- construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)
  if (anyNA(y_matrix_j)) {
    return(NA)
  }

  gamma_count <- sum(grepl("gamma", character_gamma_matrix[, jx]))
  # Check number of expected gamma_jw
  if (gamma_count != length(gamma_jw)) {
    stop("The number of gamma parameters does not match the number
        of expected parameters.")
  }

  # Get number of endogenous variables in equation j
  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))

  if (number_endogenous_in_j == 0) {
    z_matrix_j <- as.matrix(y_matrix[, jx])

    # if number_endogenous_in_j=0 use identity when constructing Aj matrix
    a_matrix_j <- 1
  } else {
    y_matrix_j <- construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)
    z_matrix_j <- construct_z_matrix_j(
      gamma_jw, y_matrix, y_matrix_j, jx
    )

    a_matrix_j <- diag((number_endogenous_in_j + 1))
    a_matrix_j[, 1] <- c(1, -gamma_jw)
  }

  # explicit gamma priors specified
  if (!is.null(priors_j[["gamma_mean"]]) && !is.null(priors_j[["gamma_vcv"]])) {
    # Evaluate log of target function
    # (multiply by -1: maximize instead of minimize)
    target_result <-
      -log(
        multivariate_norm_pdf(
          gamma_jw,
          mu = priors_j[["gamma_mean"]], sigma = priors_j[["gamma_vcv"]]
        )
      )
    +
      0.5 * sum(diag(t(Matrix::solve(a_matrix_j)) %*%
      t(z_matrix_j - x_matrix %*% theta_jw) %*%
      (z_matrix_j - x_matrix %*% theta_jw) %*%
      Matrix::solve(a_matrix_j) %*% Matrix::solve(omega_jw)))
  } else {
    # Evaluate log of target function
    # (multiply by -1: maximize instead of minimize)
    target_result <-
      0.5 * sum(diag(t(Matrix::solve(a_matrix_j)) %*%
        t(z_matrix_j - x_matrix %*% theta_jw) %*%
        (z_matrix_j - x_matrix %*% theta_jw) %*%
        Matrix::solve(a_matrix_j) %*% Matrix::solve(omega_jw)))
  }
  target_result
}

#' Compute the target function for the jth equation
#'
#' `target_j_informative_initial` calculates the target function used in the
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
#' @keywords internal
target_j_informative_initial <- function(y_matrix, x_matrix,
                                         character_gamma_matrix,
                                         character_beta_matrix, jx, gamma_jw) {
  y_matrix_j <- construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)
  if (anyNA(y_matrix_j)) {
    return(NA)
  }

  gamma_count <- sum(grepl("gamma", character_gamma_matrix[, jx]))
  # Check number of expected gamma_jw
  if (gamma_count != length(gamma_jw)) {
    stop("The number of gamma parameters does not match the number
        of expected parameters.")
  }

  number_of_observations <- nrow(y_matrix)
  # number of exogenous, predetermined variables + intercept
  number_of_exogenous <- nrow(character_beta_matrix)

  # Get numnber of endogenous variables in equation j
  number_endogenous_in_j <-
    length(grep("gamma", character_gamma_matrix[, jx]))

  if (number_endogenous_in_j == 0) {
    z_matrix_j <- as.matrix(y_matrix[, jx])

    # if number_endogenous_in_j=0 use identity when constructing Aj matrix
    a_matrix_j <- 1
  } else {
    y_matrix_j <- construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)
    z_matrix_j <- construct_z_matrix_j(
      gamma_jw, y_matrix, y_matrix_j, jx
    )

    a_matrix_j <- diag((number_endogenous_in_j + 1))
    a_matrix_j[, 1] <- c(1, -gamma_jw)
  }

  beta_hat_j <- construct_beta_hat_j_matrix(
    x_matrix, z_matrix_j, character_beta_matrix, jx
  )

  pi_hat_0 <- construct_pi_hat_0(x_matrix, z_matrix_j)

  # Compute theta_hat matrix
  theta_hat <- cbind(beta_hat_j, pi_hat_0)

  # omega_hat <- t(z_matrix_j - x_matrix %*% theta_hat) %*%
  #  (z_matrix_j - x_matrix %*% theta_hat) / number_of_observations
  #
  # Evaluate log of target function
  # (multiply by -1: maximize instead of minimize)
  # target_result <- -dnorm(gamma_jw, mean = 0, sd = 100, log = TRUE) + 0.5*sum(diag(t(Matrix::solve(a_matrix_j))%*%t(z_matrix_j - x_matrix %*% theta_hat) %*%
  #                                 (z_matrix_j - x_matrix %*% theta_hat)%*%Matrix::solve(a_matrix_j)%*%
  #                             Matrix::solve(omega_hat)))
  #
  target_result <- ((number_of_observations - number_of_exogenous) / 2) *
    log(Matrix::det(t(z_matrix_j - x_matrix %*% theta_hat) %*%
      (z_matrix_j - x_matrix %*% theta_hat)))
  target_result
}

#' Compute initial omega for the jth equation
#'
#' `initial_omega_j` computes OLS quantity for initial omega_j using initialized
#' gamma_j'
#'
#' @inheritParams draw_parameters_j_informative
#' @inheritParams draw_gamma_j_informative
#'
#' @return The function returns the evaluation of the target function,
#' which is used to decide whether to accept or reject proposed states
#' in the MH algorithm. Returns NA if there are no gamma parameters.
#' @keywords internal
initial_omega_j <- function(y_matrix, x_matrix, character_gamma_matrix,
                            character_beta_matrix, jx, gamma_jw) {
  gamma_count <- sum(grepl("gamma", character_gamma_matrix[, jx]))

  if (gamma_count == 0) {
    z_matrix_j <- as.matrix(y_matrix[, jx])
  } else {
    y_matrix_j <- construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)

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
    x_matrix, z_matrix_j, character_beta_matrix, jx
  )

  pi_hat_0 <- construct_pi_hat_0(x_matrix, z_matrix_j)

  # Compute theta_hat matrix
  theta_hat <- cbind(beta_hat_j, pi_hat_0)

  # Evaluate log of target function
  # (multiply by -1: maximize instead of minimize)
  omega_hat <- t(z_matrix_j - x_matrix %*% theta_hat) %*%
    (z_matrix_j - x_matrix %*% theta_hat)

  omega_hat
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
