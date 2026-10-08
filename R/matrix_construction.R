#' Cache fixed data for an equation
#'
#' @inheritParams draw_parameters_j
#' @return A list containing the endogenous subset, restricted exogenous
#'   subset, beta positions, parameter counts, and observation count.
#' @keywords internal
construct_equation_data <- function(y_matrix, x_matrix, character_gamma_matrix,
                                    character_beta_matrix, jx) {
  gamma_count <- length(grep("gamma", character_gamma_matrix[, jx]))
  beta_positions <- grep("^0", character_beta_matrix[, jx], invert = TRUE)
  list(
    y_matrix_j = if (gamma_count > 0) {
      construct_y_matrix_j(y_matrix, character_gamma_matrix, jx)
    } else {
      NA
    },
    x_b = x_matrix[, beta_positions, drop = FALSE],
    beta_positions = beta_positions,
    gamma_count = gamma_count,
    number_of_observations = nrow(y_matrix),
    number_of_exogenous = nrow(character_beta_matrix)
  )
}

#' Constructs a matrix of endogenous variables appearing in equation j
#'
#' This extracts a \eqn{(T x n_j)} matrix of endogenous variables appearing in
#' equation \eqn{j}, with \eqn{T} being the number of observations and
#' \eqn{n_j} the number of endogenous variables in equation \eqn{j}.
#'
#' @param y_matrix A \eqn{(T \times n)} matrix \eqn{Y}, where \eqn{T} is the
#' number of observations and \eqn{n} the number of equations, i.e. endogenous
#' variables.
#' @param character_gamma_matrix A matrix \eqn{\Gamma} that holds the
#' coefficients in character form for all equations. The dimensions of the
#' matrix are \eqn{(T \times n)}, where \eqn{T} is the number of
#' observations and \eqn{n} the number of equations.
#' @param jx The index of equation \eqn{j}.
#'
#' @return If equation \eqn{j} contains \eqn{\gamma} parameters, the function
#' returns a subset of \eqn{Y}.
#' If there are no gamma parameters, the function returns NA.
#' @keywords internal
construct_y_matrix_j <- function(y_matrix, character_gamma_matrix, jx) {
  indexes_of_gamma_parameters <- grep(
    "gamma",
    character_gamma_matrix[, jx]
  )
  y_matrix_j <- y_matrix[, indexes_of_gamma_parameters]

  if (length(indexes_of_gamma_parameters) == 0) {
    cli::cli_warn("Equation {jx} does not contain any gamma parameters. Returning NA.")
    return(NA)
  } else {
    return(y_matrix_j)
  }
}

#' Construct Z_j
#'
#' \eqn{Z_j = [ \, y_j - Y_j * \gamma_j, Y_j ] \,}
#'
#' @param gamma_parameters_j A \eqn{(n_j \times 1)} matrix with the parameters
#' of the \eqn{\gamma} matrix, where \eqn{n_j} is the number of endogenous
#' variables in equation \eqn{j}.
#' @param y_matrix A \eqn{(T \times n)} matrix \eqn{Y}, where \eqn{T} is the
#' number of observations and \eqn{n} the number of equations, i.e. endogenous
#' variables.
#' @param y_matrix_j A \eqn{(T \times n_j)} matrix of endogenous variables
#' appearing in equation \eqn{j}, with \eqn{T} being the number of observations
#' and \eqn{n_j} the number of endogenous variables in equation \eqn{j}.
#' @param jx The index of equation \eqn{j}.
#'
#' @return \eqn{Z_j} matrix with dimensions \eqn{T \times 2}
#' @keywords internal
construct_z_matrix_j <- function(gamma_parameters_j, y_matrix, y_matrix_j, jx) {
  if (anyNA(y_matrix_j)) {
    stop("y_matrix_j cannot contain NAs.")
  }
  if (anyNA(gamma_parameters_j)) {
    stop("gamma_parameters_j cannot contain NAs.")
  }

  z_matrix_j <- cbind(
    y_matrix[, jx] - y_matrix_j %*% as.matrix(gamma_parameters_j),
    y_matrix_j
  )

  if (anyNA(z_matrix_j)) {
    stop("z_matrix_j cannot contain NAs. Please review your arguments.")
  }
  return(z_matrix_j)
}

#' Construct \eqn{\hat{\beta_j}}
#'
#' \eqn{\hat{\beta_j} = (x_b'x_b)^{-1} x_b' Z_j}
#' with \eqn{x_b} being the predetermined variables and constant of equation
#' \eqn{j} and \eqn{Z_j = y_j - Y_j * \gamma_j}
#' @param x_matrix A \eqn{(T \times k)} matrix \eqn{X} of observations on
#' \eqn{k} exogenous variables.
#' @param z_matrix_j A \eqn{Z_j = y_j - Y_j * \gamma_j} matrix.
#' @param character_beta_matrix A matrix \eqn{\beta} that holds the
#' coefficients in character form for all equations. The dimensions of the
#' matrix are \eqn{(k \times n)}, where \eqn{k} is the number of
#' exogenous variables and \eqn{n} the number of equations.
#' @param jx The index of equation \eqn{j}.
#' @param xbtxb Precomputed \eqn{x_b'x_b}, where \eqn{x_b} is
#' \eqn{x_matrix} restricted to the columns kept for equation \eqn{j}. This
#' is invariant across Gibbs draws for a given equation, so it is computed
#' once per equation instead of on every call.
#'
#' @return \eqn{\hat{\beta_j}} with dimensions \eqn{k \times 1}.
#' @param equation_data Fixed equation subsets and counts returned by
#'   [construct_equation_data()]. The samplers compute this once per equation.
#' @keywords internal
construct_beta_hat_j_matrix <- function(x_matrix, z_matrix_j,
                                        character_beta_matrix, jx,
                                        xbtxb, equation_data) {
  beta_positions <- equation_data$beta_positions
  x_b <- equation_data$x_b
  number_of_exogenous <- equation_data$number_of_exogenous
  beta_hat_j <- matrix(0, number_of_exogenous, 1)
  if (length(beta_positions) == 0) {
    return(beta_hat_j)
  }
  beta_hat_b <- solve(xbtxb, crossprod(x_b, z_matrix_j[, 1]))

  beta_hat_j[beta_positions] <-
    beta_hat_b

  return(beta_hat_j)
}

#' Construct reduced form coefficients \eqn{\Pi_0}
#'
#' Computes the estimated \eqn{\Pi_0}, which is a \eqn{(k \times n_j)} matrix
#' of reduced form coefficients, where \eqn{n_j} is the number of endogenous
#' variables in equation \eqn{j}.
#'
#' @param x_matrix A \eqn{(T \times k)} matrix \eqn{X} of observations on
#' \eqn{k} exogenous variables.
#' @param z_matrix_j A \eqn{Z_j = y_j - Y_j * \gamma_j} matrix.
#' @param xtx Precomputed \eqn{x_matrix'x_matrix}. This is invariant across
#' Gibbs draws, so it is computed once instead of on every call.
#'
#' @return \eqn{\hat{\Pi_0}} with dimensions \eqn{(k \times n_j)}.
#' @keywords internal
construct_pi_hat_0 <- function(x_matrix, z_matrix_j, xtx) {
  rhs <- z_matrix_j[, -1]

  # Equation j has no other endogenous variables: Pi_0 is a (k x 0) matrix.
  # solve(A, B) errors on a zero-column B, so skip it rather than falling
  # back to the slower solve(A) %*% B for every call.
  if (NCOL(rhs) == 0) {
    return(matrix(nrow = ncol(x_matrix), ncol = 0))
  }

  pi_hat_0 <- solve(xtx, crossprod(x_matrix, rhs))

  return(pi_hat_0)
}

#' Construct \eqn{\hat{\Theta}_j}
#'
#' Computes the OLS estimate for \eqn{\hat{\Theta}_j}, which
#' is a \eqn{(k \times (1 + n_j))} matrix, where \eqn{n_j} is the number
#' of endogenous variables in equation \eqn{j} and k is the number of
#' exogenous variables.
#'
#' \eqn{\hat{\Theta}_j = (X'X)^{-1}X' Z_j}
#'
#' @param x_matrix A \eqn{(T \times k)} matrix \eqn{X} of observations on
#' \eqn{k} exogenous variables.
#' @param z_matrix_j A \eqn{Z_j = y_j - Y_j * \gamma_j} matrix.
#' @param xtx Precomputed \eqn{x_matrix'x_matrix}. This is invariant across
#' Gibbs draws, so it is computed once instead of on every call.
#'
#' @return \eqn{\hat{\Theta}_j} with dimensions \eqn{(k \times (1 + n_j))}.
#' @keywords internal
construct_theta_hat_j <- function(x_matrix, z_matrix_j, xtx) {
  theta_hat <- solve(xtx, crossprod(x_matrix, z_matrix_j))

  return(theta_hat)
}

#' Construct \eqn{\bar{\Theta}_j}
#'
#' Computes the posterior mean for \eqn{\hat{\Theta}_j} with informative
#' priors, which is a \eqn{(k \times (1 + n_j))} matrix, where \eqn{n_j} is
#' the number of endogenous variables in equation \eqn{j} and k is the number of
#' exogenous variables.
#'
#' \eqn{
#'  \bar{\Theta}_j = \overline{\Xi} \left(
#'    kron(\tilde{\Omega},X'X) \hat{\theta} +
#'    \underline{\Xi}^{-1} \underline{\theta}
#'    \right)
#' }
#'
#' @inheritParams draw_gamma_j_informative
#' @param x_matrix A \eqn{(T \times k)} matrix \eqn{X} of observations on
#' \eqn{k} exogenous variables.
#' @param z_matrix_j A \eqn{Z_j = y_j - Y_j * \gamma_j} matrix.
#' @param omega_tilde_jw A variance-covariance matrix
#'   \eqn{\tilde{\Omega}_j = A'_j \Omega_j A_j} for row \eqn{j}.
#' @param xtx Precomputed \eqn{x_matrix'x_matrix}. This is invariant across
#' Gibbs draws, so it is computed once instead of on every call.
#'
#' @return \eqn{\hat{\Theta}_j} with dimensions \eqn{(k \times (1 + n_j))}.
#' @keywords internal
construct_theta_bar_j <- function(x_matrix, z_matrix_j, priors_j,
                                  omega_tilde_jw, xtx) {
  # c() vectorizes matrix
  theta_hat <- c(construct_theta_hat_j(x_matrix, z_matrix_j, xtx))
  omega_kron_xtx <- kronecker(
    solve(omega_tilde_jw),
    xtx
  )
  xi_bar <- solve(
    omega_kron_xtx + priors_j$theta_precision
  )
  theta_bar <- xi_bar %*% (omega_kron_xtx %*% theta_hat +
    priors_j$theta_precision_mean)

  return(list(
    theta_bar = theta_bar,
    xi_bar = xi_bar
  ))
}

#' Order the elements of theta with the zero restrictions last
#'
#' Finds the order that moves the parameters of \eqn{\theta_j} restricted to
#' zero to the end. The zero restrictions are only on the betas, i.e. on the
#' first column of \eqn{\Theta_j}. The free betas come first, then the
#' parameters of the other columns in their original order, then the
#' restricted betas.
#'
#' Indexing with the returned order, `theta[permutation]` and
#' `xi[permutation, permutation]`, gives the same result as multiplying with
#' the permutation matrix \eqn{P}, \eqn{P \theta} and \eqn{P \Xi P'}, but is
#' much faster.
#'
#' @inheritParams construct_beta_hat_j_matrix
#' @param number_of_parameters The length of the vectorized \eqn{\Theta_j},
#' i.e. \eqn{k (1 + n_j)}.
#'
#' @return A list with `permutation`, the positions of the elements of theta
#' in the new order, and `seperate_blocks_at`, the number of free parameters.
#' @keywords internal
construct_theta_permutation <- function(character_beta_matrix, jx,
                                        number_of_parameters) {
  # Find the indices of elements in equation j that are betas
  fpos <- grep("^0", character_beta_matrix[, jx], invert = TRUE)
  # Find the indices of elements in equation j that are 0
  fposend <- grep("\\b0\\b", character_beta_matrix[, jx])

  # Leave free parameters of other columns at their place
  number_of_exogenous <- nrow(character_beta_matrix)
  other_columns <- number_of_exogenous +
    seq_len(number_of_parameters - number_of_exogenous)

  list(
    permutation = c(fpos, other_columns, fposend),
    seperate_blocks_at = number_of_parameters - length(fposend)
  )
}
