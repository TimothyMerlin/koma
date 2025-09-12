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
#'
#' @return \eqn{\hat{\beta_j}} with dimensions \eqn{k \times 1}.
#' @keywords internal
construct_beta_hat_j_matrix <- function(x_matrix, z_matrix_j,
                                        character_beta_matrix, jx) {
  number_of_exogenous <- nrow(character_beta_matrix)

  indices_to_remove <- grep("\\b0\\b", character_beta_matrix[, jx])

  if (length(indices_to_remove) > 0) {
    x_b <- x_matrix[, -indices_to_remove, drop = FALSE]
  } else {
    x_b <- x_matrix # Keep the original matrix if no matches are found
  }
  beta_hat_b <- Matrix::solve(t(x_b) %*% x_b) %*% t(x_b) %*% z_matrix_j[, 1]

  beta_hat_j <- matrix(0, number_of_exogenous, 1)
  beta_hat_j[grep("^0", character_beta_matrix[, jx], invert = TRUE)] <-
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
#'
#' @return \eqn{\hat{\Pi_0}} with dimensions \eqn{(k \times n_j)}.
#' @keywords internal
construct_pi_hat_0 <- function(x_matrix, z_matrix_j) {
  # Compute pi_hat_0 matrix
  pi_hat_0 <- Matrix::solve(t(x_matrix) %*% x_matrix) %*%
    t(x_matrix) %*% z_matrix_j[, -1]

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
#'
#' @return \eqn{\hat{\Theta}_j} with dimensions \eqn{(k \times (1 + n_j))}.
#' @keywords internal
construct_theta_hat_j <- function(x_matrix, z_matrix_j) {
  theta_hat <-
    Matrix::solve(t(x_matrix) %*% x_matrix) %*% t(x_matrix) %*% z_matrix_j

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
#'
#' @return \eqn{\hat{\Theta}_j} with dimensions \eqn{(k \times (1 + n_j))}.
#' @keywords internal
construct_theta_bar_j <- function(x_matrix, z_matrix_j, priors_j,
                                  omega_tilde_jw) {
  # c() vectorizes matrix
  theta_hat <- c(construct_theta_hat_j(x_matrix, z_matrix_j))
  xi_bar <- Matrix::solve(
    Matrix::kronecker(
      Matrix::solve(omega_tilde_jw),
      t(x_matrix) %*% x_matrix
    ) + Matrix::solve(priors_j[["theta_vcv"]])
  )
  theta_bar <- xi_bar %*% (Matrix::kronecker(
    Matrix::solve(omega_tilde_jw), t(x_matrix) %*% x_matrix
  ) %*% theta_hat + Matrix::solve(priors_j[["theta_vcv"]]) %*%
    priors_j[["theta_mean"]])

  return(list(
    theta_bar = theta_bar,
    xi_bar = xi_bar
  ))
}
