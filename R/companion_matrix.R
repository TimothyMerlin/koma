#' Construct Companion Matrices for Dynamic SEM
#'
#' @param posterior A list containing gamma_matrix, beta_matrix,  sigma_matrix
#' and phi_matrix derived from the LIBA estimates.
#' @param exogenous_variables A character vector of names of exogenous
#' variables.
#'
#' @return A list containing matrices in companion form: phi_matrix,
#' beta_tilde_matrix, gamma_matrix, c_matrix, and values n and p.
#' @keywords internal
construct_companion_matrix <- function(posterior, exogenous_variables) {
  phi_matrix <- posterior$phi_matrix
  gamma_matrix <- posterior$gamma_matrix
  beta_matrix <- posterior$beta_matrix

  # Construct B_tilde matrix and constant
  indices_exogenous_variables <-
    which(rownames(beta_matrix) %in% exogenous_variables)
  beta_tilde_matrix <- NULL
  # Case: e.g. AR(p)
  if (length(indices_exogenous_variables) != 0) {
    beta_tilde_matrix <- beta_matrix[indices_exogenous_variables, , drop = FALSE]
  }
  constant <- beta_matrix[1, ]

  # Stack Phis column wise
  phi_matrix_stacked <- do.call(rbind, phi_matrix)

  n <- dim(phi_matrix_stacked)[2]
  if (!is.null(n)) {
    p <- dim(phi_matrix_stacked)[1] / n

    dimdiff <- n * (p - 1)

    companion_phi <- cbind(
      phi_matrix_stacked,
      rbind(diag(dimdiff), matrix(0, n, dimdiff))
    )
    # Case: e.g. AR(p)
    if (length(indices_exogenous_variables) != 0) {
      companion_beta_tilde <- cbind(
        beta_tilde_matrix,
        matrix(0, dim(beta_tilde_matrix)[1], dimdiff)
      )
    } else {
      companion_beta_tilde <- NULL
    }
    companion_gamma <- cbind(
      rbind(gamma_matrix, matrix(0, dimdiff, n)),
      rbind(matrix(0, n, dimdiff), diag(dimdiff))
    )
    companion_c <- t(c(constant, matrix(0, 1, dimdiff)))
  } else {
    # Case where there is only one equation or no lagged variables
    p <- 1
    n <- ncol(gamma_matrix)

    companion_phi <- phi_matrix_stacked
    companion_beta_tilde <- beta_tilde_matrix
    companion_gamma <- gamma_matrix
    companion_c <- t(constant)
  }

  list(
    phi_matrix = companion_phi,
    beta_tilde_matrix = companion_beta_tilde,
    gamma_matrix = companion_gamma,
    c_matrix = companion_c,
    n = n,
    p = p
  )
}

#' Construct Reduced Form Parameters from Companion Matrix
#'
#' This function computes the reduced form parameters companion_pi,
#' companion_theta, and companion_d using the provided companion_matrix.
#'
#' The companion matrix \eqn{\Gamma_c} is block-diagonal with \eqn{\Gamma} and an
#' identity matrix, so its inverse has \eqn{\Gamma^{-1}} in the first block.
#' Only the first \eqn{n} columns of each matrix are multiplied by
#' \eqn{\Gamma^{-1}}; the remaining columns stay as they are.
#'
#' @param companion_matrix A list containing the matrices beta_tilde_matrix,
#' phi_matrix, gamma_matrix, and c_matrix used to calculate the reduced form
#' parameters, and the number of equations n.
#'
#' @return A list containing the matrices companion_pi, companion_theta, and
#' companion_d that represent the reduced form parameters
#' @keywords internal
construct_reduced_form <- function(companion_matrix) {
  n <- companion_matrix$n
  first <- seq_len(n)
  gamma_inv <- solve(companion_matrix$gamma_matrix[first, first, drop = FALSE])
  times_inv <- function(m) {
    if (is.null(m)) {
      return(NULL)
    }
    m[, first] <- m[, first, drop = FALSE] %*% gamma_inv
    m
  }

  list(
    companion_pi = times_inv(companion_matrix$beta_tilde_matrix),
    companion_theta = times_inv(companion_matrix$phi_matrix),
    companion_d = times_inv(companion_matrix$c_matrix)
  )
}
