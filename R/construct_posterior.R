#' Construct Posterior Matrices from LIBA Estimates
#'
#' This function constructs posterior system matrices from LIBA estimates.
#' It produces the structural coefficient matrices \eqn{\Gamma} and \eqn{B},
#' the implied reduced-form coefficient matrix \eqn{\Phi}, the structural shock
#' covariance matrix \eqn{\Sigma}, and the reduced-form innovation covariance
#' matrix \eqn{\Omega = (\Gamma^{-1})'\Sigma\Gamma^{-1}}.
#'
#' Identity equations are incorporated via `update_estimates_with_weights()`.
#' Structural shocks for identities are assumed to have zero variance, implying
#' corresponding zero rows and columns in \eqn{\Sigma}.
#'
#' @param estimate A draw that contains beta, gamma and omega tilde estimates.
#' @inheritParams forecast_draw
#'
#' @return A list containing posterior system matrices:
#'   * `gamma_matrix`: Posterior \eqn{\Gamma} (n x n) structural coefficient matrix.
#'   * `beta_matrix`: Posterior \eqn{B} (k x n) exogenous coefficient matrix.
#'   * `phi_matrix`: List of lagged endogenous coefficient matrices (n x n).
#'   * `sigma_matrix`: Structural shock covariance matrix \eqn{\Sigma} (n x n).
#'   * `omega_matrix`: Reduced-form innovation covariance matrix
#'     \eqn{\Omega = (\Gamma^{-1})'\Sigma\Gamma^{-1}} (n x n).
#'
#' @keywords internal
construct_posterior <- function(sys_eq, estimate) {
  call <- rlang::caller_env()

  posterior <-
    update_estimates_with_weights(
      sys_eq$identities,
      estimate$gamma_matrix,
      estimate$beta_matrix
    )

  # Check if the gamma and beta matrices have values in the expected indices
  gamma_mismatch_indices <- which(
    (posterior$gamma_matrix == 0) !=
      (sys_eq$character_gamma_matrix == 0),
    arr.ind = TRUE
  )

  beta_mismatch_indices <- which(
    (posterior$beta_matrix == 0) !=
      (sys_eq$character_beta_matrix == 0),
    arr.ind = TRUE
  )

  indices_to_string <- function(indices) {
    out <- paste("column", indices[, "col"], "and row", indices[, "row"])
    names(out) <- rep("*", length(out))
    out
  }

  # Construct the error message for gamma mismatches
  if (nrow(gamma_mismatch_indices) > 0) {
    cli::cli_abort(
      c(
        "The posterior gamma matrix has zeros at different indices compared to
        the character gamma matrix at:",
        cli::format_bullets_raw(indices_to_string(gamma_mismatch_indices))
      ),
      call = call
    )
  }

  # Construct the error message for beta mismatches
  if (nrow(beta_mismatch_indices) > 0) {
    cli::cli_abort(
      c(
        "The posterior beta matrix has zeros at different indices compared to
        the character beta matrix at:",
        cli::format_bullets_raw(indices_to_string(beta_mismatch_indices))
      ),
      call = call
    )
  }

  phi_matrix <- construct_phi(sys_eq, posterior$beta_matrix)

  sigma_matrix <- estimate$sigma_matrix
  dimnames(sigma_matrix) <- dimnames(posterior$gamma_matrix)
  gamma_matrix_inv <- solve(posterior$gamma_matrix)
  omega_matrix <- t(gamma_matrix_inv) %*% sigma_matrix %*% gamma_matrix_inv

  # Validate Sigma matrix
  if (!is.matrix(sigma_matrix)) {
    cli::cli_abort(
      "{.arg sigma_matrix} must be a matrix."
    )
  }

  if (nrow(sigma_matrix) != ncol(sigma_matrix)) {
    cli::cli_abort(
      "{.arg sigma_matrix} must be square. Found {nrow(sigma_matrix)} x
     {ncol(sigma_matrix)}."
    )
  }

  if (nrow(sigma_matrix) != nrow(posterior$gamma_matrix)) {
    cli::cli_abort(
      "{.arg sigma_matrix} and {.arg gamma_matrix} have incompatible dimensions:
     Sigma is {nrow(sigma_matrix)} x {ncol(sigma_matrix)}, while Gamma is
     {nrow(posterior$gamma_matrix)} x {ncol(posterior$gamma_matrix)}."
    )
  }


  id_idx <- match(names(sys_eq$identities),
    colnames(sigma_matrix),
    nomatch = 0
  )

  if (any(id_idx > 0) && any(diag(sigma_matrix)[id_idx] != 0)) {
    cli::cli_abort(
      "Identity equations must have zero variance in {.arg sigma_matrix}."
    )
  }

  list(
    gamma_matrix = posterior$gamma_matrix,
    beta_matrix = posterior$beta_matrix,
    phi_matrix = phi_matrix,
    sigma_matrix = sigma_matrix,
    omega_matrix = omega_matrix
  )
}

#' Update Gamma and Beta Matrices with Identity Weights
#'
#' This function injects identity weights into the structural coefficient
#' matrices. For each identity component, it finds the target entry encoded in
#' the theta name (e.g., "theta6_4") and replaces the corresponding value in
#' \eqn{\Gamma} or \eqn{B}.
#'
#' @param identities A list of identity equations.
#' @param gamma_matrix Initial \eqn{\Gamma} matrix (structural coefficients).
#' @param beta_matrix Initial \eqn{B} matrix (exogenous coefficients).
#'
#' @return A list containing:
#'   - `gamma_matrix`: \eqn{\Gamma} updated with identity weights.
#'   - `beta_matrix`: \eqn{B} updated with identity weights.
#'
#' @seealso
#' \code{\link{construct_posterior}} for a function that uses this function's
#' outputs.
#' @keywords internal
update_estimates_with_weights <- function(identities, gamma_matrix, beta_matrix) {
  # weights have to be defined before matrices can be updated
  contains_null <- sapply(
    identities,
    function(id) any(sapply(id$weights, is.null))
  )
  if (any(contains_null)) {
    missing_eqs <- paste(names(contains_null[contains_null]), collapse = ", ")
    cli::cli_abort(
      c(
        "x" = "Missing weights in the following identity equations:",
        ">" = "{missing_eqs}",
        "!" = "Please define all weights before continuing."
      ),
      call = rlang::caller_env()
    )
  }

  for (identity_equation in identities) {
    components <- names(identity_equation$components)
    for (idx in seq_along(components)) {
      component <- components[idx]
      character_weight <- identity_equation$components[[component]]
      matrix <- identity_equation$matrix[[idx]]
      value_weight <- identity_equation$weights[[idx]]

      indices <- gsub("[^0-9_]", "", character_weight)
      indices <- unlist(strsplit(indices, "_"))
      indices <- as.integer(indices)
      col_index <- indices[1]
      row_index <- indices[2]

      if (matrix == "gamma") {
        gamma_matrix[row_index, col_index] <- -value_weight
      } else if (matrix == "beta") {
        beta_matrix[row_index, col_index] <- value_weight
      } else {
        (
          cli::cli_abort(
            c(
              "x" = "Matrix {matrix} not recognized.",
              ">" = "Please check the matrix name in the identity equation."
            ),
            call = rlang::caller_env()
          )
        )
      }
    }
  }

  list(
    gamma_matrix = gamma_matrix,
    beta_matrix = beta_matrix
  )
}


#' Construct a Dynamic SEM (Structural Equation Model) Phi Matrix
#'
#' This function constructs the list of lagged endogenous coefficient matrices
#' \eqn{\Phi(1), \ldots, \Phi(L)} from the system of equations and the beta
#' matrix. It identifies lagged endogenous regressors in each equation and maps
#' their coefficients into the corresponding \eqn{\Phi(\ell)} matrix in the
#' dynamic SEM:
#' \eqn{Y\Gamma = \tilde{X}\tilde{B} + Y_{t-1}\Phi(1) + \cdots + Y_{t-p}\Phi(L) + U.}
#'
#' @param sys_eq A list containing the system of equations. Must
#' include `$equations` with the equations of the system,
#' `$endogenous_variables` with the names of the endogenous variables, and
#' `$total_exogenous_variables` with the names of all exogenous variables.
#' @param beta_matrix A numeric matrix of beta coefficients corresponding to the
#' exogenous variables in the system of equations.
#'
#' @return A named list of \eqn{n \times n} matrices, one per lag \eqn{\ell},
#' where each matrix \eqn{\Phi(\ell)} contains the coefficients on the
#' \eqn{\ell}-lagged endogenous variables. The list names correspond to lag
#' orders (as character strings).
#' @keywords internal
construct_phi <- function(sys_eq, beta_matrix) { # nolint: cyclomatic_complexity_linter
  # Use the following system
  # Y * Gamma = X_tilde B_tilde + Y(-1) * Phi(1) + ... + Y(-p) * Phi(L) + U

  equations <- sys_eq$equations
  endogenous_variables <- sys_eq$endogenous_variables
  exogenous_variables <- sys_eq$total_exogenous_variables
  n <- length(endogenous_variables)

  #### Find lagged endogenous variables
  indxl <- list()
  for (jx in 1:n) {
    for (ix in 1:n) {
      # Find lagged endogenous variable
      if (any(grepl(paste0("^", endogenous_variables[ix]), exogenous_variables))) {
        # Position of endogenous variables in exogenous_variables vector
        indx <- grep(paste0("^", endogenous_variables[ix]), exogenous_variables)
        # Lagged endogenous variables ix
        vlagi <- exogenous_variables[indx]

        gx <- 1
        # Find if lagged endogenous variable present in equation jx
        for (lx in regmatches(vlagi, regexpr("[0-9]", vlagi))) {
          if (any(grepl(paste0(endogenous_variables[ix], ".L(", lx, ")"), equations[jx], fixed = TRUE))) {
            indxl[[lx]][[as.character(jx)]][[as.character(ix)]] <-
              c(indx[gx], ix)
            gx <- gx + 1
          }
        }
      }
    }
  }

  #### Construct Phi(L) matrix
  phi_matrix <- vector("list")
  for (lx in names(indxl)) {
    matf <- matrix(0, n, n)
    for (jx in names(indxl[[lx]])) {
      for (ix in names(indxl[[lx]][[jx]])) {
        matf[indxl[[lx]][[jx]][[ix]][2], as.numeric(jx)] <-
          beta_matrix[indxl[[lx]][[jx]][[ix]][1], as.numeric(jx)]
      }
    }
    phi_matrix[[lx]] <- matf
  }

  phi_matrix
}
