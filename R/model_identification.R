#' Identify Model Parameters from Character Matrices
#'
#' This function calculates the gamma and beta matrices based on the
#' given character gamma and beta matrices, along with identity weights.
#' It checks the identification of all stochastic equations and
#' verifies the order and rank conditions for model identification.
#'
#' @param character_gamma_matrix A character matrix representing the
#'   gamma structure of the model.
#' @param character_beta_matrix A character matrix representing the
#'   beta structure of the model.
#' @param identity_weights A list of identity weights to help construct
#'   constant vectors.
#' @param call The environment from which the error is called. Defaults to
#'   `rlang::caller_env()`, and is used to provide context in case of an error.
#'
#' @return NULL. This function is used for side effects, stopping execution
#'   if model identification conditions are not met.
#' @export
model_identification <- function(character_gamma_matrix,
                                 character_beta_matrix, identity_weights,
                                 call = rlang::caller_env()) {
  number_of_endogenous <- ncol(character_beta_matrix)
  number_of_exogenous <- nrow(character_beta_matrix)

  gamma_vec <- gamma_vectorization(
    character_gamma_matrix, identity_weights
  )

  # If there are no gamma parameters, the model has no simultaneity.
  # In this case, each equation is just a standalone regression,
  # so we can skip the identification check.
  if (any(is.na(gamma_vec))) {
    return(TRUE)
  }
  beta_vec <- beta_vectorization(character_beta_matrix)

  # Compute Gamma matrix
  gamma_parameters <- multivariate_norm(
    n = 1,
    matrix(0, dim(gamma_vec$transformation_matrix)[2], 1),
    diag(dim(gamma_vec$transformation_matrix)[2]) * 0.5
  )
  gamma_matrix <- vector_to_matrix(
    gamma_vec$transformation_matrix,
    gamma_parameters,
    gamma_vec$constant_vector,
    nrow = number_of_endogenous, ncol = number_of_endogenous
  )

  # Compute Beta matrix
  beta_parameters <- multivariate_norm(
    n = 1,
    matrix(0, dim(beta_vec$transformation_matrix)[2], 1),
    diag(dim(beta_vec$transformation_matrix)[2]) * 0.5
  )
  beta_matrix <- vector_to_matrix(
    beta_vec$transformation_matrix,
    beta_parameters,
    beta_vec$constant_vector,
    nrow = number_of_exogenous, ncol = number_of_endogenous
  )
  # Drop intercept from B matrix
  beta_matrix <- beta_matrix[-1, ]

  # number of exogenous variables (minus intercept)
  number_of_exogenous <- number_of_exogenous - 1
  number_of_identities <- length(identity_weights)

  # Check identification all stochastic equations
  rank_all <- list()
  order_all <- list()

  for (j in seq(1, (number_of_endogenous - number_of_identities))) {
    # Sort Gamma and B such that Gamma = [1,-gamma_j,0] and B=[beta_j 0]
    gammas <- rbind(
      gamma_matrix[gamma_matrix[, j] == 1, ],
      gamma_matrix[gamma_matrix[, j] != 0 & gamma_matrix[, j] != 1, ],
      gamma_matrix[gamma_matrix[, j] == 0, ]
    )
    betas <- rbind(
      beta_matrix[beta_matrix[, j] != 0, ],
      beta_matrix[beta_matrix[, j] == 0, ]
    )

    rr <- rbind(gammas, betas)
    r <- rr[rr[, j] == 0, -j, drop = FALSE]
    rank_all[[j]] <- list(
      "Fullfilled" = qr(r)$rank == (number_of_endogenous - 1),
      "Rank" = qr(r)$rank
    )
    order_all[[j]] <- list(
      "Fullfilled" = dim(r)[2] <= dim(r)[1],
      "# included endogenous" = dim(r)[2],
      "# excluded exogenous" = dim(r)[1]
    )
  }
  names(rank_all) <-
    setdiff(colnames(character_beta_matrix), names(identity_weights))
  names(order_all) <- names(rank_all)

  check_condition <- function(mat, condition_name, call = rlang::caller_env()) {
    if (sum(do.call(cbind, do.call(cbind, mat)[1, ])) !=
      (number_of_endogenous - number_of_identities)) {
      cli::cli({
        cli::cli_alert_danger(
          c(
            "The specified model is not identified because the {condition_name}
           condition is not satisfied!"
          )
        )
        cli::cli_verbatim(c(utils::capture.output(do.call(rbind, mat)), "\n"))
      })
      # throw the error
      cli::cli_abort(
        "Model identification error: {condition_name} condition not satisfied.",
        call = call
      )
    }
  }

  check_condition(order_all, "order", call = call)
  check_condition(rank_all, "rank", call = call)

  return(TRUE)
}

#' Vectorize Gamma Matrix
#'
#' Converts the character representation of the gamma matrix into
#' a transformation matrix and a constant vector, such that
#' vector = transformation_matrix %*% parameters + constant_vector
#' where vector = vec(character_gamma_matrix).
#'
#' @param character_gamma_matrix A character matrix representing the
#'   gamma structure of the model.
#' @param identity_weights A list of identity weights for adjusting
#'   constant vectors.
#' @return A list with three elements:
#'   \describe{
#'     \item{transformation_matrix}{A numeric matrix used for parameter
#' transformation.}
#'     \item{parameters}{A character vector of parameter names.}
#'     \item{constant_vector}{A numeric vector for constant terms.}
#'   }
#' @keywords internal
gamma_vectorization <- function(character_gamma_matrix, identity_weights) {
  number_of_endogenous <- ncol(character_gamma_matrix)
  character_vector <- c(character_gamma_matrix)

  parameters <- get_parameters(character_gamma_matrix, "gamma")

  if (length(parameters) == 0) {
    return(NA)
  }

  # transformation_matrix matrix in character form
  transformation_matrix <-
    matrix(0, number_of_endogenous^2, length(parameters))

  for (ix in seq_along(parameters)) {
    transformation_matrix[
      grep(parameters[ix], character_vector), ix
    ] <- -1
  }

  # transformation_matrix in numeric form
  transformation_matrix <-
    apply(transformation_matrix, 2, as.numeric)

  theta_parameters <- get_parameters(character_gamma_matrix, "theta")
  # constant_vector in character form
  constant_vector <- matrix(0, number_of_endogenous^2, 1)

  for (ix in seq_along(theta_parameters)) {
    constant_vector[
      grep(theta_parameters[ix], character_vector)
    ] <- theta_parameters[ix]
  }

  constant_vector[which(character_vector == 1)] <-
    character_vector[which(character_vector == 1)]

  constant_vector <- adjust_constant_vector(constant_vector, identity_weights)


  return(list(
    transformation_matrix = transformation_matrix,
    parameters = parameters,
    constant_vector = constant_vector
  ))
}

#' Adjust Constant Vector Using Identity Weights
#'
#' Adjusts a given constant vector based on a list of identity weights.
#' The identity weights are used to replace specific elements in the
#' constant vector with corresponding numeric values.
#'
#' @param constant_vector A numeric vector representing the constant terms to
#' be adjusted.
#' @param identity_weights A list where each element corresponds to a set of
#' identity rules.
#' @return The adjusted constant vector in numeric form with modified values
#' based on the identity weights.
adjust_constant_vector <- function(constant_vector, identity_weights) {
  # Iterate through indices in identity_weights
  for (idx in seq_along(identity_weights)) {
    # Get the names of elements in identity_weights at the current index
    weight_name <- names(identity_weights[[idx]]$weights)
    for (ix in weight_name) {
      # Find matching elements in constant_vector
      matches <- grep(ix, constant_vector)

      if (length(matches) > 0) {
        # Replace with corresponding numeric value
        constant_vector[matches] <-
          -as.numeric(identity_weights[[idx]]$weights[ix])
      }
    }
  }

  # constant vector in numeric form
  constant_vector <- apply(constant_vector, 2, as.numeric)
  return(constant_vector)
}

#' Vectorize Beta Matrix
#'
#' Converts the character representation of the beta matrix into
#' a transformation matrix and constant vector, such that
#' vector = transformation_matrix %*% parameters + constant_vector
#' where vector = vec(character_beta_matrix)
#'
#' @param character_beta_matrix A character matrix representing the
#'   beta structure of the model.
#' @return A list with three elements:
#'   \describe{
#'     \item{transformation_matrix}{A numeric matrix used for parameter
#'           transformation.}
#'     \item{parameters}{A character vector of parameter names.}
#'     \item{constant_vector}{A numeric vector for constant terms.}
#'   }
#' @keywords internal
beta_vectorization <- function(character_beta_matrix) {
  number_of_exogenous <- nrow(character_beta_matrix)
  number_of_endogenous <- ncol(character_beta_matrix)
  character_vector <- c(character_beta_matrix)

  parameters <- get_parameters(character_beta_matrix, "beta")

  # transformation matrix in numeric form
  transformation_matrix <- matrix(
    0, number_of_endogenous * number_of_exogenous, length(parameters)
  )
  for (ix in seq_along(parameters)) {
    transformation_matrix[grep(parameters[ix], character_vector), ix] <- 1
  }

  # constant_vector in numeric
  constant_vector <- matrix(0, number_of_endogenous * number_of_exogenous, 1)
  # constant_vector[grep("theta", character_vector)]

  return(list(
    transformation_matrix = transformation_matrix,
    parameters = parameters,
    constant_vector = constant_vector
  ))
}

#' Convert Vector to Matrix
#'
#' Given a transformation matrix, a vector of parameters, and a constant
#' vector, this function constructs a matrix with specified row and
#' column dimensions.
#' vector = transformation_matrix %*% parameters, constant_vector
#' where vector = vec(matrix)
#'
#' @param transformation_matrix A numeric matrix used for parameter
#' transformation.
#' @param parameters A numeric vector of parameters.
#' @param constant_vector A numeric vector for constant terms.
#' @param nrow Number of rows for the resulting matrix.
#' @param ncol Number of columns for the resulting matrix.
#' @return A numeric matrix with specified row and column dimensions.
#' @keywords internal
vector_to_matrix <- function(transformation_matrix, parameters, constant_vector,
                             nrow, ncol) {
  vec <- transformation_matrix %*% parameters + constant_vector

  return(matrix(vec, nrow, ncol))
}

#' Get Parameter Names from Character Matrix
#'
#' Extracts unique parameter names from a character matrix
#' based on a given pattern (such as "gamma" or "beta").
#'
#' @param character_matrix A character matrix to extract parameters from.
#' @param pattern A string pattern to identify parameter names.
#' @return A character vector of unique parameter names matching the pattern.
#' @keywords internal
get_parameters <- function(character_matrix, pattern) {
  # Flatten the matrix to get all elements as a vector
  elements <- as.vector(character_matrix)

  # Remove leading minus signs
  elements <- gsub("^-", "", elements)

  # Use grep to find all elements that start with the given prefix
  matching_elements <- grep(
    paste0("^", pattern, "([0-9]*_[0-9]*)"), elements,
    value = TRUE
  )

  # Return unique matching elements
  unique(as.character(matching_elements))
}
