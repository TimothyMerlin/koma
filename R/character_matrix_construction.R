#' Construct Gamma Matrix
#'
#' @description
#' Constructs a Gamma matrix based on the given system of equations and
#' endogenous variables.
#'
#' This function constructs a Gamma matrix, which is a diagonal matrix
#' representing the coefficients of endogenous variables in the system
#' of equations. The Gamma matrix is used in various econometric models.
#' It also returns the constructed weights and parameters.
#' @param equations A character string containing the system of equations,
#' where equations are separated by commas.
#' @param endogenous_variables A character vector representing the
#' endogenous variables.
#' @return A Gamma matrix.
#' @keywords internal
construct_gamma_matrix <- function(equations, endogenous_variables) {
  gamma_matrix <- diag(length(endogenous_variables))

  for (ix in seq_along(equations)) {
    equation <- equations[ix]
    # Remove weights from the equation
    equation <- gsub("\\([^\\)]+\\)\\*\\s*", "", equation)

    for (endogenous_variable in endogenous_variables[-ix]) {
      # This grepl function searches for the endogenous_variable in the
      # equation, excluding cases where the variable is part of a lag or
      # function call (e.g., current_account.L(1)).
      # The pattern uses a negative lookbehind to exclude
      # matches preceded by an opening parenthesis, and a negative lookahead to
      # exclude matches followed by a dot or opening parenthesis, ensuring that
      # only standalone instances of the variable are matched.
      pattern <- paste0("(?<!\\()\\b", endogenous_variable, "\\b(?![.(])")
      if (grepl(pattern, equation, perl = TRUE)) {
        variable <- grep(
          paste0("\\b", endogenous_variable, "\\b"),
          endogenous_variables
        )
        which_variable <-
          which(endogenous_variables %in% endogenous_variable)

        # if identity equation
        if (!grepl("\\~", equation)) {
          weight <- paste0("theta", ix, "_", variable)
          gamma_matrix[which_variable, ix] <-
            paste0("-", weight)

          # if stochastic equation
        } else {
          parameter <-
            paste0("gamma", ix, "_", variable)
          gamma_matrix[which_variable, ix] <-
            paste0("-", parameter)
        }
      }
    }
  }

  colnames(gamma_matrix) <- endogenous_variables
  rownames(gamma_matrix) <- endogenous_variables
  gamma_matrix
}

#' Construct Beta Matrix
#'
#' @description
#' Constructs the B matrix based on the given system of equations and the
#' exogenous variables.
#'
#' @param equations A character string containing the system of equations,
#' where equations are separated by commas.
#' @param exogenous_variables A character vector representing the
#' exogenous variables.
#' @return Beta matrix.
#' @keywords internal
construct_beta_matrix <- function(equations, exogenous_variables) {
  beta_matrix <- matrix(
    0,
    length(exogenous_variables),
    length(equations)
  )

  parameters <- list()

  for (ix in seq_along(equations)) {
    equation <- equations[ix]
    # Remove weights from the equation
    equation <- gsub("\\([^\\)]+\\)\\*\\s*", "", equation)

    for (exogenous_variable in exogenous_variables) {
      which_variable <- which(exogenous_variables %in% exogenous_variable)

      # Match exogenous variable exactly, even if it includes dots or
      # parentheses.
      escaped <- gsub("([.()])", "\\\\\\1", exogenous_variable)
      pattern <- paste0("(^|[^[:alnum:]_])", escaped, "([^[:alnum:]_]|$)")
      if (grepl(pattern, equation, perl = TRUE)) {
        if (grepl("\\~", equation)) {
          # if exogenous is a constant
          if (grepl(paste0("\\b", "constant", "\\b"), exogenous_variable)) {
            parameter <- paste0("constant", ix)
            parameters[[exogenous_variable]][ix] <- parameter
            beta_matrix[which_variable, ix] <- parameter
          } else {
            parameter <- paste0("beta", ix, "_", which_variable)
            parameters[[exogenous_variable]][ix] <- parameter
            beta_matrix[which_variable, ix] <- parameter
          }
        } else {
          weight <- paste0("theta", ix, "_", which_variable)
          beta_matrix[which_variable, ix] <- weight
        }
      }
    }
  }

  endogenous_variables <- get_endogenous_variables(equations)
  colnames(beta_matrix) <- endogenous_variables
  rownames(beta_matrix) <- exogenous_variables

  beta_matrix
}
