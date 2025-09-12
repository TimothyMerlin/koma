#' Extract Median Estimates
#'
#' This function computes either central tendency estimates (median or mean) or
#' returns estimates from a specific draw for gamma, beta, and sigma matrices.
#' It is mainly used internally by the `construct_posterior` function.
#'
#' @param sys_eq A list containing detailed components of the
#' system of equations such as `endogenous_variables`,
#' `total_exogenous_variables`, `identity_equations`, `character_beta_matrix`,
#' and `character_gamma_matrix`.
#' @param estimates A list of estimates, obtained from previous estimation
#' procedures.
#' @param jx If an index is set then the posterior is constructed for estimate
#' of draw jx, otherwise the posterior is constructed using the median of the
#' estimates.
#' The default is that the posterior is constructed using the median estimates.
#' @param central_tendency A character string indicating which central tendency
#' measure ("mean" or "median") to use.
#' Default is "mean". Active only when jx is NULL.
#'
#' @return A list containing:
#'   - `gamma_matrix`: Either central tendency estimates or specific draw
#'     estimates for gamma values, depending on `jx`.
#'   - `beta_matrix`: Either central tendency estimates or specific draw
#'     estimates for beta values, depending on `jx`.
#'   - `sigma_matrix`: Either central tendency estimates or specific draw
#'     estimates for sigma values, depending on `jx`.
#' @keywords internal
extract_estimates_from_draws <- function(sys_eq, estimates,
                                         jx = NULL,
                                         central_tendency = "mean") {
  # Helper function to extract estimates based on central tendency
  # or specific draw
  extract_estimate <- function(estimates_var, jx, central_tendency, var_name) {
    if (is.null(jx)) {
      # Case: point forecast
      estimate <- quantiles_from_estimates(
        estimates_var[[var_name]],
        include_mean = TRUE
      )
      return(get_central_tendency(central_tendency, estimate))
    } else {
      # Case: specific draw
      return(estimates_var[[var_name]][[jx]])
    }
  }

  endogenous_variables <- sys_eq$endogenous_variables
  total_exogenous_variables <- sys_eq$total_exogenous_variables
  identities <- sys_eq$identities
  character_gamma_matrix <- sys_eq$character_gamma_matrix
  number_of_endogenous <- length(endogenous_variables)
  number_of_total_exogenous <- length(total_exogenous_variables)

  # Initialize gamma, beta and sigma matrices
  gamma_matrix <- diag(number_of_endogenous)
  colnames(gamma_matrix) <- endogenous_variables
  rownames(gamma_matrix) <- endogenous_variables
  beta_matrix <- matrix(0, number_of_total_exogenous, number_of_endogenous,
    dimnames = list(total_exogenous_variables, endogenous_variables)
  )
  sigma_matrix <- matrix(0, 1, number_of_endogenous)

  ix <- which(endogenous_variables %in% names(identities))
  if (length(ix) > 0) {
    endogenous_variables <- endogenous_variables[-ix]
  }

  for (kx in seq_along(endogenous_variables)) {
    estimates_var <- estimates[[kx]]

    beta_estimate <- extract_estimate(
      estimates_var, jx, central_tendency, "beta_jw"
    )
    gamma_estimate <- extract_estimate(
      estimates_var, jx, central_tendency, "gamma_jw"
    )
    omega_estimate <- extract_estimate(
      estimates_var, jx, central_tendency, "omega_tilde_jw"
    )
    # Populate matrices
    # beta_matrix[, kx] <- beta_estimate
    beta_matrix[which(sys_eq$character_beta_matrix[, kx] != 0), kx] <- beta_estimate
    gamma_matrix[grep("gamma", character_gamma_matrix[, kx]), kx] <-
      -gamma_estimate
    sigma_matrix[kx] <- omega_estimate[1]
  }

  return(list(
    gamma_matrix = gamma_matrix,
    beta_matrix = beta_matrix,
    sigma_matrix = sigma_matrix
  ))
}
