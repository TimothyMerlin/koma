#' Calculate Summary Statistics for Estimates
#'
#' Computes summary statistics for a specified endogenous variable
#' within a system of equations. It returns the coefficients and standard
#' errors of the equation's variables.
#'
#' @param endogenous_variables A character vector of name(s) of the stochastic
#' endogenous variable to summarize.
#' @param estimates A list of estimates for each stochastic endogenous variable.
#' @param sys_eq A list containing endogenous and exogenous
#' variables, character_gamma_matrix, and character_beta_matrix.
#' @param central_tendency A character string indicating which central tendency
#' measure ("mean" or "median") to use for summary statistics. Default is
#' "mean".
#' @param ci_low The lower bound for the confidence interval as a percentile.
#'   Default is 5.
#' @param ci_up The upper bound for the confidence interval as a percentile.
#'   Default is 95.
#'
#' @return A list of summary statistics for each endogenous variable.
#' @keywords internal
summary_statistics <- function(endogenous_variables, estimates, sys_eq,
                               central_tendency = "mean",
                               ci_low = 5,
                               ci_up = 95) {
  out <- list()
  # percentiles that should be extracted from estimates include:
  # ci_low, ci_up and globally defined quantiles that contain median
  probs <- unique(c(ci_low / 100, ci_up / 100, get_quantiles()))
  ci_low <- paste0("q_", ci_low)
  ci_up <- paste0("q_", ci_up)

  for (endogenous_variable in endogenous_variables) {
    jx <- grep(
      paste0("\\b", endogenous_variable, "\\b"),
      sys_eq$endogenous_variables
    )
    if (length(jx) == 0) {
      cli::cli_abort(c(
        "Variable not found in endogenous variables: ",
        ">" = endogenous_variable
      ))
    }

    estimate_jx <- estimates[[endogenous_variable]]
    if (is.null(estimate_jx)) {
      cli::cli_abort(
        "The provided endogenous variable {.strong {endogenous_variable}}
        was not found in the estimates."
      )
    }

    beta_data <- quantiles_from_estimates(
      estimate_jx$beta_jw,
      include_mean = TRUE, probs = probs
    )
    gamma_data <- quantiles_from_estimates(
      estimate_jx$gamma_jw,
      include_mean = TRUE, probs = probs
    )

    idx_beta <- which(sys_eq$character_beta_matrix[, jx] != 0)
    idx_gamma <- which(
      sys_eq$character_gamma_matrix[, jx] != 0 &
        sys_eq$character_gamma_matrix[, jx] != 1
    )

    beta_coef <- get_central_tendency(central_tendency, beta_data)
    gamma_coef <- get_central_tendency(central_tendency, gamma_data)

    beta_ci_low <- beta_data[[ci_low]]
    beta_ci_up <- beta_data[[ci_up]]
    gamma_ci_low <- gamma_data[[ci_low]]
    gamma_ci_up <- gamma_data[[ci_up]]

    coef_names <- c(
      sys_eq$total_exogenous_variables[idx_beta],
      if (any(!is.na(gamma_coef))) sys_eq$endogenous_variables[idx_gamma] else NULL
    )

    coef_values <- c(
      beta_coef,
      if (any(!is.na(gamma_coef))) gamma_coef else NULL
    )

    names(coef_values) <- coef_names

    ci_low_values <- c(
      beta_ci_low,
      if (any(!is.na(gamma_coef))) gamma_ci_low else NULL
    )

    ci_up_values <- c(
      beta_ci_up,
      if (any(!is.na(gamma_coef))) gamma_ci_up else NULL
    )

    tr <- list(
      coef.names = coef_names,
      coef = coef_values,
      ci.low = ci_low_values,
      ci.up = ci_up_values,
      pvalues = numeric(),
      model.name = endogenous_variable
    )

    out[[endogenous_variable]] <- tr
  }

  out
}
