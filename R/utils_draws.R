#' Convert Draw Lists to Matrices
#'
#' Convert a list of MCMC draw objects into a matrix where rows correspond to
#' coefficients and columns correspond to draws. If the draws are 3D arrays,
#' the first slice is used.
#'
#' @param draws A list of draws, where each element is a numeric vector or
#'   matrix representing coefficient values for a single draw.
#'
#' @return A numeric matrix of draws, or `NULL` if `draws` is empty.
#'
#' @keywords internal
draws_to_matrix <- function(draws) {
  if (length(draws) == 0L) {
    return(NULL)
  }
  if (length(draws[[1]]) == 1) {
    mat <- t(as.matrix(simplify2array(draws)))
  } else {
    mat <- simplify2array(draws)
  }
  if (length(dim(mat)) == 3) {
    mat <- mat[, 1, ]
  }
  mat
}

#' Prepare Draw Matrix and Indices
#'
#' Internal helper that applies thinning, converts draw lists to a matrix, and
#' applies an optional matrix transformation.
#'
#' @param draws A list of MCMC draws.
#' @param thin Positive integer thinning interval.
#' @param mat_transform Optional function to transform the draw matrix.
#'
#' @return A list with elements `mat` (numeric matrix) and `draw_idx`
#'   (integer vector), or `NULL` when no draws are available.
#'
#' @keywords internal
prepare_draw_matrix <- function(draws, thin = 1L, mat_transform = NULL) {
  if (is.null(draws) || length(draws) == 0L) {
    return(NULL)
  }

  n_draws <- length(draws)
  draw_idx <- seq_len(n_draws)
  if (thin > 1L) {
    draw_idx <- draw_idx[seq(1, n_draws, by = thin)]
  }

  draws <- draws[draw_idx]
  mat <- draws_to_matrix(draws)
  if (is.null(mat) || length(mat) == 0L) {
    return(NULL)
  }
  if (!is.null(mat_transform)) {
    mat <- mat_transform(mat)
    if (is.null(mat) || length(mat) == 0L) {
      return(NULL)
    }
  }
  if (is.null(dim(mat))) {
    mat <- matrix(mat, nrow = 1)
  }

  list(mat = mat, draw_idx = draw_idx)
}

#' Normalize Coefficient Names
#'
#' Internal helper that aligns coefficient names to the number of matrix rows.
#'
#' @param coef_names Character vector of coefficient names.
#' @param n_coef Integer number of coefficients (rows).
#'
#' @return A character vector of length `n_coef`.
#'
#' @keywords internal
normalize_coef_names <- function(coef_names, n_coef) {
  if (!length(coef_names)) {
    coef_names <- paste0("coef_", seq_len(n_coef))
  }
  if (length(coef_names) != n_coef) {
    coef_names <- coef_names[seq_len(min(length(coef_names), n_coef))]
    if (length(coef_names) < n_coef) {
      coef_names <- c(
        coef_names,
        paste0("coef_", seq_len(n_coef - length(coef_names)))
      )
    }
  }

  coef_names
}

#' Add Parameter Draws to a Long Data Frame
#'
#' Internal helper that checks whether a parameter group is selected and, if so,
#' delegates draw extraction to a caller-supplied builder function.
#'
#' @param param_name Character scalar naming the parameter group (e.g., "beta").
#' @param draws A list of MCMC draws for the parameter group.
#' @param coef_names Character vector of coefficient names.
#' @param params Character vector of selected parameter groups.
#' @param variable Character scalar naming the endogenous variable.
#' @param build_draw_df Function that converts draws into a long data.frame.
#' @param mat_transform Optional function to transform the draw matrix before
#'   conversion.
#'
#' @return A data.frame or `NULL` when the parameter group is not selected or no
#'   draws are available.
#'
#' @keywords internal
add_draws <- function(param_name, draws, coef_names, params, variable,
                      build_draw_df, mat_transform = NULL) {
  if (!param_name %in% params) {
    return(NULL)
  }

  build_draw_df(
    draws = draws,
    variable = variable,
    param_name = param_name,
    coef_names = coef_names,
    mat_transform = mat_transform
  )
}
