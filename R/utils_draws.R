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
