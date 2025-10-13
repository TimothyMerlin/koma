#' Random Wishart Draw
#'
#' Generates a random draw from the Wishart distribution.
#' Adapted from \code{MCMCpack::rwish} (Martin et al., 2011) under GPL-3.
#'
#' @param v Degrees of freedom (scalar). Must be >= dimension of S.
#' @param S Positive-definite scale matrix (p x p).
#'
#' @return A single random draw (matrix) from the Wishart distribution.
#'
#' @references Martin, R., Quinn, K., & Park, J. (2011). MCMCpack: Markov
#' Chain Monte Carlo in R. \emph{Journal of Statistical Software, 42}(9), 1-21.
rwish <- function(v, S) {
  if (!is.matrix(S)) S <- matrix(S)
  if (nrow(S) != ncol(S)) stop("S must be square in rwish()")
  if (v < nrow(S)) stop("v must be >= dimension of S in rwish()")

  p <- nrow(S)
  CC <- chol(S)
  Z <- matrix(0, p, p)
  diag(Z) <- sqrt(stats::rchisq(p, v:(v - p + 1)))

  if (p > 1) {
    pseq <- 1:(p - 1)
    Z[rep(p * pseq, pseq) + unlist(lapply(pseq, seq))] <- rnorm(p * (p - 1) / 2)
  }

  crossprod(Z %*% CC)
}

#' Random Inverse-Wishart Draw
#'
#' Generates a random draw from the Inverse-Wishart distribution.
#'
#' This function is adapted from \code{MCMCpack::riwish}
#' (Martin et al., 2011) under GPL-3. The Inverse-Wishart
#' distribution is commonly used as a prior for covariance matrices.
#'
#' @param v Degrees of freedom (scalar). Must be >= dimension of S.
#' @param S Positive-definite scale matrix (p x p).
#'
#' @return A single random draw (matrix) from the Inverse-Wishart distribution.
#'
#' @references Martin, R., Quinn, K., & Park, J. (2011). MCMCpack: Markov
#' Chain Monte Carlo in R. \emph{Journal of Statistical Software, 42}(9), 1-21.
riwish <- function(v, S) {
  solve(rwish(v, solve(S)))
}
