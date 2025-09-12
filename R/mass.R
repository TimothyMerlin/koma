#' Simulate from a Multivariate Normal Distribution
#'
#' Produces one or more samples from the specified multivariate normal
#' distribution.
#'
#' @param n The number of samples required.
#' @param mu A vector giving the means of the variables.
#' @param sigma A positive-definite symmetric matrix specifying the covariance
#' matrix of the variables.
#' @param tol Tolerance (relative to the largest variance) for numerical lack
#' of positive-definiteness in \code{sigma}.
#' @param empirical Logical. If true, mu and sigma specify the empirical not
#' population mean and covariance matrix.
#'
#' @return If `n = 1` a vector of the same length as `mu`, otherwise
#' an `n` by `length(mu)` matrix with one sample in each row.
#'
#' @details The matrix decomposition is done vai `eigen`; although a Choleski
#' decomposition might be faster, the eigendecomposition is stabler.
#'
#' @references
#' Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics
#' with S.  Fourth Edition. Springer, New York. ISBN 0-387-95457-0
#'
#' @examples
#' #'
#' \dontrun{
#' sigma <- matrix(c(10, 3, 3, 2), 2, 2)
#' sigma
#' var(multivariate_norm(n = 1000, rep(0, 2), sigma))
#' var(multivariate_norm(n = 1000, rep(0, 2), sigma, empirical = TRUE))
#' }
#' @seealso [rnorm()]
#' @importFrom stats rnorm
#' @keywords internal
multivariate_norm <- function(n = 1, mu, sigma, tol = 1e-06,
                              empirical = FALSE) {
  # nolint start
  p <- length(mu)
  if (!all(dim(sigma) == c(p, p))) {
    stop("incompatible arguments")
  }
  eS <- eigen(sigma, symmetric = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L]))) {
    stop("'Sigma' is not positive definite")
  }
  X <- matrix(rnorm(p * n), n)
  if (empirical) {
    X <- scale(X, TRUE, FALSE)
    X <- X %*% svd(X, nu = 0)$v
    X <- scale(X, FALSE, TRUE)
  }
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*%
    t(X)
  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(sigma))) {
    nm <- dn[[1L]]
  }
  dimnames(X) <- list(nm, NULL)
  if (n == 1) {
    drop(X)
  } else {
    t(X)
  }
  # nolint end
}


#' Evaluate Multivariate Normal Density at x

#' @param x A vector at which to evaluate the density.
#' @param mu A vector giving the means of the variables.
#' @param sigma A positive-definite symmetric matrix specifying the covariance
#' matrix of the variables.
#'
#' @return The density of the multivariate normal distribution at \code{x}.
#'
#' @importFrom stats dnorm
#' @keywords internal
multivariate_norm_pdf <- function(x, mu, sigma) {
  p <- length(mu)
  if (!all(dim(sigma) == c(p, p))) {
    stop("incompatible arguments")
  }

  det_sigma <- det(sigma)
  inv_sigma <- solve(sigma)

  diff <- as.vector(x - mu)
  exponent <- -0.5 * t(diff) %*% inv_sigma %*% diff
  density <- (1 / sqrt((2 * pi)^p * det_sigma)) * exp(exponent)

  return(as.numeric(density))
}
