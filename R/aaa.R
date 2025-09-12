#' Define the internal environment of the package
#'
the <- new.env(parent = emptyenv())

## Maximum lag of variables
the$maxlag <- 100

## Quantiles

get_quantiles <- function() {
  c(0.05, 0.5, 0.95)
}
