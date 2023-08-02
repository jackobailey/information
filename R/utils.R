#' Output a tiny number to avoid log(0)
#'
#' @description
#'
#' Where possible, the `{information}` package relies on microservices architecture. This function serves only one purpose: to output a tiny value. This is necessary to replace zeroes and avoid log(0), which is undefined, in some cases. By having a single function do this, it is easier to keep things consistent throughout the package.

epsilon <- function() 1e-10


#' Additive Smoothing and Normalisation
#'
#' @description
#'
#' Where possible, the `{information}` package relies on microservices architecture. This function serves to increment all values in distributions that include at least on zero and would otherwise lead to log(0) problems with a tiny number, then normalise the resulting distribution.
#'
#' @param x A numeric vector or matrix.

add_smooth_normalise <- function(x){
  x <- x + epsilon()
  x <- x/sum(x)
  x
}
