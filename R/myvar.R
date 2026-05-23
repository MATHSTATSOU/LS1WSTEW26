#' Calculation of sample variance
#'
#' @description
#' This is a very simple introduction to function making and packaging
#'
#'
#' @param y a numeric vector
#'
#' @returns a scalar (variance)
#' @export
#'
#' @examples
#' myvar(1:10)
#' \dontrun{myvar(1:10)}
myvar <- function(y){
  n <- length(y)
  ssq <- sum((y - mean(y))^2)/(n - 1) # n-1 for unbiased estimator
  ssq
}
