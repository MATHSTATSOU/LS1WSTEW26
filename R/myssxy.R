#' Calculation of Cross Product
#'
#' @param x a numeric vector
#' @param y a numeric vector
#'
#' @returns a scalar (the cross product)
#' @export
#'
#' @examples
#' myssxy(x = 1:10, y = 1:10)
myssxy <- function(x,y){
  cpdt <- sum(( x - mean(x)) * (y - mean(y)))
  cpdt
}
