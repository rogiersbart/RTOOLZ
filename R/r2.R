#' Coefficient of Determination
#' 
#' @param x a numeric vector, matrix or data frame.
#' @param y a numeric vector, matrix or data frame with compatible dimensions to \code{x}.
#' @param ... arguments passed to \code{\link{cor}}.
#' @export
r2 <- function(x, y){
  return((cor(x, y, ...)) ^ 2)
}