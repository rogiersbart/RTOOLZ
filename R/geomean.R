#' Calculate a geometric mean
#' 
#' @param x An R object.
#' @param ... further arguments passed to \code{\link{prod}}
#' @export
#' @seealso \code{\link{harmean}} and \code{\link{mean}}
geomean <- function(x)
{
  return(prod(x, ...)^(1/length(x)))
}