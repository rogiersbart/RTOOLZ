#' Calculate a weighted geometric mean
#' 
#' @param x An R object.
#' @param ... further arguments passed to \code{\link{prod}}
#' @export
#' @seealso \code{\link{weighted.harmean}}, \code{\link{weighted.mean}}, \code{\link{geomean}}, \code{\link{harmean}} and \code{\link{mean}}
weighted.geomean <- function(x, w, ...)
{
  return(prod(x^w, ...)^(1/sum(w)))
}