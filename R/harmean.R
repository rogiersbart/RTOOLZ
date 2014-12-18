#' Calculate a harmonic mean
#' 
#' @param x An invertable R object.
#' @param ... further arguments passed to \code{\link{mean}}
#' @export
#' @seealso \code{\link{geomean}} and \code{\link{mean}}
harmean <- function(x, ...)
{
  return(1/(mean(1/x, ...)))
}
