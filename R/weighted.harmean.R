#' Calculate a weighted harmonic mean
#' 
#' @param x An invertable R object.
#' @param ... further arguments passed to \code{\link{sum}}
#' @export
#' @seealso \code{\link{weighted.geomean}}, \code{\link{weighted.mean}}, \code{\link{harmean}} \code{\link{geomean}} and \code{\link{mean}}
weighted.harmean <- function(x, w, ...)
{
  return(sum(w)/(sum(w/x, ...)))
}

