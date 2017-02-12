#' Get the nth minimum value
#' 
#' @export
nmin <- function(x, n=2){
  len <- length(x)
  if(n > len){
    warning('n greater than length(x).  Setting n=length(x)')
    n <- length(x)
  }
  sort(x,partial=n)[n]
}