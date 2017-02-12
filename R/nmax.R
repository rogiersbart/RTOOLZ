#' Get the nth maximum value
#' 
#' @export
nmax <- function(x, n=2){
  len <- length(x)
  if(n > len){
    warning('n greater than length(x).  Setting n=length(x)')
    n <- length(x)
  }
  sort(x,partial=len-n+1)[len-n+1]
}