#' Complementary error function
#' 
#' @export
erfc <- function(x) {
  return(2 * pnorm(x * sqrt(2), lower = FALSE))
} 
