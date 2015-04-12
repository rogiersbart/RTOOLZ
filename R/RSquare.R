#' R Square
#' 
#' @export
RSquare <- function(observations, predictions)
{return((cor(observations, predictions))^2)}