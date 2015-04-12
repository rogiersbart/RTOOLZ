#' Mean Squared Error
#' 
#' @export
MSE <- function(observations, predictions)
{return(mean((observations-predictions)^2))}