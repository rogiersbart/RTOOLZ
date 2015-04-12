#' Mean Absolute Error
#' 
#' @export
MAE <- function(observations, predictions)
{return(mean(abs(observations-predictions)))}