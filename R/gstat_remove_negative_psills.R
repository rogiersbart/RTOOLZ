#' Remove negative partial sills from a gstat variogram model
#' 
#' @param varmod variogram model
#' @return variogram model without negative partial sills
#' @export
gstat_remove_negative_psills <- function(varmod)
{
  warning('Possibly the same as gstat posdef function?')
  varmod$psill[which(varmod$psill < 0)] <- 0
  return(varmod)
}