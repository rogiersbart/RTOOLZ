#' Mean Squared Error
#' 
#' See \code{hydroGOF::\link[hydroGOF]{mse}} for more information.
#' 
#' @importFrom hydroGOF mse mse.default mse.data.frame mse.matrix mse.zoo
#' @export
mse <- function(sim, obs, ...){
  hydroGOF::mse(sim, obs, ...)
}