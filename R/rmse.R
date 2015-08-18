#' Root Mean Squared Error
#' 
#' See \code{hydroGOF::\link[hydroGOF]{rmse}} for more information.
#' 
#' @importFrom hydroGOF rmse rmse.default rmse.data.frame rmse.matrix rmse.zoo
#' @export
rmse <- function(sim, obs, ...){
  hydroGOF::rmse(sim, obs, ...)
}