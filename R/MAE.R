#' Mean Absolute Error
#' 
#' See \code{hydroGOF::\link[hydroGOF]{mae}} for more information.
#' 
#' @importFrom hydroGOF mae mae.default mae.data.frame mae.matrix mae.zoo
#' @export
mae <- function(sim, obs, ...){
  hydroGOF::mae(sim, obs, ...)
}