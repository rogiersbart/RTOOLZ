#' Nash-Sutcliffe Efficiency
#' 
#' See \code{hydroGOF::\link[hydroGOF]{NSE}} for more information.
#' 
#' @importFrom hydroGOF NSE NSE.default NSE.data.frame NSE.matrix NSE.zoo
#' @export
nse <- function(sim, obs, ...){
  hydroGOF::NSE(sim, obs, ...)
}