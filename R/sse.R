#' Sum of Squared Errors
#' 
#' See \code{hydroGOF::\link[hydroGOF]{ssq}} for more information.
#' 
#' @importFrom hydroGOF ssq ssq.default ssq.data.frame ssq.matrix
#' @export
sse <- function(sim, obs, ...){
  hydroGOF::ssq(sim, obs, ...)
}