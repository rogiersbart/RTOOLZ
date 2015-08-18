#' Mean Error
#' 
#' See \code{hydroGOF::\link[hydroGOF]{me}} for more information.
#' 
#' @importFrom hydroGOF me me.default me.data.frame me.matrix me.zoo
#' @export
me <- function(sim, obs, ...){
  hydroGOF::me(sim, obs, ...)
}