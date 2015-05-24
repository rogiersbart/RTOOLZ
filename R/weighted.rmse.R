#' @export
weighted.rmse <- function(x, y, w, ...)
{
  return(sqrt(weighted.mean((x-y)^2),w,...))
}