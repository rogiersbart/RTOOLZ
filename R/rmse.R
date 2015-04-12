#' @export
rmse <- function(x, y,...)
{
  return(sqrt(mean((x-y)^2),...))
}