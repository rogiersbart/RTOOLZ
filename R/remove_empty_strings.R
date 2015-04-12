#' Remove empty elements from a vector of strings.
#' 
#' @param stringArray Vector of strings.
#' @return Vector of strings without the empty items.
#' @export
remove_empty_strings <- function(stringArray)
{
  return(stringArray[which(stringArray!='')])
}