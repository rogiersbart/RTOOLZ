#' Split the Elements of a Character Vector, and remove empty elements.
#' 
#' @param ... Parameters for \code{strsplit}.
#' @return Output of \code{strsplit}, without empty elements.
#' @export
strsplit0 <- function(...)
{
  output <- strsplit(...)
  for(i in 1:length(output))
  {
    if(output[[i]][1]=='')
    {
      output[[i]] <- output[[i]][-1]
    }
  }
  return(output)
}