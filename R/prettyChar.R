#' Format a character object with specified width, analogues to the \code{base} package \code{prettyNum} function.
#' 
#' @param x Character object.
#' @param width Width of the output string.
#' @return Formatted character object.
#' @export
prettyChar <- function(x,width)
{
  nChars <- nchar(x)
  for(i in 1:length(x))
  {
    x[i] <- paste0(c(rep(' ',width-nChars[i]),x[i]),collapse='')
  }
  return(x)
}
