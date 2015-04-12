#' Conditional return
#' 
#' \code{ifelse0} returns \code{yes} if \code{test} is \code{TRUE}. If \code{test} is \code{FALSE}, it returns \code{no}.
#' @param test an object which can be coerced to logical mode.
#' @param yes return value for \code{test==TRUE}
#' @param no return value for \code{test==FALSE}
#' @export
ifelse0 <- function(test, yes, no)
{
  if(test)
  {
    return(yes)
  } else {
    return(no)
  }
}