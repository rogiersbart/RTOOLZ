#' Get comments at the beginning of a vector of strings
#' 
#' @param lines Vector of strings, typically obtained from using \code{scan} with \code{sep='\n'}.
#' @return Vector of strings with the commented lines.
#' @export
get_comments_from_lines <- function(lines)
{
  i <- 0
  comments <- NULL
  while(i==0) 
  {
    ifelse(substr(lines[1], 1,1)=='#', comments <- append(comments,lines[1]), i<-1)   
    lines <- lines[-1]
  }
  return(comments)
}