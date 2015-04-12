#' Remove comments at the beginning of a vector of strings
#' 
#' @param lines Vector of strings, typically obtained from using \code{scan} with \code{sep='\n'}.
#' @return Vector of strings without the commented lines.
#' @export
remove_comments_from_lines <- function(lines)
{
  i <- 0
  while(i==0) ifelse(substr(lines[1], 1,1)=='#', lines <- lines[-1], i<-1)   
  #if(i==1) cat('Comments removed\n')
  return(lines)
}