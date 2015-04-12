#' Split a string in numbers.
#' 
#' @param string A string.
#' @return Vector of numbers within the string, without comments at the end of the string.
#' @export
split_line_numbers <- function(string)
{
  split.line <- as.vector(na.omit(as.numeric(strsplit(remove_comments_end_of_line(string),' |\t')[[1]])))
  return(split.line)
}