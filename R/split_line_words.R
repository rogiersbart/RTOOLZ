#' Split a string in words.
#' 
#' @param string A string.
#' @return Vector of words within the string, without comments at the end of the string.
#' @export
split_line_words <- function(string)
{
  split.line <- as.character(strsplit(remove_comments_end_of_line(string),' |\t')[[1]])
  split.line <- remove_empty_strings(split.line)
  return(split.line)
}