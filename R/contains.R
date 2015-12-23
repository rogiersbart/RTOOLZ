#' Check if file contains the provided pattern
#' 
#' @param file filename or vector of filenames
#' @param find text string to replace
#' @param replace text string to use as replacement
#' @export
contains <- function(file, pattern)
{
  contains_pattern <- logical(length = length(file))
  for(i in 1:length(file)) {
    contains_pattern[i] <- any(grepl(pattern, readLines(file[i], warn = FALSE)))
  }
  return(file[which(contains_pattern)])
}