#' Find and replace text string in files
#' 
#' @param file filename or vector of filenames
#' @param find text string to replace
#' @param replace text string to use as replacement
#' @export
find_and_replace <- function(file,find,replace,fixed=TRUE, ...)
{
  for(f in file)
  {
    x <- readLines(f)
    y <- gsub(find, replace, x, fixed=fixed, ...)
    cat(y, file=f, sep="\n")
  }
}