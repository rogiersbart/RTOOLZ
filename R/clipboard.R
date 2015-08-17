#' Clipboard
#' 
#' @export
clipboard <- function(x = NULL, row.names = FALSE, col.names = TRUE, ...) {
  if (is.null(x)) {
    return(read.delim("clipboard", header = col.names, ...))
  } else {
    write.table(x,"clipboard",sep="\t",row.names = row.names, col.names = col.names, ...)
  }
}
