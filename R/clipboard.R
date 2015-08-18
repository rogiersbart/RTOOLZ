#' Copy/paste data from/to clipboard
#' 
#' Compatible with vectors, data frames (with header) and numeric matrices
#' 
#' @param x object to copy to clipboard; if not provided, the current clipboard contents will be returned
#' @seealso \code{\link{utils::readClipboard}}, \code{\link{utils::writeClipboard}}
#' @export
#' @rdname clipboard
clipboard <- function(x = NULL, ...){
  UseMethod('clipboard')
}

#' @method clipboard default
#' @export
#' @rdname clipboard
#' @examples
#' clipboard(1:10)
#' clipboard()
#' clipboard(letters)
#' clipboard()
#' clipboard(rnorm(10)>0.5)
#' which(clipboard())
clipboard.default <- function(x, ...) {
  write(x,"clipboard",sep="\t", ncolumns = length(x), ...)
}

#' @method clipboard NULL
#' @export
#' @rdname clipboard
clipboard.NULL <- function(x, ...) {
  clip <- read.delim("clipboard",header=FALSE)
  if(dim(clip)[1] == 1) {
    clip <- t(clip)
    rownames(clip) <- NULL
    return(clip[,1])
  } else {
    if(is.numeric(clip[,1])) {
      return(clip)
    } else {
      return(read.delim("clipboard"))
    }
  }
}

#' @method clipboard data.frame
#' @export
#' @rdname clipboard
#' @examples
#' clipboard(cars)
#' clipboard()
clipboard.data.frame <- function(x, ...) {
  write.table(x,"clipboard",sep="\t",row.names = FALSE, col.names = TRUE, ...)
}

#' @method clipboard matrix
#' @export
#' @rdname clipboard
#' @examples
#' clipboard(volcano)
#' clipboard()
clipboard.matrix <- function(x, ...) {
  write.table(x,"clipboard",sep="\t",row.names = FALSE, col.names = FALSE, ...)
}