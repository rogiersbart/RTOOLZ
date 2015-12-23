#' Write a GSLIB file
#' 
#' @param dat data frame
#' @param file Filename
#' @param nodatavalue Numeric value for missing data
#' @return None
#' @export
write_gslib <- function(dat, file, nodatavalue=-99999) {
   # This function writes a data frame to a GEO-EAS text file.
   # Write the header
   cat('GSLIB file created in R\n', file=file)
   cat(length(names(dat)), file=file, append=TRUE)
   cat('\n', file=file, append=TRUE)
   write(cbind(names(dat)), file=file, append=TRUE)                          
   for(i in 1:ncol(dat)) dat[is.na(dat[,i]),i] <- nodatavalue
   for(i in 1:ncol(dat)) dat[dat[,i]=='NA',i] <- nodatavalue
   for(i in 1:ncol(dat)) dat[is.infinite(dat[,i]),i] <- nodatavalue
   write.table(dat,file=file, append=TRUE, sep='\t', col.names=FALSE, row.names=FALSE)
}