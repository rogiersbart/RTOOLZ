#' Convert a shapefile with point data to a table with the attributes plus the coordinates
#' 
#' @param file filename of the shapefile, with extension!
#' @param crs target crs for the x and y coordinates in the dataframe
#' @return data frame with all shapefile attributes, and additional x and y columns
#' @export
convert_shapefile_to_dataframe <- function(file, crs = options()$default_crs) {
  shp <- rgdal::readOGR(dirname(file),layer=substr(basename(file),1,nchar(basename(file))-4))
  shp <- data.frame(rgdal::spTransform(shp,crs))
  shp <- shp[,c(1:(ncol(shp)-1))]
  names(shp)[c(ncol(shp)-1,ncol(shp))] <- c('x','y')
  return(shp)
}
