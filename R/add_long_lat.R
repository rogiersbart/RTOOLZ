#' Add long lat coordinates to a data frame
#' 
#' @param dat data frame with x and y coordinates
#' @param crs coordinate reference system of the data (defaults to \code{\link{getOption}('default_crs')})
#' @return data frame with additional long and lat columns
#' @importFrom sp spTransform SpatialPoints
#' @export
add_long_lat <- function(dat, crs = NULL){
  if(is.null(crs)){
    warning('No CRS specified. Using default.')
    crs <- getOption('default_crs')
  }
  return(convert_coordinates(dat,from=crs,to=long_lat(),names_from=c('x','y'),names_to=c('long','lat')))
}