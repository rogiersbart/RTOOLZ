#' Add mercator coordinates to a data frame
#' 
#' @param dat data frame with x and y coordinates
#' @param crs coordinate reference system of the data (defaults to \code{\link{getOption}('default_crs')})
#' @return data frame with additional x_merc and y_merc columns
#' @importFrom sp spTransform SpatialPoints
#' @export
add_mercator <- function(dat, crs = NULL) {
  if (is.null(crs)) {
    warning('No CRS specified. Using default.')
    crs <- getOption('default_crs')
  }
  return(convert_coordinates(dat,from=crs,to=mercator(),names_from=c('x','y'),names_to=c('x_merc','y_merc')))
}