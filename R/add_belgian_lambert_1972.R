#' Add Belgian Lambert 1972 coordinates to a data frame
#' 
#' @param dat data frame with x and y coordinates
#' @param crs coordinate reference system of the data (defaults to \code{\link{getOption}('default_crs')})
#' @return data frame with additional x_belg and y_belg columns
#' @export
add_belgian_lambert_1972 <- function(dat, crs = NULL) {
  if (is.null(crs)) {
    warning('No CRS specified. Using default.')
    crs <- getOption('default_crs')
  }
  return(convert_coordinates(dat,from=crs,to=belgian_lambert_1972(),names_from=c('x','y'),names_to=c('x_belg','y_belg')))
}