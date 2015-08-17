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
  nrs <- which(!is.na(dat$x+dat$y))
  if (length(nrs) > 1) {
    longlat <- spTransform(SpatialPoints((cbind(dat$x, dat$y)[nrs, ]), proj4string = crs), RTOOLZ::long_lat)
  } else {
    longlat <- spTransform(SpatialPoints(data.frame(cbind(dat$x, dat$y))[nrs, ], proj4string = crs), RTOOLZ::long_lat)
  } 
  dat <- data.frame(dat, long = rep(NA, nrow(dat)), lat = rep(NA, nrow(dat)))
  if(length(nrs) == 1) {
    dat$long[nrs] <- longlat$X1
    dat$lat[nrs] <- longlat$X2
  } else {
    dat$long[nrs] <- longlat$coords.x1
    dat$lat[nrs] <- longlat$coords.x2
  }
  return(dat)
}