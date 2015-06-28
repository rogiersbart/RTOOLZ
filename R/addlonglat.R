#' Add long lat coordinates to a data frame
#' 
#' @param dat data frame with x and y coordinates
#' @param CRS_from original CRS; defaults to '+init=epsg:31370'
#' @param CRS_to target CRS; defaults to '+proj=longlat +datum=WGS84'
#' @return data frame with additional long and lat columns
#' @export
addlonglat <- function(dat,CRS_from=CRS("+init=epsg:31370"),CRS_to=CRS("+proj=longlat +datum=WGS84"))
{
  require(rgdal)
  nrs <- which(!is.na(dat$x+dat$y))
  longlat <- spTransform(SpatialPoints((cbind(dat$x,dat$y)[nrs,]),proj4string=CRS_from),CRS_to)
  dat <- data.frame(dat,long=rep(NA,nrow(dat)),lat=rep(NA,nrow(dat)))
  dat$long[nrs] <- longlat$coords.x1
  dat$lat[nrs] <- longlat$coords.x2
  return(dat)
}