#' Add osm mercator coordinates to a data frame
#'
#' @param dat data frame with x and y coordinates
#' @param CRS_from original CRS; defaults to '+init=epsg:31370'
#' @param CRS_to target CRS; defaults to osm mercator
#' @return data frame with additional Xosmmerc and Yosmmerc columns
#' @export
addosmmerc <- function(dat,CRS_from=CRS("+init=epsg:31370"),CRS_to=CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
{
  require(rgdal)  
  nrs <- which(!is.na(dat$x+dat$y))
  if(length(nrs)>1) 
  {
    longlat <- spTransform(SpatialPoints((cbind(dat$x,dat$y)[nrs,]),proj4string=CRS_from),CRS_to)
  } else {
    longlat <- spTransform(SpatialPoints(data.frame(cbind(dat$x,dat$y))[nrs,],proj4string=CRS_from),CRS_to)
  } 
  dat <- data.frame(dat,long=rep(NA,nrow(dat)),lat=rep(NA,nrow(dat)))
  if(length(nrs)==1)
  {
    dat$Xosmmerc[nrs] <- longlat$X1
    dat$Yosmmerc[nrs] <- longlat$X2
  } else {
    dat$Xosmmerc[nrs] <- longlat$coords.x1
    dat$Yosmmerc[nrs] <- longlat$coords.x2
  }
  return(dat)
}