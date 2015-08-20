#' Longitude latitude coordinate reference system
#' 
#' Commonly used by organizations that provide GIS data for the entire globe or many countries. CRS used by Google Earth.
#' 
#' @export
long_lat <- function() return(sp::CRS("+init=epsg:4326"))
