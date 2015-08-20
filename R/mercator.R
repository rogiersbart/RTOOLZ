#' Mercator coordinate reference system
#' 
#' Used by Google Maps, Open Street Maps, Stamen Maps
#' 
#' @export
mercator <- function() return(sp::CRS("+init=epsg:3857"))
