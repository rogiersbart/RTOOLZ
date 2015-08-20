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
  nrs <- which(!is.na(dat$x+dat$y))
  if (length(nrs) > 1) {
    merc <- sp::spTransform(sp::SpatialPoints((cbind(dat$x, dat$y)[nrs, ]), proj4string = crs), mercator())
  } else {
    merc <- sp::spTransform(sp::SpatialPoints(data.frame(cbind(dat$x, dat$y))[nrs, ], proj4string = crs), mercator())
  } 
  dat <- data.frame(dat, x_merc = rep(NA, nrow(dat)), y_merc = rep(NA, nrow(dat)))
  if(length(nrs) == 1) {
    dat$x_merc[nrs] <- merc$X1
    dat$y_merc[nrs] <- merc$X2
  } else {
    dat$x_merc[nrs] <- merc$coords.x1
    dat$y_merc[nrs] <- merc$coords.x2
  }
  return(dat)
}