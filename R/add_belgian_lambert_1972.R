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
  nrs <- which(!is.na(dat$x+dat$y))
  if (length(nrs) > 1) {
    belg <- sp::spTransform(sp::SpatialPoints((cbind(dat$x, dat$y)[nrs, ]), proj4string = crs), belgian_lambert_1972())
  } else {
    belg <- sp::spTransform(sp::SpatialPoints(data.frame(cbind(dat$x, dat$y))[nrs, ], proj4string = crs), belgian_lambert_1972())
  } 
  dat <- data.frame(dat, x_belg = rep(NA, nrow(dat)), y_belg = rep(NA, nrow(dat)))
  if(length(nrs) == 1) {
    dat$x_belg[nrs] <- belg$X1
    dat$y_belg[nrs] <- belg$X2
  } else {
    dat$x_belg[nrs] <- belg$coords.x1
    dat$y_belg[nrs] <- belg$coords.x2
  }
  return(dat)
}