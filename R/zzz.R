#' Default coordinate reference system
#' 
.onLoad <- function(libname, pkgname) {
  opts_global <- options()
  opts_RTOOLZ <- list(
    default_crs = belgian_lambert_1972()
  )
  toset <- !(names(opts_RTOOLZ) %in% names(opts_global))
  if(any(toset)) options(opts_RTOOLZ[toset])
  invisible()
}
