#' Change default coordinate reference system
#' 
#' @export
set_default_crs <- function(crs){
  options(default_crs = crs)
}