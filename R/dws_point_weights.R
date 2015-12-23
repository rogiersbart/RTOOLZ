#' Distance weighted statistics: point weights
#' 
#' @param x
#' @param y
#' @param z
#' @param newX
#' @param newY
#' @param newZ
#' @param verticalExaggeration
#' @param bandwidth
#' @param background
#' @return point weights
#' @references Machuca-Mory, D. F., & Deutsch, C. V. (2012). Non-stationary Geostatistical Modeling Based on Distance Weighted Statistics and Distributions. Mathematical Geosciences, 45(1), 31–48. http://doi.org/10.1007/s11004-012-9428-z
#' @export
dws_point_weights <- function(x,y,z,newX,newY,newZ,verticalExaggeration=1, bandwidth, background)
{
  distances <- sqrt(rowSums(data.frame((x-newX)^2,(y-newY)^2,(z*verticalExaggeration-newZ*verticalExaggeration)^2))) # points.distance.3d(x,y,z,newX,newY,newZ, verticalExaggeration)
  return(dws_gaussian_kernel(bandwidth, background, distances))
}