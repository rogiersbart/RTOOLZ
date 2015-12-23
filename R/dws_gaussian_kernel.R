#' Distance weighted statistics: gaussian kernel
#' 
#' @param bandwidth
#' @param background
#' @param x distances for which to get the gaussian kernel value
#' @return gaussian kernel values for distances x
#' @references Machuca-Mory, D. F., & Deutsch, C. V. (2012). Non-stationary Geostatistical Modeling Based on Distance Weighted Statistics and Distributions. Mathematical Geosciences, 45(1), 31–48. http://doi.org/10.1007/s11004-012-9428-z
#' @export
dws_gaussian_kernel <- function(bandwidth, background, x)
{
  sumExp <- sum(exp(-(x^2)/(2*bandwidth^2)))
  return( (background + exp(-(x^2)/(2*bandwidth^2)))/(length(x)*background + sumExp) )
}