#' Distance weighted statistics: pair weights
#' 
#' @param exp_var_cloud experimental variogram cloud
#' @param point_weights point weights
#' @param t
#' @return pair weights
#' @references Machuca-Mory, D. F., & Deutsch, C. V. (2012). Non-stationary Geostatistical Modeling Based on Distance Weighted Statistics and Distributions. Mathematical Geosciences, 45(1), 31–48. http://doi.org/10.1007/s11004-012-9428-z
#' @export
dws_pair_weights <- function(exp_var_cloud, point_weights, t=1)
{
  return( ((point_weights[data.frame(exp_var_cloud)$left]^t + point_weights[data.frame(exp_var_cloud)$right]^t)/2)^(1/t) )
}