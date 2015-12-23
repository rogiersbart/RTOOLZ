#' Distance weighted statistics: weighted experimental variogram
#' 
#' @param exp_var_cloud
#' @param boundaries
#' @param t
#' @param point_weights
#' @param x
#' @param y
#' @param z
#' @param newX
#' @param newY
#' @param newZ
#' @param verticalExaggeration
#' @param bandwidth
#' @param background
#' @return weighted experimental variogram
#' @references Machuca-Mory, D. F., & Deutsch, C. V. (2012). Non-stationary Geostatistical Modeling Based on Distance Weighted Statistics and Distributions. Mathematical Geosciences, 45(1), 31–48. http://doi.org/10.1007/s11004-012-9428-z
#' @export
dws_weighted_exp_var <- function(exp_var_cloud, bandwidth, background, x, y, z,
                           newX, newY, newZ, verticalExaggeration, boundaries, t, point_weights=NULL)
{
  if(is.null(point_weights)) point_weights <- dws_point_weights(x,y,z,newX,newY,newZ,verticalExaggeration,bandwidth,background)
  pairWeights <- dws_pair_weights(exp_var_cloud, point_weights, t)
  directions <- paste(exp_var_cloud$dir.hor,exp_var_cloud$dir.ver)
  ndirections <- nlevels(as.factor(directions))
  expvar <- NULL
  expvar_dir <- NULL
  exp_var_cloud$dir.hor <- as.numeric(as.character(exp_var_cloud$dir.hor))
  exp_var_cloud$dir.ver <- as.numeric(as.character(exp_var_cloud$dir.ver))
  for(direction in 1:ndirections)
  {
    temp <- data.frame(apply(data.frame(levels(factor(directions))),1,FUN=strsplit,split=' '))
    dir.hor <- as.numeric(as.character(temp[1,direction]))
    dir.ver <- as.numeric(as.character(temp[2,direction]))
    for(i in 1:(length(boundaries)-1))
    {
      nrs <- which(exp_var_cloud$dist >= boundaries[i] & exp_var_cloud$dist < boundaries[i+1] & exp_var_cloud$dir.hor==dir.hor & exp_var_cloud$dir.ver==dir.ver)
      expvar_dir$dist[i] <- weighted.mean(exp_var_cloud$dist[nrs],pairWeights[nrs])
      expvar_dir$gamma[i] <- weighted.mean(exp_var_cloud$gamma[nrs],pairWeights[nrs])
      expvar_dir$np[i] <- sum(pairWeights[nrs])
    }    
    if(direction==1) expvar <- data.frame(expvar_dir,dir.hor=dir.hor,dir.ver=dir.ver)
    if(direction > 1) expvar <- rbind(expvar,data.frame(expvar_dir,dir.hor=dir.hor,dir.ver=dir.ver))
  }
  expvar <- as.data.frame(expvar)
  expvar$dir.hor <- as.factor(expvar$dir.hor)
  expvar$dir.ver <- as.factor(expvar$dir.ver)
  class(expvar) <- c('gstatVariogram','data.frame')
  return(na.omit(expvar))
}