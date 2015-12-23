#' @export
gstat_fit_anisotropic_variogram <- function(expvar,varmod)
{
  fit_rmse <- function(params,expvar,varmod)
  {
    #print(paste(params))
    varmod$psill[1] <- params[1]
    varmod$psill[2] <- params[2]
    varmod$range[2] <- params[3]
    varmod$ang1[2] <- params[4]
    varmod$anis1[2] <- params[5]
    expvar$fit <- gstat_semivariance(varmod,dist=expvar$dist,dir.hor=expvar$dir.hor)
    return(weighted.rmse(expvar$gamma,expvar$fit,expvar$np/expvar$dist^2))
  }
  opt <- optim(c(varmod$psill[1],varmod$psill[2],varmod$range[2],varmod$ang1[2],varmod$anis1[2]),fit_rmse,expvar=expvar,varmod=varmod,method='L-BFGS-B',lower=c(0.001,0.001,10,0.1,0.01),upper=c(Inf,Inf,Inf,180,1),control=list(parscale=c(varmod$psill[1],varmod$psill[2],varmod$range[2],varmod$ang1[2],varmod$anis1[2])))
  varmod$psill[1] <- opt$par[1]
  varmod$psill[2] <- opt$par[2]
  varmod$range[2] <- opt$par[3]
  varmod$ang1[2] <- opt$par[4]
  varmod$anis1[2] <- opt$par[5]
  attributes(varmod)$rmse <- opt$value
  return(varmod)
}