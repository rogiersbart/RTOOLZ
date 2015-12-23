#' Model a 2d variogram with gstat
#' 
#' @param dat data frame with coordinates x, y and attribute z
#' @param model variogram model type, e.g. "Exp", "Sph", "Gau", "Mat".
#' @return an isotropic gstat variogram model
#' @export
gstat_model_2d_variogram <- function(dat, model='Exp')
{
  vgm.hor <- NULL; expvar.hor <- NULL
  gstatobject <- gstat(id='z',formula= z ~ 1, locations= ~ x+y, data=dat)
  expvar.hor <- variogram(gstatobject, width=100, beta=0, cutoff=10000)
  varmodhor <- vgm(psill=var(dat$z), model=model, range=500, nugget=var(dat$z)/10)
  vgm.hor <- fit.variogram(expvar.hor, varmodhor, fit.sills=T, fit.ranges=T)
  plot(expvar.hor$dist, expvar.hor$gamma, xlab='Horizontal lag distance (m)', ylab='Semivariance')
  lines(variogramLine(vgm.hor, maxdist=10000), col='red', lwd=3)
  print(vgm.hor)
  return(vgm.hor)
}