#' Krige a 2d field with gstat
#' 
#' @param x x coordinates
#' @param y y coordinates
#' @param attribute data values
#' @return a matrix with the predicted values
#' @export
gstat_krige_2d_field <- function(x,y,attribute)
{
  warning('This function has to be updated!')
  dat <- remove.na.rows(data.frame(x=x, y=y, z=attribute))
  dat <- dat[which(dat$z!=Inf & dat$z!=-Inf & dat$z!='NaN'),]
  newdata <- data.frame(x=as.vector(aqmatrix$x),y=as.vector(aqmatrix$y))
  vgmModel <- model.2d.variogram(dat, model='Exp')
  dat$x <- jitter(dat$x, amount=1)
  gstatobj <- gstat(formula = z ~1, locations = ~ x + y, model=vgmModel, data=dat)
  krige <- predict.gstat(gstatobj, newdata=newdata)
  map <- matrix(data=krige$var1.pred , nrow=180, ncol=250)
  return(map)       
}