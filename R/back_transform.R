#' Back transformation based on data attributes
#' 
#' For use with \code{\link{normal_transform}}
#' @param dat vector of values in transformed space, with transform attributes
#' @param newdat vector of values in transformed space, to be backtransformed with dat attributes
#' @export
back_transform <- function(dat,newdat=NULL)
{
  if(is.null(newdat))
  {
    dat <- pnorm(dat)
    dat <- spline(x=c(c(1:length(dat))-0.5),y=attributes(dat)$transform_data[order(attributes(dat)$transform_data)],xout=dat*length(dat),method='natural')$y
    return(dat)
  } else {
    newdat <- pnorm(newdat)
    newdat <- spline(x=c(c(1:length(dat))-0.5),y=attributes(dat)$transform_data[order(attributes(dat)$transform_data)],xout=newdat*length(dat),method='natural')$y
    return(newdat)
  }
}