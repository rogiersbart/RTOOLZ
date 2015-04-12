digitize.graph <- function(name,x1,x2,y1,y2,sets=1,setlabels=1:sets,log='',xlab='x axis',ylab='y axis')
{
  require(digitize)
  dataset <- data.frame(x=NULL,y=NULL,lab=NULL)
  cat('Mark axes min and max values \n')
  axes.points <- ReadAndCal(name)
  if(log=='x'){x1 <- log10(x1);x2 <- log10(x2)}
  if(log=='y'){y1 <- log10(y1);y2 <- log10(y2)}
  if(log=='xy'){x1 <- log10(x1);x2 <- log10(x2);y1 <- log10(y1);y2 <- log10(y2)}
  for(i in 1:sets)
  {
    cat(paste('Mark point set "',setlabels[i],'"\n',sep=''))
    data.points <- DigitData(col = 'red')
    dat <- Calibrate(data.points, axes.points, x1, x2, y1, y2)      
    dat$lab <- rep(setlabels[i],nrow(dat))
    dataset <- rbind(dat, dataset)
  }
  if(log=='x'){dataset$x <- 10^(dataset$x)}
  if(log=='y'){dataset$y <- 10^(dataset$y)}
  if(log=='xy'){dataset$x <- 10^(dataset$x);dataset$y <- 10^(dataset$y)}
  print(dataset)
  plot(dataset$x,dataset$y,log=log,pch=as.numeric(as.factor(dataset$lab)),col=as.numeric(as.factor(dataset$lab)),xlab=xlab,ylab=ylab)
  legend('bottomright',setlabels, pch=1:sets,col=1:sets, bty='n')
  return(dataset)    
}