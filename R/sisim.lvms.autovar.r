sisim.lvms.autovar <- function(newdata, data, harddata, sbt.type, model)
{
    top <- 35;    bottom <- -30;    nrintervals <- 65
    intervalwidth <- (top-bottom)/nrintervals;    toprow <- seq((bottom+intervalwidth),top,intervalwidth);    bottomrow <- seq(bottom,(top-intervalwidth),intervalwidth)
    probmatrix <- NULL;    for(i in 1:nrintervals) probmatrix <- rbind(probmatrix,percentinterval(toprow[i],bottomrow[i],data$geoz,data[,sbt.type]))
    lvms <- matrix(nrow=nrow(newdata), ncol=ncol(probmatrix)); for(i in 1:nrow(newdata)) lvms[i,] <- probmatrix[which(toprow >= newdata$z[i] & bottomrow < newdata$z[i]),]
    results <- sisim(newdata, harddata, model.variograms(sbt.type=sbt.type, data=data, model=model), lvms/100)
    return(results)
}
