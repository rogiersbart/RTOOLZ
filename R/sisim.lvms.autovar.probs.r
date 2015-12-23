sisim.lvms.autovar.probs <- function(newdata, harddata, model)
{
    top <- 35;    bottom <- -30;    nrintervals <- 65
    intervalwidth <- (top-bottom)/nrintervals;    toprow <- seq((bottom+intervalwidth),top,intervalwidth);    bottomrow <- seq(bottom,(top-intervalwidth),intervalwidth)
    probmatrix <- NULL;    for(i in 1:nrintervals) probmatrix <- rbind(probmatrix,percentinterval.probs(toprow[i],bottomrow[i],harddata$z,harddata[,4:ncol(harddata)]))
    lvms <- matrix(nrow=nrow(newdata), ncol=ncol(probmatrix)); for(i in 1:nrow(newdata)) lvms[i,] <- probmatrix[which(toprow >= newdata$z[i] & bottomrow < newdata$z[i]),]
    results <- sisim(newdata, harddata, model.variograms.probs(harddata, model=model), lvms/100)
    return(results)
} # probmatrix!!!!
