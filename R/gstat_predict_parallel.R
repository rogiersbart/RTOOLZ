#' @import parallel
#' @export
gstat_predict_parallel <- function(g,newdata,cores=detectCores(), ...)
{
  newdata_list <- list()
  nrs <- seq(0,nrow(newdata),length=1+cores)
  for(i in 1:cores) newdata_list[[i]] <- newdata[(nrs[i]+1):(nrs[i+1]),]
  gstat_predict_parallel_fun <- function(newdata,g)
  {
    as.data.frame(predict(g,newdata=newdata, ...))
  }
  
  cl <- makeCluster(cores)
  clusterCall(cl,function(){library(gstat)})
  #clusterExport(cl, predict.gstat)
  
  preds <- parLapply(cl,newdata_list,gstat_predict_parallel_fun,g=g)
  
  stopCluster(cl)
  
  # transform results in dataframe
  pred <- do.call('rbind',preds)
}