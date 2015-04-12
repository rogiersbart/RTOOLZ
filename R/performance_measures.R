#' Model performance measures
#' 
#' @export
performance_measures <- function(observations, predictions,print=F)
{
  mse <- MSE(observations, predictions)
  mae <- MAE(observations, predictions)
  me <- ME(observations, predictions)
  r2 <- RSquare(observations, predictions)
  nseff <- NSeff(observations, predictions)
  if(print)
  {
    cat(paste('MSE: ', round(mse,2), ' (Mean Squared Error)\n'))
    cat(paste('MAE: ', round(mae,2), ' (Mean Absolute Error)\n'))
    cat(paste('ME: ', round(me,2), ' (Mean Error)\n'))
    cat(paste('R^2: ', round(r2,2), ' (R squared)\n'))
    cat(paste('NSeff: ',round(nseff,2), ' (Nash-Sutcliffe efficiency)\n'))  
  }
  return(data.frame(mse=mse, mae=mae, me=me, r2=r2, nseff=nseff))
}