#' Bayesian data fusion
#' 
#' @param data vector of observed values
#' @param pred list of predictions (vectors, matrices or arrays)
#' @param var list of variances (vectors, matrices or arrays)
#' @return list with with Bayesian data fusion results: prediction and variance (vectors, matrices or arrays)
#' @export
bayesian_data_fusion <- function(data,pred,var)
{
  sum_of_one_over_var <- 0
  sum_of_pred_over_var <- 0
  for(i in 1:length(pred)){
    sum_of_one_over_var <- sum_of_one_over_var + (1/var[[i]])
    sum_of_pred_over_var <- sum_of_pred_over_var + (pred[[i]]/var[[i]])
  }
  var_bdf <- 1/(sum_of_one_over_var- ((length(pred)-1)/var(data)))
  pred_bdf <- (sum_of_pred_over_var - ((length(pred)-1)*mean(data)/var(data)))*var_bdf
  return(list(pred=pred_bdf,var=var_bdf))
}
