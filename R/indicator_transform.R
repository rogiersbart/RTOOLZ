#' Indicator transform
#' 
#' @param dat factor
#' @param classes factor levels
#' @return data frame with an indicator column for each factor level
#' @export
indicator_transform <- function(dat, classes=levels(dat))
{
  trans <- matrix(nrow=length(dat),ncol=length(classes))
  for(i in 1:length(classes))
  {
    for(j in 1:length(dat))
    {
    ifelse(dat[j]==classes[i], trans[j,i]<-1, trans[j,i]<-0)
    }
  }
  trans <- as.data.frame(trans)
  names(trans) <- classes
  return(trans)
}