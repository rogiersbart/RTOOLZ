inv_cummean <- function(v)
{
  icm <- rep(NA,length(v))
  for(i in 1:length(v))
  {
    if(i==1)
    {
      icm[i] <- v[i]
    } else {
      icm[i] <- v[i]*i - sum(icm[1:(i-1)])
    }
    
  }
  return(icm)
}