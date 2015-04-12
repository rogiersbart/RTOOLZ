erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1 
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower=FALSE) 
