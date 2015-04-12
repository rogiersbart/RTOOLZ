################################################################################
# FUNCTION - reduce.dataset ####################################################
################################################################################
reduce.dataset <- function(dataset, times, uniformDistribution=NULL)
{
    if(length(uniformDistribution)==0) { return(dataset[seq(1,nrow(dataset),times),]) }
    else{
    
    nr <- round(nrow(dataset)/times,0)
    rowNumbers <- c(1:nrow(dataset))
    densities <- density(uniformDistribution)
    probabilities <- NULL
    for(i in 1:nrow(dataset)) probabilities[i] <- densities$y[which.min(abs(densities$x-uniformDistribution[i]))]
    useNumbers <- sample(rowNumbers, nr, replace = FALSE, prob = 1/probabilities)
    return(dataset[useNumbers,])
    }
    
}