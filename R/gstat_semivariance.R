#' @export
gstat_semivariance <- function(varmod,dist,dir.hor)
{
  dirs <- as.numeric(as.character(levels(dir.hor)))
  ndirs <- length(dirs)
  dir.hor <- as.numeric(as.character(dir.hor))
  semvar <- rep(NA,length(dist))
  #       for(i in 1:length(dist))
  #       {
  #         semvar[i] <- variogramLine(varmod,dist_vector=dist[i],dir=c(cos((dir.hor[i]+90)*pi/180),sin((dir.hor[i]+90)*pi/180),0))$gamma
  #       }
  for(i in 1:ndirs)
  {
    semvar[which(dir.hor==dirs[i])] <- variogramLine(varmod,dist_vector=dist[which(dir.hor==dirs[i])],dir=c(cos((dirs[i]+90)*pi/180),sin((dirs[i]+90)*pi/180),0))$gamma
  }
  return(semvar)
}