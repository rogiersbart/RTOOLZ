#' @export
gstat_variogram_lines <- function(varmod,maxdist,dir.hor)
{
  varmodLines <- data.frame(variogramLine(varmod,maxdist,dir=c(cos((dir.hor[1]+90)*pi/180),sin((dir.hor[1]+90)*pi/180),0)),dir.hor=dir.hor[1])
  if(length(dir.hor > 1))
  {
    for(i in 2:length(dir.hor))
    {
      varmodLines <- rbind(varmodLines,data.frame(variogramLine(varmod,maxdist,dir=c(cos((dir.hor[i]+90)*pi/180),sin((dir.hor[i]+90)*pi/180),0)),dir.hor=dir.hor[i]))
    }
  }
  return(varmodLines)
}