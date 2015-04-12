points.distance <- function(x1,y1,x2,y2)
{
  return( sqrt( (x2-x1)^2 + (y2-y1)^2 ))
}
points.distance.3d <- function(x1,y1,z1,x2,y2,z2,verticalExaggeration=1)
{
  z1 <- verticalExaggeration * z1  
  z2 <- verticalExaggeration * z2
  return( sqrt( (x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2 ))
}