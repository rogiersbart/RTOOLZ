#' Get n points evenly spread on a circle
#' 
#' @param centre_x x coordinate of the circle centre
#' @param centre_y y coordinate of the circle centre
#' @param radius radius of the circle
#' @param n number of points on the circle
#' @return Data frame with x and y coordinates
#' @export
points_on_circle <- function(centre_x, centre_y, radius, n)
{
  # This function creates a data frame with n points equally spaced on a circle
  alpha = pi * 2 / n
  pointsX = c(rep(NA,n))
  pointsY = c(rep(NA,n))
  i = -1
  while( i < n-1 )
  {
    #print(i)
    theta = alpha * i
    pointOnCircleX = (cos( theta ) * radius) + centre_x
    pointOnCircleY = (sin( theta ) * radius) + centre_y 
    pointsX[ i+2 ] = pointOnCircleX
    pointsY[ i+2 ] = pointOnCircleY
    i <- i+1
    #print(c(pointOnCircleX, pointOnCircleY))
  }
  return(data.frame(x=pointsX, y=pointsY))
}