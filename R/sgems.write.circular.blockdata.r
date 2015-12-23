################################################################################
# FUNCTION - SGeMS write circular blockdata ####################################
################################################################################
sgems.write.circular.blockdata <- function(centersX, centersY, radii, values, error, npolygon=20, outfile, plot=F)
{
   ### This function writes a 2D SGeMS blockdata file with default icosagon approximation of circular support blocks
   ## Header
   cat('Blockdata file created in R\n', file=outfile)
   ## Number of blocks
   cat(length(centersX), file=outfile, append=TRUE)
   cat('\n', file=outfile, append=TRUE)
   if(plot) plot(NULL, xlim=c(-500,5000), ylim=c(-500,5000), xlab='X (mm)', ylab='Y (mm)')
   ## Block loop
   for(i in 1:length(centersX))
   {
      write(paste('block #', i, sep=''), file=outfile, append=TRUE)  # block name
      write(values[i], file=outfile, append=TRUE)                    # block value
      write(error[i], file=outfile, append=TRUE)                     # block error
      circlepoints <- pointsOnCircle(centersX[i], centersY[i], radii[i], npolygon)
      circlepoints <- data.frame(circlepoints, z=circlepoints$x*0)
      if(plot) lines(rbind(circlepoints,circlepoints[1,]))
      write.table(circlepoints,file=outfile, append=TRUE, sep='\t', col.names=FALSE, row.names=FALSE)
   }

}
pointsOnCircle <- function(centerX, centerY, radius, n)
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
        pointOnCircleX = (cos( theta ) * radius) + centerX
        pointOnCircleY = (sin( theta ) * radius) + centerY 
        pointsX[ i+2 ] = pointOnCircleX
        pointsY[ i+2 ] = pointOnCircleY
        i <- i+1
        #print(c(pointOnCircleX, pointOnCircleY))
    }
    return(data.frame(x=pointsX, y=pointsY))
} 