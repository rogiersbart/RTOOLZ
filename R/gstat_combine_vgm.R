#' Combine separately modelled isotropic horizontal and vertical variograms into a single anisotropic variogram
#' 
#' @param horvgm the horizontal variogram
#' @param vervgm the vertical variogram
#' @return an anisotropic gstat variogram model
#' @export
gstat_combine_vgm <- function(horvgm, vervgm)
{
    outvgm <- horvgm
    for(i in 1:nrow(horvgm))
    {
        #print(i)
        if(outvgm[i,'model']=='Nug') {
                outvgm[i,'model'] <- 'Sph'
                outvgm[i,'range'] <- 100000
                outvgm[i,'ang1'] <- 0
                outvgm[i,'ang2'] <- 90
                outvgm[i,'ang3'] <- 0
                outvgm[i,'anis1'] <- 0.01/100000
                outvgm[i,'anis2'] <- outvgm[i,]$anis1
                outvgm <- vgm(psill=vervgm[i,'psill'], model='Sph', range=0.01, anis=c(0,0,0,1,0.01/100000),add.to=outvgm)
        } else {
        outvgm[i,]$ang1 <- 0
        outvgm[i,]$ang2 <- 90
        outvgm[i,]$ang3 <- 0
        outvgm[i,]$range <- 100000
        outvgm[i,]$anis1 <- as.numeric(outvgm[i,'range'])/100000
        outvgm[i,]$anis2 <- outvgm[i,]$anis1
        outvgm <- vgm(psill=vervgm[i,'psill'], model=vervgm[i,'model'], range=100000, anis=c(0,0,0,1,as.numeric(vervgm[i,'range'])/100000),add.to=outvgm)
        }
    }
    return(outvgm)
}