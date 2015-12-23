sisim <- function(newdata, harddata, vgms, lvms)
{

# newdata$ xyz
# harddata$ xyz + n columns
# vgms list with n vgm objects
# lvms matrix with n x m
    require(tcltk)
    #print('Initialising Sisim')
    # Variable initialisation
    n <- length(vgms)
    m <- nrow(newdata)
    require(gstat)
    dummy <- T
    gstatdata <- NULL
    randompath <- sample(1:m)
    randomnumbers <- runif(m, min=0, max=1)
    simdata <- NULL
    hardandsimdata <- harddata

    # Variogram models parameters
    maxdist <- 200
    nmax <- 500
    nmin <- 200
    #print('Starting Loop')
    # Start Loop ###############################################################
    #path <- 1 #### test of loop!!!
    pb <- tkProgressBar("Sisim progress bar", "Percentage of simulated new data points", 0, 100, 0)
    for(path in 1:m)                 # length(randompath)
    {
        newpoint <- newdata[randompath[path],]
        localprop <- lvms[path,]
        if(prod(localprop=='NaN')==1) {
        x <- rep(NA, n)
        } else {
        indicator.probs <- NULL
        for(i in 1:n)
        {
            g <- gstat(id=paste('sbt',i,sep=''), formula=hardandsimdata[,(3+i)]~1, locations= ~ x + y + z , data=hardandsimdata, model=vgms[[i]], maxdist=maxdist, nmax=nmax, nmin=nmin, beta=lvms[path,i], vdist=T, fill.cross=F)
            indicator.probs[i] <- predict.gstat(g, newpoint, debug.level=0)[4]
        }
        # Correct for order relation deviations
        predperc <- indicator.probs; predperc[which(predperc <= 0)] <- 0; predperc[which(predperc >= 1)] <- 1

        if(sum(as.numeric(predperc))<=0) newpointSBT <- which.max(localprop)
        if(sum(as.numeric(predperc))> 0)
        {
            predperc <- as.numeric(predperc)/sum(as.numeric(predperc))
            newpointSBT <- which(cumsum(as.numeric(predperc))>randomnumbers[path])[1]
        }

        x <- rep(0, n)
        }
        for(i in 1:n){ if(newpointSBT == i) { x[i]<-1 } }
        #print('First rbind')
        add <- as.data.frame(cbind(x=newpoint$x, y=newpoint$y, z=newpoint$z, rbind(x[1:n]))); for(i in 1:n) names(add)[3+i] <- paste('sbt',i,sep='')
        #print('Second rbind')
        hardandsimdata <- rbind(hardandsimdata, add)

        #### add new point to harddata
        #print('Third rbind')
        simdata <- rbind(simdata, add)
        cat(paste('Sequential indicator simulation progress:',floor(path/m*100),'%\n'))
        info <- sprintf("%d%% of simulated new data points done", floor(path/m*100))
        setTkProgressBar(pb, floor(path/m*100), label=info)
    }
    close(pb)
    return(simdata)
}
