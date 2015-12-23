model.variograms.probs <- function(harddata, model='Exp')
{
        sbt.vgm <- NULL; vgm.ver <- NULL; vgm.hor <- NULL; expvar.hor <- NULL; expvar.ver <- NULL
        sbt.max <- ncol(harddata)-3
        for(sbt.class in 1:sbt.max)
        {
            ### Fit vertical one with different models
            sbt.indicator <- as.numeric(harddata[,(3+sbt.class)])
            gstatobject <- gstat(id='sbt',formula= sbt.indicator ~ 1, locations= ~ x+y+z, data=harddata)

            expvar.ver[[sbt.class]] <- variogram(gstatobject, width=1, beta=90, cutoff=20)
            varmodver <- vgm(psill=var(sbt.indicator), model=model, range=3, nugget=(var(sbt.indicator)/10))#, add.to=vgm(psill=0.01, model='Per', range=30, add.to=vgm(psill=0.01, model='Lin', range=30, add.to=vgm(psill=0.01, model='Exp', range=30))))
            vgm.ver[[sbt.class]] <- fit.variogram(expvar.ver[[sbt.class]], varmodver, fit.sills=T, fit.ranges=T)

            expvar.hor[[sbt.class]] <- variogram(gstatobject, width=10, beta=0, cutoff=200)
            varmodhor <- vgm(psill=var(sbt.indicator), model=model, range=30, nugget=(var(sbt.indicator)/10))
            vgm.hor[[sbt.class]] <- fit.variogram(expvar.hor[[sbt.class]], varmodhor, fit.sills=T, fit.ranges=T)

            par(mfrow=c(2,1), mar=c(5,5,1,5))
            plot(expvar.hor[[sbt.class]]$dist, expvar.hor[[sbt.class]]$gamma, xlim=c(0,200), ylim=c(0,max(expvar.hor[[sbt.class]]$gamma)), xlab='Horizontal lag distance (m)', ylab='Semivariance')
            lines(variogramLine(vgm.hor[[sbt.class]], maxdist=200), col='red', lwd=3)
            plot(expvar.ver[[sbt.class]]$dist,expvar.ver[[sbt.class]]$gamma, xlim=c(0,20), ylim=c(0,max(expvar.ver[[sbt.class]]$gamma)), xlab='Vertical lag distance (m)', ylab='Semivariance')
            lines(variogramLine(vgm.ver[[sbt.class]], maxdist=20), col='red', lwd=3)
            print(vgm.hor[[sbt.class]])
            print(vgm.ver[[sbt.class]])
            ### Write 3d variogram model
            sbt.vgm[[sbt.class]] <- combine.vgm(vgm.hor[[sbt.class]], vgm.ver[[sbt.class]])
            cat(paste('Variogram modelling progress:',floor(sbt.class/sbt.max*100),'%\n'))
        }
        return(sbt.vgm)
}