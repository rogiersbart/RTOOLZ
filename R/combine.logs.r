################################################################################
# FUNCTION - combine.logs ######################################################
################################################################################
combine.logs <- function(R1k, R1cptinfo, cpt, ra, ralsd)
{
#    R1k <- read.csv(kfile, header=T)
#    R1cptinfo <- read.csv(cptinfo, header=T)
    R1cpt <- cpt[which(as.character(cpt$id)==as.character(R1cptinfo$Filename)),]
    R1cpt <- cbind(R1cpt, z=R1cptinfo$Z-R1cpt$gecorrigeerdediepte)
    #R1cpt <- cbind(R1cpt, lsd=as.numeric(localshiftdistance(R1cpt, ralsd)[,1]))
    #head(R1k)
    #head(R1cpt)
    #print(R1cpt$conusweerstand)
    #print(R1cpt$z)
    R1cptselection <- R1cpt[1:length(R1k[,1]),]

    i<-1
    while(i<=length(R1k[,1]))
    {
        WithinRange <- which(R1cpt$z < (R1k$Z[i]+ra) & R1cpt$z > (R1k$Z[i]-ra))
        R1cptselection[i,] <- mean(R1cpt[WithinRange,])
        i<-i+1
    }
    print(head(R1cptselection))
# print(R1cptselection$conusweerstand)
#    print(R1cptselection$z)
#    print(min(R1cpt$z))
#    print(R1k$Z)
    return(cbind(R1k,R1cptselection))
}