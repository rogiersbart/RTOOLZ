################################################################################
# FUNCTION - SGeMS write square blockdata ######################################
################################################################################
write.square.blockdata <- function(centersX, centersY, lengthX, lengthY, values, error, outfile)
{
    blocknumbers <- length(centersX)
    blockvalues <- values
    blockwest <- centersX - (lengthX/2)
    blockeast <- centersX + (lengthX/2)
    blocknorth <- centersY + (lengthY/2)
    blocksouth <- centersY - (lengthY/2)
    blockerror <- error
    blockdata <- data.frame(nr=blocknumbers, value=blockvalues, east=blockeast, west=blockwest, north=blocknorth, south=blocksouth, error=blockerror)
    
    # This function writes an SGeMS blockdata file with rectangular blocks
    # The header
    cat('Blockdata file created in R\n', file=outfile)
    # Number of blocks
    cat(length(blockdata[,1]), file=outfile, append=TRUE)
    cat('\n', file=outfile, append=TRUE)
    # Block loop
    for(i in 1:length(blockdata[,1]))
    {
        write(paste('block #', i, sep=''), file=outfile, append=TRUE)          # block name
        write(blockdata$value[i], file=outfile, append=TRUE)    # block value
        write(blockdata$error[i], file=outfile, append=TRUE)    # block error
        write(paste(blockdata$west[i], blockdata$south[i], '0', sep=' '), file=outfile, append=TRUE)    # lower left corner
        write(paste(blockdata$west[i], blockdata$north[i], '0', sep=' '), file=outfile, append=TRUE)    # upper left corner
        write(paste(blockdata$east[i], blockdata$north[i], '0', sep=' '), file=outfile, append=TRUE)    # upper right corner
        write(paste(blockdata$east[i], blockdata$south[i], '0', sep=' '), file=outfile, append=TRUE)    # lower right corner
    }
}