#' mirror a matrix
#' 
#' @export
mirror.matrix <- function(mat, mirror.type='both')
{
nr <- length(mat[,1])
nc <- length(mat[1,])
matmirror <- matrix(nrow=nr, ncol=nc)

    if(mirror.type=='both')
    {
        matmirror2 <- matrix(nrow=nr,ncol=nc)
        for(j in 1:nc) matmirror2[,j] <- mat[,nc-j+1]
        for(i in 1:nr) matmirror[i,] <- matmirror2[nr-i+1,]
    }
    if(mirror.type=='horizontal')
    {
        for(j in 1:nc) matmirror[,j] <- mat[,nc-j+1]
    }
    if(mirror.type=='vertical')
    {
        for(i in 1:nr) matmirror[i,] <- mat[(nr-i+1),]
    }
    if(mirror.type!='both' & mirror.type!='horizontal' & mirror.type!='vertical')
    {
        cat('Error: wrong mirror type specified.')
    }
    return(matmirror)
}