#' Read a GSLIB file
#' 
#' @param file Filename
#' @param nodatavalue Numeric value for missing data
#' @return data frame
#' @export
read_gslib <- function(file, nodatavalue=-99999, type='points')
{
if(type=='points') {
  char <- scan(file, what=character())
  nums <- which(as.numeric(char)!='NaN')
  nrcol <- as.numeric(char[nums[1]])
  begin <- nums[2]
  eind <- nums[length(nums)]
  nrrows <- (eind-begin+1)/nrcol
  indices <- seq(1,nrrows*nrcol-1,nrcol)
  mat <- matrix(ncol=nrcol, nrow=nrrows)
  i <- 1
  while(i <= nrcol) { mat[,i] <- as.numeric(char[(indices+i+begin-2)]); i <- i+1 }
  mat <- as.data.frame(mat)
  names(mat) <-  c(char[(nums[1]+c(1:nrcol))])
  for(i in 1:ncol(mat)) mat[which(mat[,i]==nodatavalue),i] <- 'NA'
  return(mat)
} else if(type=='grid') {
  char <- scan(file, what=character())
  nums <- which(as.numeric(char)!='NaN')
  
  nx <- as.numeric(char[1])
  ny <- as.numeric(char[2])
  nz <- as.numeric(char[3])
  xsiz <- as.numeric(char[4])
  ysiz <- as.numeric(char[5])
  zsiz <- as.numeric(char[6])
  xmn <- as.numeric(char[7])
  ymn <- as.numeric(char[8])
  zmn <- as.numeric(char[9])
  nvar <- as.numeric(char[10])
  var_names <- c(char[11:(12-nvar)])
  
  nrcol <- nvar
  begin <- 11+nvar
  eind <- nums[length(nums)]
  nrrows <- (eind-begin+1)/nrcol
  indices <- seq(1,nrrows*nrcol,nrcol)
  mat <- matrix(ncol=nrcol, nrow=nrrows)
  i <- 1
  while(i <= nrcol) { mat[,i] <- as.numeric(char[(indices+i+begin-2)]); i <- i+1 }
  
  dat <- list()
  for(i in 1:ncol(mat)) dat[[i]] <- array(mat[,i],dim=c(ny,nx,nz))
  names(dat) <- var_names
  return(dat)
} else {
  stop('Please provide valid gslib file type.')
}

}