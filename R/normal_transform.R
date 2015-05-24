#' Normal transform
#' 
#' @export
normal_transform <- function(dat)
{
  attributes(dat) <- list(transform_type='normal',transform_data=dat)
  dat[order(dat)] <- (c(1:length(dat))-0.5)/length(dat)
  dat <- qnorm(dat)
  return(dat)
}