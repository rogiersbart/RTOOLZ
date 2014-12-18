#' Write number in full
#' 
#' @param x A number.
#' @export
full <- function(x)
{
  if(x >= 10) return(x)
  if(x == 1) return('one')
  if(x == 2) return('two')
  if(x == 3) return('three')
  if(x == 4) return('four')
  if(x == 5) return('five')
  if(x == 6) return('six')
  if(x == 7) return('seven')
  if(x == 8) return('eigth')
  if(x == 9) return('nine')
}
