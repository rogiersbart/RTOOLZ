#' Insert
#' 
#' @export
insert <- function(original_string,insert_string,index=0)
{
  return(paste0(substr(original_string,0,index-1),insert_string,substr(original_string,index,nchar(original_string))))
}