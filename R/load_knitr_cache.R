#' Load all knitr cache data
#' 
#' @export
load_knitr_cache <- function() {
  rdx_files <- dir('cache')[grep('.rdx',dir('cache'))]
  for(i in 1:length(rdx_files)) lazyLoad(substr(paste0('cache/',rdx_files[i]),1,nchar(paste0('cache/',rdx_files[i]))-4),envir=globalenv())
}