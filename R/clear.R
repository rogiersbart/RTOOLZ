#' Remove all objects from an environment
#' 
#' @param name name of the environment to clear; defaults to global environment
#' @export
clear <- function(name = .GlobalEnv) {
  rm(list = ls(name), envir = as.environment(name))
}