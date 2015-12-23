#' Create a regular rectangular 3d grid coordinates data frame
#' 
#' @param origin xyz coordinates of the origin
#' @param dim dimensions of the grid
#' @param width width of the cells
#' @return data frame with xyz coordinates for each grid node
#' @export
create_3d_grid <- function(origin, dim, width)
{
  X=rep(seq(origin[1],(origin[1]+(dim[1]-1)*width[1]),width[1]), (dim[2]*dim[3]))
  Y=rep(seq(origin[2],(origin[2]+(dim[2]-1)*width[2]),width[2]),dim[3],each=dim[1])
  geoz=rep(seq(origin[3],(origin[3]+(dim[3]-1)*width[3]),width[3]),each=(dim[1]*dim[2]))
  return(data.frame(x=X, y=Y, z=geoz))
}