#' Transform gstat object to linear model of coregionalization
#' 
#' @param g gstat object
#' @param correct_diagonal
#' @return gstat object
#' @export
gstat_transform_to_lmc <- function (g, correct_diagonal = 1, ...) 
{
  warning('Derived from gstat fit.lmc function?')
  posdef = function(X) {
    q = eigen(X)
    d = q$values
    d[d < 0] = 0
    q$vectors %*% diag(d, nrow = length(d)) %*% t(q$vectors)
  }
  if (!inherits(g, "gstat")) 
    stop("g should be of class gstat")
  n = names(g$data)
  m = g$model[[n[1]]]
  for (k in 1:nrow(m)) {
    psill = matrix(NA, nrow = length(n), ncol = length(n))
    for (i in 1:length(n)) {
      for (j in i:length(n)) {
        name = ifelse(i == j, n[i], cross.name(n[i], 
                                               n[j]))
        psill[i, j] = psill[j, i] = g$model[[name]][k, 
                                                    "psill"]
      }
    }
    psill = posdef(psill)
    diag(psill) = diag(psill) * correct_diagonal
    for (i in 1:length(n)) {
      for (j in i:length(n)) {
        name = ifelse(i == j, n[i], cross.name(n[i], 
                                               n[j]))
        g$model[[name]][k, "psill"] = psill[i, j]
      }
    }
  }
  g
}