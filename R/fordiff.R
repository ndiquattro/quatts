#' Forward Difference Contrast Maker
#'
#' Creates a contrast matrix for forward difference coding used in regression
#' @param k Number of levels in the factor

fordiff <- function(k) {
  cmat = matrix(nrow = k, ncol = k -1)  # Create an empty matrix

  for (i in 1:ncol(cmat)) {
    cmat[1:i,i] = k-i
    cmat[(i+1):nrow(cmat),i] = i * -1
  }

  cmat = cmat / k
  return(cmat)
}
