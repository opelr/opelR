#' Find continuous sequences in discontinuous sequence
#'
#' When passed a vector of discontinuous numbers, this function returns a list of each continuous sequence
#' @param x A vector or an R object, which is coercible to one by as.vector(x, "numeric")
#' @param print logical. Should the list be printed to the console?
#' @usage encode(x, print = FALSE)
#' @export
#' @examples encode(c(1:4, 7:10))

encode<-function(x){
  rr <- rle(x - seq_along(x))
  rr$values <- seq_along(rr$values)
  s <- split(x, inverse.rle(rr))

  return(s)
}

