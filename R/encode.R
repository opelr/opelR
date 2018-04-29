#' Find continuous sequences in discontinuous sequence
#'
#' When passed a vector of discontinuous numbers, this function returns a list of each continuous sequence
#' @param x A vector or an R object, which is coercible to one by as.vector(x, "numeric")
#' @param as.list logical. Should the vector be returned as a split list?
#' @usage encode(x, as.list = FALSE)
#' @export
#' @examples encode(c(1:4, 7:10), as.list = FALSE)
#' @examples encode(c(1:4, 7:10), as.list = TRUE)

encode <- function(x, as.list = FALSE){
  run_lengths <- rle(x - seq_along(x))
  run_lengths$values <- seq_along(run_lengths$values)
  i_rle <- inverse.rle(run_lengths)

  if (as.list) {
    return(split(x, i_rle))
  } else{
    return(i_rle)
  }
}

