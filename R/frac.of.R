#' Fraction of factor in a vector
#'
#' Returns the percentage of observations for a value in a vector of characters or factors
#' @param x A vector or an R object, which is coercible to one by as.vector(x, "factor")
#' @param val The value of interest
#' @usage frac.of(x, val)
#' @export
#' @examples frac.of(letters, 'c')

frac.of <- function(x, val) length(x[x %in% val]) / length(x)
