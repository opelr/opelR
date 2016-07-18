#' Standard Error of the Mean
#'
#' Computes the standard error of the mean (SEM)
#' @param x A numeric vector or an R object, which is coercible to one by as.vector(x, "numeric")
#' @param na.rm logical. Should missing values be removed?
#' @usage sem(x, na.rm = FALSE)
#' @seealso sd for standard deviation
#' @export
#' @examples sem(1:4)

sem <- function(x, na.rm = FALSE){
  sd(x, na.rm)/sqrt(length(x))
}
