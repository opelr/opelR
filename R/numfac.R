#' Convert factors to numeric
#'
#' Converts factors to numeric. Replaces 'as.numeric(as.character(...))'
#' @param ... An array or an R object, which is coercible to one by as.array(x, "factor")
#' @usage numfac(...)
#' @export

numfac <- function(...) {
  as.numeric(as.character(...))
}
