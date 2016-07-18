#' Merge and preserve level order of factors
#'
#' concatenates factors while preserving original level order
#' @param ... An array or an R object, which is coercible to one by as.array(x, "factor")
#' @usage mergeFac(...)
#' @export

mergeFac <- function(...) {
  l <- list(...)
  factor(do.call("c", lapply(l, function(x) levels(x)[x])), levels = unique(do.call("c", lapply(l, levels))))
}
