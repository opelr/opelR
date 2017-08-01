#' Replace NAs
#'
#' Replace all NA's with another value in a data.frame or matrix.
#' @param dat A data.frame or matrix
#' @param repl Replacement value. Defaults to 0.
#' @usage replace_na(dat, repl)
#' @export
#' @examples replace_na(matrix(c(1,NA,3,4), ncol = 2), repl = 0)


replace_na <- function(dat, repl = 0) {
  dat[is.na(dat)] <- repl
  return(dat)
}
