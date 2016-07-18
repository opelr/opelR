#' Convert Excel column names to numbers
#'
#' When passed a letter, this function returns the associated column number. Used for calling Excel columns by name.
#' @param col A vector or an R object, which is coercible to one by as.vector(x, "character")
#' @usage col2num(col)
#' @export
#' @examples col2num(c('T', 'AF'))

col2num<-Vectorize(function(col){
  require("R.oo", quietly = TRUE)
  col <- unlist(strsplit(toupper(col), ""))
  sum((charToInt(col) - charToInt("A") + 1) * (26 ^ seq(length(col) - 1, 0, -1)))
})
