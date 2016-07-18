#' Two-item permutations
#'
#' Returns a vector of all possible 2-item permutations for elements in a vector
#' @param x A vector or an R object, which is coercible to one by as.vector(x, "numeric")
#' @param repeats logical. Can elements combine with themselves?
#' @usage permutation(x, repeats = FALSE)
#' @export
#' @seealso combn A function that executes this much better
#' @seealso expand.grid
#' @examples permutation(x = c("a", "b", "c"), repeats = TRUE)

permutation <- function(x, repeats = FALSE){
  if (repeats == TRUE){
    (within(expand.grid(x, x, stringsAsFactors = FALSE), Var3<-paste0(Var1, Var2))$Var3)
  } else {
    (within(expand.grid(x, x, stringsAsFactors = FALSE)[apply(expand.grid(x, x, stringsAsFactors = FALSE), 1, function (y) {length(unique(y)) == 2}), ], Var3<-paste0(Var1, Var2))$Var3)
  }
}
