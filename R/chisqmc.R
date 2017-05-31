#' Chi-sqaure post-hoc testing
#'
#' Perform Bonferroni-corrected chi-sqaure post-hoc testing on a table or matrix.
#' @param mat A table or matrix
#' @param alpha Alpha significance value. Defaults to 0.05
#' @usage chisq.mc(matrix, alpha = 0.05)
#' @export
#' @examples chisq.mc(table(mtcars$gear, mtcars$cyl), alpha = 0.05)


chisq.mc <- function(mat, alpha = 0.05) {
  if(is.null(rownames(mat))) stop("Matrix must have row names")
  comparisons <- combn(rownames(mat), 2, simplify = F)

  out <- do.call("rbind", lapply(comparisons, function(ii) {
    nam <- paste(ii, collapse = "-")
    chi  <- chisq.test(mat[ii, ])
    X2   <- signif(chi$statistic, 4)
    p   <- signif(chi$p.value * length(comparisons), 4)
    p   <- ifelse(p > 1, 1, p)

    sig <- ifelse(p > alpha, "", "*")


    out <- cbind(nam, X2, p, sig)
    rownames(out) <- ""
    return(out)
  }))

  colnames(out) <- c("Comparison", "X2", "p.value", "Signif")
  out <- data.frame(out)

  return(out)
}
