#' Export to GraphPad Prism and Excel
#'
#' Perform Bonferroni-corrected chi-sqaure post-hoc testing on a table or matrix.
#' @param formula A string
#' @param data A dataframe.
#' @param excel Pastes data into your clipboard for easy pasting into Excel. Defaults to False
#' @usage prism(formula, data, excel = F)
#' @export
#' @examples prism("mpg ~ carb", mtcars, excel = T)

prism <- function(formula, data, SEM = T, excel = F) {
  if(! is.data.frame(data)) stop("'data' must be a data frame")

  formula <- gsub(" ", "", formula)
  form <- as.formula(formula)

  x <- strsplit(formula, "~")[[1]][1]
  y <- strsplit(formula, "~")[[1]][2]
  y <- strsplit(y, "+", fixed = T)[[1]]

  if (SEM) {
    fn <- function(x) {sd(x, na.rm = T)/sqrt(length(x))}
    column_name <- "SEM"
  } else {
    fn <- function(x) {sd(x, na.rm = T)}
    column_name <- "SD"
  }

  mea <- aggregate(form, data, mean)
  sem <- aggregate(form, data, function(ii) fn(ii))
  n   <- aggregate(form, data, length)

  if (excel == T) {
    xxx <- merge(mea, sem, by = c(y))
    out <- apply(signif(xxx[-1], 3), 1, function(x) paste0(paste0(round(x, 2), collapse = " ("), ")"))
    names(out) <- xxx[, 1]
    write.table(t(out), 'clipboard', sep = "\t", col.names = F, row.names = F)

  } else {
    xxx <- merge(mea, sem, by = c(y))
    xxx <- merge(xxx, n, by = c(y))

    colnames(xxx) <- c(y, 'Mean', column_name, 'n')

    return(xxx)
  }
}
