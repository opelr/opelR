#' Transform dates imported from Excel
#'
#' Creates a vector of randomly-generated, normally-distributed numbers with specified mean and sd.
#' @param x
#' @param system Was data exported from Excel for Windows or Mac? Options: "windows", "mac"
#' @usage excel_date(42077, system = "windows")
#' @seealso as.Date
#' @export

excel_date <- function(x, system) {
  orig = ifelse(grepl("win", tolower(system)),
                "1899-12-30", "1904-01-01")

  return(as.Date(x, origin = orig))
}
