#' Normally-Distributed Vector
#'
#' Creates a vector of randomly-generated, normally-distributed numbers with specified mean and sd.
#' @param n Number of items
#' @param mean Mean of the normal distribution
#' @param sd SD of the normal distribution
#' @usage vnorm(100, 15, 3)
#' @seealso scale
#' @seealso rnorm
#' @export
#' @examples x <- vnorm(100, 15, 3)
#' @examples all(length(x) == 100, mean(x) == 15, sd(x) == 3)

vnorm <- function(n, mean, sd) {as.vector(mean + sd * scale(rnorm(n)))}
