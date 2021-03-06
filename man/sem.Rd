% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sem.R
\name{sem}
\alias{sem}
\title{Standard Error of the Mean}
\usage{
sem(x, na.rm = FALSE)
}
\arguments{
\item{x}{A numeric vector or an R object, which is coercible to one by as.vector(x, "numeric")}

\item{na.rm}{logical. Should missing values be removed?}
}
\description{
Computes the standard error of the mean (SEM)
}
\examples{
sem(1:4)
}
\seealso{
sd for standard deviation
}
