% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frac.of.R
\name{frac.of}
\alias{frac.of}
\title{Fraction of factor in a vector}
\usage{
frac.of(x, val)
}
\arguments{
\item{x}{A vector or an R object, which is coercible to one by as.vector(x, "factor")}

\item{val}{The value of interest}
}
\description{
Returns the percentage of observations for a value in a vector of characters or factors
}
\examples{
frac.of(letters, 'c')
}
