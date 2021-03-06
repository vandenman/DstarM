% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estDensities.R
\name{Density}
\alias{Density}
\title{Density function}
\usage{
Density(rt, tt)
}
\arguments{
\item{rt}{vector of reaction times}

\item{tt}{grid to evaluate the density on}
}
\value{
a vector of \code{length(tt)}
}
\description{
Density function
}
\details{
Can be passed to the argument \code{densityMethod} of \code{\link{estDstarM}}. This function is a minimal
example to use as custom smoothing function.
}
\examples{
x <- rgamma(1e5, 1, 1)
tt <- seq(0, 5, .01)
d <- Density(x, tt)
hist(x, freq = FALSE)
lines(tt, DstarM:::Density(x, tt))
}
