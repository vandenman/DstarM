% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obsQuantiles.R
\name{obsQuantiles}
\alias{obsQuantiles}
\title{Calculate model fit}
\usage{
obsQuantiles(data, probs = seq(0, 1, 0.01), what = "cr")
}
\arguments{
\item{data}{A dataframe with: a column named \code{rt} containing response times in ms,
a column named \code{response} containing at most 2 response options, and an
optional column named \code{condition} containing a numeric index as to which conditions
observations belong.}

\item{probs}{vector of probabilities for which the corresponding values should be called}

\item{what}{Character. \code{'cr'} if the quantiles are to be calculated per condition-response
pair, \code{'c'} if the quantiles are to be calculated per condition, and
\code{'r'} if the quantiles are to be calculated per response.}
}
\description{
This function is nothing but a wrapper for \code{\link{quantile}}.
}
\examples{
tt = seq(0, 5, .01)
pars = c(.8, 2, .5, .5, .5, # condition 1
        .8, 3, .5, .5, .5,  # condition 2
        .8, 4, .5, .5, .5)  # condition 3
pdfND = dbeta(tt, 10, 30)
# simulate data
data = simData(n = 3e3, pars = pars, tt = tt, pdfND = pdfND)
probs = seq(0, 1, .01)
q = obsQuantiles(data, probs = probs)
matplot(probs, q, type = 'l', las = 1, bty = 'n')
}
