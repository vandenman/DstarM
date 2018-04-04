#' Normalize two pdfs
#' @param x a numeric matrix or vector where each column represents a probability density function evaluated at the grid defined in \code{tt}.
#' @param tt a numeric grid defined in \code{x}.
#' @param props the value each density should integrate to.

#' @export
normalize <- function(x, tt, props = NULL) {

	x <- as.matrix(x)
	ncondition <- ncol(x) / 2
	mm <- matrix(0, ncondition * 2, ncondition)
	mm[1:dim(mm)[1L] + dim(mm)[1L] * rep(1:dim(mm)[2L] - 1, each = 2)] <- 1

	if (!is.null(props))
		x <- x %*% diag(props)

	x <- x %*% (diag(dim(x)[2L]) / rep(apply(x %*% mm, 2, simpson, x = tt), each = 2))

	return(x)
}
