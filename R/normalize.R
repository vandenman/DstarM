#' Normalize two pdfs
#' @param x a numeric matrix or vector where each column represents a probability density function evaluated at the grid defined in \code{tt}.
#' @param tt a numeric grid defined in \code{x}.
#' @param props the value each density should integrate to.
#' @examples
#' tt <- seq(0, 5, .01)
#' x1 <- cbind(
#' 	dexp(tt, .5),
#' 	dexp(tt, 2)
#' )
#' x2 <- normalize(x1, tt)
#' x3 <- normalize(x1, tt, props = c(.1, .9))
#'
#' matplot(tt, cbind(x1, x2, x3), type = "l",
#' 		col = rep(1:3, each = 2), lty = rep(1:2, 3), las = 1, bty = "n")
#' legend("topright", legend = rep(paste0("x", 1:3), each = 2),
#' 	   col = rep(1:3, each = 2), lty = rep(1:2, 3), bty = "n")



#' @export
normalize <- function(x, tt, props = NULL) {

	x <- as.matrix(x)
	if (any(is.infinite(x), is.na(x), x < 0))
		stop("x contains missing, infinite, or negative values.")
	ncondition <- ncol(x) / 2
	mm <- matrix(0, ncondition * 2, ncondition)
	mm[1:dim(mm)[1L] + dim(mm)[1L] * rep(1:dim(mm)[2L] - 1, each = 2)] <- 1

	if (!is.null(props)) {
		props <- props / sum(props)
		if (any(is.infinite(props), is.na(props), props < 0))
			stop("props contains missing, infinite, or negative values.")

		x <- x %*% diag(props)
	}

	x <- x %*% (diag(dim(x)[2L]) / rep(apply(x %*% mm, 2, simpson, x = tt), each = 2))

	return(x)
}
