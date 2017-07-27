#' Descriptives of reaction time data
#'
#' @param data A reaction time dataset. Must be a dataframe with $rt, $condition
#' and $response.
#' @param plot Logical, should a density plot of all condition-response pairs be made?
#' @param verbose Logical, should a table of counts and proportions be printed?
#'
#' @return Invisibly returns an object of class 'D*M'. It's first element is \code{table} and contains raw counts and proportions for
#' condition response pairs, conditions, and responses. It's second element \code{plot} contains a ggplot object.
#'
#' @details This function and \code{\link{rtHist}} are helper functions to inspect raw data.
#'
#' @examples
#' tt = seq(0, 5, .01)
#' pars = matrix(.5, 5, 2)
#' pars[1, ] <- 1
#' pars[2, ] <- c(0, 2)
#' dat = simData(n = 3e3, pars = pars, tt = tt, pdfND = dbeta(tt, 10, 30))
#' x = rtDescriptives(dat)
#'
#' print(x$table, what = 'cr')
#' print(x$table, what = 'c')
#' print(x$table, what = 'r')


#' @export
rtDescriptives = function(data, plot = TRUE, verbose = TRUE) {

	if (!all(c("rt", "response") %in% colnames(data)))
		stop("'data' must be a data.frame with the following columnnames:\n - 'rt' for reaction times.\n - 'response' for responses.\n - 'condition' for manipulations (optional).")

	lenCR = tapply(data$rt, list(data$condition, data$response), length)
	d = dim(lenCR)
	lenC = .rowSums(lenCR, m = d[1], n = d[2])
	lenR = .colSums(lenCR, m = d[1], n = d[2])

	table = list(
		counts = list(
			conditionResponse = lenCR,
			condition = lenC,
			response = lenR),
		props = list(
			conditionResponse = lenCR / lenC,
			condition = lenC / sum(lenC),
			response = lenR / sum(lenR)),
		responses = colnames(lenCR)
	)
	class(table) = 'DstarM'
	if (verbose)
		print(table)

	if ("condition" %in% colnames(data)) {

		mapping <- ggplot2::aes_string(x = "rt", group = "interaction(response, condition)",
									   fill = "interaction(response, condition)")

	} else {

		mapping <- ggplot2::aes_string(x = "rt", group = "response", fill = "response")

	}

	graph <- ggplot2::ggplot(data = data, mapping = mapping) +
		ggplot2::geom_density(alpha = .25)

	if (plot)
		print(graph)

	out <- list(table = table, graph = graph)
	class(out) <- "D*M"
	return(invisible(out))

}





