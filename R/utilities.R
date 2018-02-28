# the functions in this file are mainly helper functions for other files

errCheckData <- function(data, tt, h, by) {

	# Does error handling on the data and returns notes if necessary.
	if (length(by) != 1) {
		stop('Time grid tt must be equally spaced and length(unique(zapsmall(diff(tt)))) == 1 must be TRUE.',
			 call. = FALSE)
	}
	# if h is smaller than by, the kernel to be convoluted with consists only of 0s.
	if (h < by) {
		stop('Kernel bandwith must be larger than or equal to the step size of the time grid.',
			 call. = FALSE)
	}

	if (!all(c("rt", "response") %in% colnames(data)))
		stop("'data' must be a data.frame with the following columnnames:\n - 'rt' for reaction times.\n - 'response' for responses.\n - 'condition' for manipulations (optional).")


	stopifnot(is.numeric(data$rt))
	if (!(length(unique(data$response)) == 2 | length(levels(data$response)) ==  2)) {
		stop('There need to be at least 2 response options in data$response. If only one response option has been observed, data$response should be a factor with 2 levels where the levels represent the response options.')
	}
	if (any(data$rt > max(tt))) {
		stop('Observations in data$rt outside of time grid. any(data$rt > max(tt)) must be FALSE.',
			 call. = FALSE)
	}
	# check if upper and lower appear in response options.
	note = NULL
	if (any(!(c('upper', 'lower') %in% data$response))) {
		rsp = unique(data$response)
		if (!(length(rsp) ==  1 & rsp[1] %in% c('upper', 'lower'))) {
			note = sprintf("Note: Unique responses (%s) are recoded to 'lower' and 'upper' respectively.\n",
						   paste(sort(rsp), collapse = ', '))
			cat(note)
		}
	}
	stopifnot(all(data$rt <= max(tt)))
	return(list(note = note, by = by))
}

errCheckOptim = function(Optim, values = c(1e-3, 1e3, 50, .9, 0, 0)) {

	# Does error handling on Optim and if necessary set defaults

	if (!is.list(Optim)) {
		stop('Optim must be a list', call. = FALSE)
	}
	ch = !(names(Optim) %in% names(formals(DEoptim::DEoptim.control)))
	if (sum(ch) == 1) {
		warning(sprintf('%s is not an argument of DEoptim.control(). It is unused.',
						paste0('Optim$', names(Optim)[ch])),
				call. = FALSE, immediate. = TRUE)
	} else if (any(ch)) {
		nms = paste0('Optim$', names(Optim)[ch])
		last = length(nms)
		nms1 = paste0(nms[-last],
					  ifelse(last > 2, ', ', ' '),
					  collapse = '')
		nms2 = nms[last]
		warning(sprintf('%sand %s are not arguments of DEoptim.control(). They are unused.',
						nms1, nms2),
				call. = FALSE)
	}
	names = c('reltol', 'itermax', 'steptol', 'CR', 'trace', 'parallelType')
	bounds = c(rep('<=', 3), rep('<', 3))
	for (i in 1:length(names)) {
		if (is.null(Optim[[names[i]]])) {
			Optim[[names[i]]] = values[i]
		} else {
			if (!is.numeric(Optim[[names[i]]])) {
				stop(sprintf('Optim$%s must be numeric.',
							 names[i]), call. = FALSE)
			}
			if (is.nan(Optim[[names[i]]])) {
				stop(sprintf('Optim$%s cannot contain NaN values.',
							 names[i]), call. = FALSE)
			}
			if (do.call(bounds[i], list(Optim[[names[i]]], 0))) {
				stop(sprintf('Optim$%s must be %s.',
							 names[i],
							 ifelse(bounds[i] == '>', 'positive', 'non-negative')), call. = FALSE)
			}
		}
	}
	return(Optim)
}

errCheckDatamg <- function(mg, tt, ncondition) {
	if (!all(is.matrix(mg), dim(mg) == c(length(tt), 2*ncondition))) {
		stop('mg must be a matrix of length(tt) x ncondition.', call. = FALSE)
	}
}

getData <- function(formula, data, checks = TRUE) {

	noFormula <- is.null(formula)
	if (noFormula)
		formula <- response ~ rt + condition

	terms <- terms.formula(formula, data = data)
	origNames <- rownames(attr(terms, "factors"))
	if (length(origNames) > 3) {
		stop("More than 3 columns remained after specifying `model.frame(formula, data = data)`")
	} else if (length(origNames) == 2) {
		origNames[3] <- "condition"
		hasConditions <- FALSE
	} else {
		hasConditions <- TRUE
	}

	response <- origNames[1]
	rt <- origNames[2]
	condition <- origNames[3]

	data <- model.frame(formula, data = data)
	if (ncol(data) == 2) {
		data[[condition]] <-  1
	}

	if (!is.numeric(data[[rt]]) || any(dat[[rt]] < 0)) {
		stop(sprintf("%s is not numeric or has negative values.", origNames[2]))
	}
	return(list(data = data, rt = rt, response = response, condition = condition,
				hasConditions = hasConditions))
}

getCombinations <- function(group) {
	ii = matrix(NA, ((dim(group)[1L]) * (dim(group)[1L] - 1L)) / 2, dim(group)[2L])
	jj = ii
	for (i in 1:dim(group)[2L]) {
		temp1 = rep(stats::na.omit(group[-1L, i]), 1L:(length(stats::na.omit(group[, i])) - 1L))
		ii[1L:length(temp1), i] = temp1
		temp2 = group[unlist(lapply(2L:length(stats::na.omit(unique(group[, i]))), function(x) 1L:(x - 1L))), i]
		jj[1L:length(temp2), i] = temp2
	}
	ii = stats::na.omit(c(ii))
	jj = stats::na.omit(c(jj))
	return(list(ii = ii, jj = jj))
}

getFixed <- function(fixed, nms, useRcpp) {
	# impose paremter restrictions
	if (length(fixed) > 0) {
		fixedNames <- fixed[1, ]

		# remove fixed parameters
		indFixed = which(nms %in%  fixed[1L, ]) # get parameter indices to remove
		if (!length(indFixed)) {
			# something with provided names and possible names
			print(paste('names fixed:', fixed[1L, ]))
			print(paste(c('possible names:', nms), collapse = ' '))
			stop('no matches in names supplied by fixed and names of parameters.')
		}
		# something with if replacement is character then look up if it exists in names(lower)
		replacement = sapply(strsplit(fixed[2, ], ' '), `[[`, 1) # finds first value; extend to ' ' and */+- ? # does this equal making stuff in the restr.mat in the same column equal?
		fixedMat = rbind(fixed, replacement)
		fixed = list(fixedMat = fixedMat, indFixed = indFixed,
					 isNumeric = suppressWarnings(!is.na(as.numeric(fixedMat[3, ]))),
					 fixedNames = fixedNames,
					 fixed$anyFixed <- TRUE)

		if (useRcpp) {
			fixed$mat <- fixed2Rcpp(fixed, nms)
			fixed$anyFixed <- TRUE

		}
	} else if (useRcpp) {

		fixed <- list(mat = matrix(1, 1, 1),
					  anyFixed = FALSE)
	}

	return(fixed)
}

fixed2Rcpp = function(fixed, nms) {

	# Rcpp requires some more specific input
	fixednew = matrix(0, nrow = 5, ncol = ncol(fixed$fixedMat))
	fixednew[1, ] = 1 * fixed$isNumeric
	fixednew[2, ] = fixed$indFixed - 1 # convert R index to c++ index
	for (i in 1:ncol(fixednew)) {

		if (fixed$isNumeric[i]) {
			fixednew[3, i] = as.numeric(fixed$fixedMat[2, i])
		} else {

			vals = strsplit(x = fixed$fixedMat[2, i], split = " ")[[1]]
			if (any(length(vals) != 3,
					identical(vals, fixed$fixedMat[3, i]),
					!(vals[1] %in% nms),
					!vals[2] %in% c("+", "-", "/", "*"),
					(vals[3] %in% nms))) {
				msg = paste(c("Incorrect input for argument fixed. Note that:",
							  "1) The first symbol(s) in fixed must denote a parameter to look up in names(lower).",
							  "3) The second symbol must be a reference to '+', '-', '/', '*'.",
							  "3) The third symbol must be a constant numeric.",
							  "4) Symbols must be separated by spaces (BAD: 'a2/2' GOOD: 'a2 / 2').",
							  " ",
							  sprintf("Problem detected at input: '%s'", fixed$fixedMat[3, i])),
							collapse = "\n")
				stop(cat(msg))
			}
			fixednew[3, i] = ifelse(is.na(vals[3]), 1, as.numeric(vals[3]))      # constant
			fixednew[4, i] = which(nms == vals[1])                      # location (zero index)
			fixednew[5, i] = switch(vals[2], "+" = 0, "-" = 1, "*" = 2, "/" = 3) # operator
		}

	}
	# print(fixednew)
	return(fixednew)

}
