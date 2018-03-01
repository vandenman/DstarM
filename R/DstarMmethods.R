# custom print function for `D*M` output
print.DstarM = function(x, na.print = '-', ...) {
	# exists for legacy purposes
	other = FALSE
	if (is.list(x)) { # estDstarM, estND, estObserved
		nm = names(x)[1]
		dots = list(...)
		if (nm == 'Bestvals') { # x from estDstarM(pars = ...)
			out = x$Bestvals[c(x$restr.mat)]
			out[duplicated(c(x$restr.mat))] = NA
			dim(out) = dim(x$restr.mat)
			colnames(out) = paste('Condition', seq_len(ncol(out)))
			rownames(out) = sapply(strsplit(paste0(names(x$Bestvals)),
											"(?<=[a-zA-Z])(?=[0-9])", perl = TRUE), `[[`, 1)[1:nrow(out)]
			print(out, na.print = '-')
		} else if (nm == 'objVals') { # x from estDstarM(pars = ...)
			if (isTRUE(dots$sum)) {
				cat('Sum of objective function values:\n')
				print(sum(x$objVals))
			} else {
				cat('Objective function values:\n')
				print(x$objVals)
			}
		} else if (nm == 'r.hat') {
			print(x$descriptives)
		} else if (nm == 'counts') { # x from rtDescriptives
			what = dots$what
			if (is.null(what)) {
				what = 'cr'
			} else {
				if (!(what %in% c('cr', 'c', 'r'))) {
					stop(sprintf("Argument what ('%s') must be 'cr', 'c', or 'r'.", what))
				}
				dots$what = NULL
			}
			idx = switch(what, cr = 1, c = 2, r = 3)
			msg0 = switch(what, cr = 'Condition Response Pairs', c = 'Conditions', r = 'Responses')
			dots$x = cbind(x$props[[idx]],
						   x$counts[[idx]])
			dimnames(dots$x) = list(1:dim(dots$x)[1L],
									switch (what,
											cr = rep(x$responses, 2),
											c = c('Proportion', 'Counts'),
											r = x$responses))
			xLen = nchar(utils::capture.output(print(dots$x[, 1, drop = FALSE], digits = NULL)))[1]
			cat(c(' ', msg0, '\n'))
			if (idx == 1) {
				cat(c('  Prop', rep('', max(c(0, xLen - 7))),
					  rep('', xLen * (dim(dots$x)[2L] / 2 - 1)), 'Counts\n'))
			}
			do.call(print, dots)
		} else {
			other = TRUE
		}
	} else if (is.matrix(x)) { # x from calcIC
		out = matrix(nrow = dim(x)[1L], ncol = dim(x)[2L] + 1)
		out[, 1] = format(x[, 1], digits = 7)
		out[, c(2 ,4)] = x[, c(2, 4)]
		out[, 3] = format(x[, 3], digits = 4)
		out[, 5] = format(x[, 5], digits = 4)
		out[, 6] = ifelse(x[, 5] < .001, '***', ifelse(x[, 5] < .01, '**', ifelse(x[, 5] < .05, '*', 'n.s.')))
		rownames(out) = rownames(x)
		colnames(out) = c(colnames(x), 'signif')
		out[is.na(x)] = '' # as which(is.na(x))
		print(out, na.print = '', quote = FALSE)
		writeLines("\nSignif: |p < .001: *** |p < 0.01: ** |p < .05: * |p > .05: n.s.")
	} else { # if all else fails
		other = TRUE
	}
	if (other) {
		print.default(x)
	}
}

print.DstarM.fitD <- function(x, na.print = '-', ...) {

	dots <-  list(...)
	out <-  x$Bestvals[c(x$restr.mat)]
	out[duplicated(c(x$restr.mat))] = NA
	dim(out) = dim(x$restr.mat)
	colnames(out) = paste('Condition', seq_len(ncol(out)))
	rmns <- gsub("[0-9]", "", names(x$Bestvals))
	rownames(out) = rmns
	print(out, na.print = '-')
}

print.DstarM.fitND <- function(x, na.print = '-', ...) {
	print(x$descriptives, na.print = na.print, ...)
}

print.Dstarm.fitObs <- function(x, na.print = '-', ...) {
	print.default(x, na.print = na.print, ...)
}

print.DstarM.Descriptives <- function(x, na.print = '-', ...) {

	# nm = names(x)[1]
	dots = list(...)
	what = dots$what
	if (is.null(what)) {
		what = 'cr'
	} else {
		if (!(what %in% c('cr', 'c', 'r'))) {
			stop(sprintf("Argument what ('%s') must be 'cr', 'c', or 'r'.", what))
		}
		dots$what = NULL
	}
	idx = switch(what, cr = 1, c = 2, r = 3)
	msg0 = switch(what, cr = 'Condition Response Pairs', c = 'Conditions', r = 'Responses')
	dots$x = cbind(x$table$props[[idx]],
				   x$table$counts[[idx]])
	dimnames(dots$x) = list(1:dim(dots$x)[1L],
							switch (what,
									cr = rep(x$table$responses, 2),
									c = c('Proportion', 'Counts'),
									r = x$table$responses))
	xLen = nchar(utils::capture.output(print(dots$x[, 1, drop = FALSE], digits = NULL)))[1]
	cat(c(' ', msg0, '\n'))
	if (idx == 1) {
		cat(c('  Prop', rep('', max(c(0, xLen - 7))),
			  rep('', xLen * (dim(dots$x)[2L] / 2 - 1)), 'Counts\n'))
	}
	do.call(print, dots)
	print(x$graph)
}

summary.DstarM.fitD = function(object, ...) {
  if (names(object)[1L] != 'Bestvals') {
    stop('No summary method available for this object.')
  }
  if (!is.null(object$note)) {
    cat(object$note)
  }
  cat('\n')
  cat(sprintf('%s analysis done on %g observations in %s conditions.',
              ifelse(object$DstarM, 'D*M', 'Chi-Square'),
              sum(object$n), object$ncondition))
  cat('\n')
  cat('Coefficients: \n \n')
  print(object)
  cat('\n')
  if (!length(object$fixed)) {
    cat('No parameters were fixed. ')
  } else {
    if (dim(object$fixed$fixedMat)[2L] == 1) {
      cat(sprintf('Parameter %s was fixed to %s. ',
                  object$fixed$fixedMat[1L, ], object$fixed$fixedMat[2, ]))
    } else if (dim(object$fixed$fixedMat)[2L] == 2L) {
      cat(sprintf('Parameters %s were fixed to %s respectively. ',
                  paste(object$fixed$fixedMat[1L, 1L], 'and', object$fixed$fixedMat[1L, 2L]),
                  paste(object$fixed$fixedMat[2L, 1L], 'and', object$fixed$fixedMat[2L, 2L])))
    } else {
      cat(sprintf('Parameters %s were fixed to %s respectively. ',
                  paste0(paste(object$fixed$fixedMat[1L, -dim(object$fixed$fixedMat)[2L]], collapse = ', '),
                         ', and ', object$fixed$fixedMat[1L, dim(object$fixed$fixedMat)[2L]]),
                  paste0(paste(object$fixed$fixedMat[2L, -dim(object$fixed$fixedMat)[2L]], collapse = ', '),
                         ', and ', object$fixed$fixedMat[2L, dim(object$fixed$fixedMat)[2L]])))
    }
  }
  cat(sprintf('The value of the objective function was %g after %g iterations.',
              object$GlobalOptimizer$optim$bestval, object$Debug$niter))
  if (object$Debug$niter == object$Debug$itermax) {
    cat('\n')
    cat('WARNING: Maximum iterations was reached. Be aware of potential convergence issues.')
    cat('\n')
  }
}

