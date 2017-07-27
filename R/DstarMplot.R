#' @export

# custom plot function for `D*M` output
plot.DstarM = function(x, what = 'model', ...) {
	dots = list(...)
	def.args = list(bty = 'n', xlab = 'Reaction Time', las = 1, ylab = 'density',
					x = x$tt, type = 'b', lty = 1, pch = 1)
	if (!is.null(x$byPp)) { # output from byParticipant
		idx = sapply(x, is.DstarM)
		dots$y = switch(x$byPp,
						estDstarM = do.call(cbind, lapply(x[idx], `[[`, ifelse(what == 'model', 'modelDist', 'g.hat'))),
						estND = do.call(cbind, lapply(x[idx], `[[`, 'r.hat')),
						estObserved = do.call(cbind, lapply(x[idx], `[[`, 'obsNorm'))
		)
		dots$x = x[[idx[1]]]$tt
		if (x$byPp == 'estND') {
			def.args$xlim = range(unlist(lapply(x[idx], `[[`, 'ttr')))
		}
	} else if (!is.null(x$ttr)) {# output from r.hat()
		def.args$xlim = range(x$ttr)
		dots$y = x$r.hat
	} else if (names(x)[1] == 'obsNorm') { # output from estObserved
		dots$y = x$obsNorm
	} else if (names(x)[1] == 'Bestvals'){ # output from estDstarM()
		if (what == 'model') {
			dots$y = x$modelDist
		} else if (what == 'data') {
			dots$y = x$g.hat
		}
	} else {
		stop('No plot method available for this object.', call. = FALSE)
	}
	ind = unlist(lapply(dots[names(def.args)], is.null))
	dots[names(def.args)[ind]] = def.args[ind]
	do.call(graphics::matplot, dots)
	if (!is.null(colnames(dots$y))) {

		nc <- NCOL(dots$y)
		if (is.null(dots$lty))
			dots$lty <- rep(1, nc)

		if (is.null(dots$col))
			dots$col <- seq_len(nc)
		graphics::legend("topright", colnames(dots$y), col = dots$col, lty = dots$lty, bty = "n")

	}
}



