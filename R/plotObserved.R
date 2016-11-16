#' Plot histogram of data with overlayed model predicted density.
#'
#' @param resObserved output of \code{\link{estObserved}}.
#' @param data The dataset used to estimate the model.
#' @param what What to plot. Can be 'cr' for 'condition-response pairs,
#' 'c' for condition, and 'r' for response.
#' @param layout An optional layout matrix.
#' @param main an optional vector containing names for each plot.
#' @param linesArgs A list containing named arguments to be passed to \code{\link{lines}}.
#' @param ggplot Logical, should ggplot2 be used instead of base R graphics? If set to TRUE,
#' some arguments from \code{linesArgs} and \code{...} will be ignored (but can be added
#' to plots manually).
#' @param prob Should a qqplot of observed vs model implied quantiles be plotted?
#' If NULL (the default) then a histogram overlayed with model implied densities will be plotted.
#' Otherwise, this argument should be a vector of probabilities to be passed to \code{\link{estQdf}}.
#' @param probType A numeric value defining several plotting options. 0 does nothing, 1
#' removes the 0\% quantile, 2 removes the 100\% quantile and 3 removes both the 0\% and 100\% quantile.
#' @param ... Further arguments to be passed to hist.
#'
#' @description Plots histograms for each condition-response pair/ condition/ response
#' with overlayed estimated densities.
#'
#' @details Keep in mind when using \code{what = 'c'} or \code{what = 'r'} pdfs are simply
#' averaged, not weighted to the number of observed responses.
#'
#' @return if ggplot is FALSE \code{invisible()}, otherwise a list
#'
#' @examples
#'# simulate data with three stimuli of different difficulty.
#'# this implies different drift rates across conditions.
#'# define a time grid. A more reasonable stepsize is .01; this is just for speed.
#'tt = seq(0, 5, .1)
#'pars = c(.8, 2, .5, .5, .5, # condition 1
#'         .8, 3, .5, .5, .5, # condition 2
#'         .8, 4, .5, .5, .5) # condition 3
#'pdfND = dbeta(tt, 10, 30)
#'# simulate data
#'lst = simData(n = 3e5, pars = pars, tt = tt, pdfND = pdfND, return.pdf = TRUE)
#'dat = lst$dat
#'# define restriction matrix
#'restr = matrix(1:5, 5, 3)
#'restr[2, 2:3] = 6:7 # allow drift rates to differ
#'# fix variance parameters
#'fixed = matrix(c('sz1', .5, 'sv1', .5), 2, 2)
#'\dontrun{
#'# Run D*M analysis
#'resD = estDstarM(dat = dat, tt = tt, restr = restr, fixed = fixed)
#'# Estimate nondecision density
#'resND = estND(resD)
#'# Estimate observed density
#'resObs = estObserved(resD, resND)
#'# plot histograms with overlayed
#'# densities per condition-response pair
#'plotObserved(resObserved = resObs, data = dat,
#'             xlim = c(0, 1))
#'# plot estimated and true densities
#'plot(resObs, col = rep(1:3, each = 2), xlim = 0:1)
#'matlines(tt, lst$pdfNormalized, col = rep(1:3, each = 2), lty = 2)
#'# other uses of plotObserved
#'plotObserved(resObserved = resObs, data = dat, what = 'cr', xlim = c(0, 1))
#'plotObserved(resObserved = resObs, data = dat, what = 'c', xlim = c(0, 1))
#'plotObserved(resObserved = resObs, data = dat, what = 'r', xlim = c(0, 1))
#'}
#' @export
plotObserved = function(resObserved, data, what = 'cr', layout = NULL,
                        main = NULL, linesArgs = list(), ggplot = FALSE,
                        prob = NULL, probType = 3, ...) {
  if (what == 'cr') { # by condition-response pair
    rtList = split(data$rt, factor(paste(data$condition, data$response)))
    dd = resObserved$obsNorm
  } else {
    ncondition = dim(resObserved$obsNorm)[2L] / 2L
    if (what == 'c') { # by condition
      rtList = split(data$rt, factor(data$condition))
      # helper matrix
      mm = matrix(0, ncondition * 2, ncondition)
      mm[1:dim(mm)[1L] + dim(mm)[1L] * rep(1:dim(mm)[2L] - 1, each = 2)] = .5
    } else { # by response
      rtList = split(data$rt, factor(data$response))
      val = 1 / ncondition
      mm = matrix(c(val, 0, 0, val), nrow = dim(resObserved$obsNorm)[2L], ncol = 2, byrow = TRUE)
    }
    dd = resObserved$obsNorm %*% mm
  }
  tt = resObserved$tt
  # define layout
  if (is.null(layout)) {
    if (what == 'cr') { # responses within columns
      layout = matrix(1:length(rtList), nrow = 2)
    } else if (what == 'c') { #
      if (length(rtList) / 2 != floor(length(rtList) / 2) &
          floor(sqrt(length(rtList))) > 1) {
        p = c(1, 1:length(rtList))
      } else {
        p = 1:length(rtList)
      }
      layout = matrix(p, nrow = floor(sqrt(length(rtList))))
    } else { # 2 columns
      layout = matrix(1:length(rtList), ncol = 2)
    }
  }  else { # Error Handling
    if ((dim(resObserved$obsNorm)[2L] %% length(unique(layout))) != 0) {
      msg = switch (what,
                    cr = 'condition-response pairs',
                    c = 'conditions',
                    r = 'responses'
      )
      warning(sprintf('Number of plots in layout (%d) is not a multiple of the number of %s (%d).',
                   length(unique(layout)), msg, length(rtList)), call. = FALSE, immediate. = TRUE)
    }
  }
  skips = resObserved$obsIdx
  # define Titles
  if (is.null(main)) {
    if (what == 'cr') {
      main = apply(expand.grid(unique(data$response), unique(data$condition)), 1,
                   function(x) paste0('Condition: ', x[2], '\nResponse: ', x[1]))
    } else {
      main = paste(ifelse(what == 'c', 'Condition:', 'Response:'), names(rtList))
    }
    main = paste0(main, ' (N = ', lengths(rtList), ')')
    main2 = rep(paste('Unobserved', switch(what, cr = 'condition-response pair', c = 'condition', r = 'response')),
                length(skips))
  } else if (is.list(main)) {
    main2 = main[[2]]
    main = main[[1]]
  }
  # change dd and rtList into estimated and observed quantiles
  if (qq <- !is.null(prob)) {
    if (!is.vector(prob, mode = 'numeric')) {
      stop('Argument prob must either be NULL or a numeric vector.')
    }
    rtList = lapply(rtList, stats::quantile, probs = prob)
    dd = estQdf(p = prob, x = tt, cdf = estCdf(dd))
    if (probType == 0) {
      probIdx = 1:dim(dd)[1L]
    } else if (probType == 1) {
      probIdx = -1
    } else if (probType == 2) {
      probIdx = -dim(dd)[1L]
    } else {
      probIdx = -c(1, dim(dd)[1L])
    }
    rtList = lapply(rtList, `[`, probIdx)
    dd = dd[probIdx, ]
    prob = prob[probIdx]
  }
  # histogram defaults
  histArgs = list(...)
  histDefArgs = list(xlab = ifelse(qq, 'Model implied Quantiles', 'Reaction Time'),
                     ylab = ifelse(qq,  'Observed Quantiles', 'Density'),
                     las = 1, bty = 'n', freq = FALSE,
                     breaks = c(0, tt + (tt[2] - tt[1]) / 2))
  idx = unlist(lapply(histArgs[names(histDefArgs)], is.null), use.names = FALSE)
  histArgs[names(histDefArgs)[idx]] = histDefArgs[idx]

  # lines defaults
  linesDefArgs = list(type = 'b', lty = 2, pch = 1, col = 'black', lwd = 1, x = tt)
  idx = unlist(lapply(linesArgs[names(linesDefArgs)], is.null), use.names = FALSE)
  linesArgs[names(linesDefArgs)[idx]] = linesDefArgs[idx]

  # actual plotting
  rtIdx = 1 # a second index for histograms
  if (!ggplot) {
    layout(layout)
    for (i in seq_len(dim(dd)[2L])) {
      linesArgs$y = dd[, i]
      if (i %in% skips) {
        linesArgs[c('xlim', 'ylim', 'ylab', 'xlab', 'bty', 'las')] =
          c(histArgs[c('xlim', 'ylim', 'ylab', 'xlab')], list('n', 1))
        linesArgs$main = main2[which(i %in% skips)]
        do.call(graphics::plot, linesArgs)
        linesArgs[c('xlim', 'ylim', 'main', 'ylab', 'xlab', 'bty', 'las')] = NULL
      } else {
        histArgs$main = main[rtIdx]
        histArgs$x = rtList[[rtIdx]]
        if (qq) {
          histArgs$y = linesArgs$y
          histArgs[c('breaks', 'freq')] = NULL
          do.call(graphics::plot, histArgs)
          graphics::abline(a = 0, b = 1)
        } else {
          do.call(graphics::hist, histArgs)
          do.call(graphics::lines, linesArgs)
        }
        rtIdx = rtIdx + 1
      }
    }
    layout(1)
    return(invisible())
  } else {
    plotList = vector('list', dim(dd)[2L])
    if (qq) {
      linesDat = data.frame(x = prob)
    } else {
      linesDat = data.frame(x = resObserved$tt)
    }
    for (i in seq_len(dim(dd)[2L])) {
      linesDat$y = dd[, i]
      if (i %in% skips) {
        g = suppressWarnings(
          ggplot2::ggplot(data = linesDat, ggplot2::aes_string(x = 'x', y = 'y')) +
            ggplot2::geom_line(linetype = linesArgs$lty, colour = linesArgs$col,
                               size = linesArgs$lwd) +
            ggplot2::geom_point(shape = linesArgs$pch, colour = linesArgs$col,
                                size = linesArgs$lwd) +
            ggplot2::scale_x_continuous(name = histArgs$xlab, limits = histArgs$xlim) +
            ggplot2::scale_y_continuous(name = histArgs$ylab) +
            ggplot2::ggtitle(main2[which(i %in% skips)])
        )
      } else {
        histDat = data.frame(rtime = rtList[[rtIdx]])
        if (qq) {
          histDat$y = linesDat$y
          g = ggplot2::ggplot(data = histDat, ggplot2::aes_string(x = 'rtime', y = 'y')) +
            ggplot2::geom_point(ggplot2::aes_string(x = 'rtime', y = 'y'),
                                shape = linesArgs$pch, colour = linesArgs$col,
                                size = linesArgs$lwd) +
            ggplot2::geom_abline(intercept = 0, slope = 1)
        } else {
          g = suppressWarnings(
            ggplot2::ggplot(data = histDat, ggplot2::aes_string(x = 'rtime')) +
              ggplot2::geom_histogram(ggplot2::aes_string(y = '..density..'), breaks = histArgs$breaks) +
              ggplot2::scale_x_continuous(name = histArgs$xlab,
                                          limits = histArgs$xlim) +
              ggplot2::scale_y_continuous(name = histArgs$ylab) +
              ggplot2::geom_line(data = linesDat, ggplot2::aes_string(x = 'x', y = 'y'),
                                 linetype = linesArgs$lty,
                                 colour = linesArgs$col, size = linesArgs$lwd) +
              ggplot2::geom_point(data = linesDat, ggplot2::aes_string(x = 'x', y = 'y'),
                                  shape = linesArgs$pch,
                                  colour = linesArgs$col, size = linesArgs$lwd) +
              ggplot2::ggtitle(main[rtIdx])
          )
        }
        rtIdx = rtIdx + 1
      }
      plotList[[i]] = g
    }
    return(plotList)
  }
}

