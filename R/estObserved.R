#' Estimate observed data density
#'
#' @param resDecision output of \code{\link{estDstarM}}.
#' @param resND output of \code{\link{estND}}.
#'
#' @description Estimates the density of the observed data by convoluting the estimated decision distributions
#' with the estimated nondecision distributions. If a traditional analysis was run the argument resND can
#' be omitted.
#'
#' @return a list (S3 object of class 'DstarM') that contains:
#'
#' \item{obsNorm}{A matrix containing normalized densities of each condition response pair.}
#' \item{obs}{A matrix containing unnormalized densities of each condition response pair.}
#' \item{tt}{The time grid used.}
#' \item{fit}{A list containing the values of the objective function for the total model ($total),
#' for the decision model ($Decision) and for the nondecision distribution(s) ($ND).}
#' \item{npar}{The number of parameters used in the decision model.}
#' \item{obsIdx}{A numeric vector containing indices of any not observed condition-response pairs.}
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
#'}

#' @export
estObserved = function(resDecision, resND) {
  if (!is.DstarM(resDecision)) {
    stop('Argument resDecision must be of class "DstarM".', call. = FALSE)
  }
  tt = resDecision$tt
  if (resDecision$DstarM) {
    if (!is.DstarM(resND)) {
      stop('Argument resND must be of class "DstarM".', call. = FALSE)
    }

    nd = resND$r.hat[dim(resND$r.hat)[1L]:1, , drop = FALSE]
    dd = resDecision$modelDist
    splits = resDecision$splits

    # error handling
    if (dim(nd)[2L] != length(unique(splits))) {
      sign = ifelse(dim(nd)[2L] < length(unique(splits)),
                    'Less', 'More')
      stop(sprintf('%s nondecision distributions (%d) than were assumed while estimating the decision model (%d).',
                   sign, as.integer(dim(nd)[2L]), as.integer(length(unique(splits)))), call. = FALSE)
    }
    if (dim(nd)[1L] != dim(dd)[1L]) {
      stop(sprintf('resDecision$modelDist (%d) must have equal rows as resND$r.hat (%d).',
                   dim(dd)[1L], dim(nd)[1L]), call. = FALSE)
    }

    # assign indices for nds to match dds
    # this converts splits from whatever it is to 1, 2, ...
    # and then repeats each element twice since split is per
    # condition and not per condition-response pair.
    idxND = rep(apply(as.matrix(c(splits)), 1, function(x, eq) which(x == eq), eq = unique(c(splits))), each = 2)

    by = diff(tt)[1] # for normalization
    idxC = seq_along(tt) # keep only this part of convolutions
    obs = matrix(NA, length(tt), dim(dd)[2L])
    for (i in 1:dim(dd)[2L]) {
      obs[, i] = customConvolveO(dd[, i], by*nd[, idxND[i]])[idxC]
    }
    # What to do with this???
    # non approximated convolutions after all?
    obs[obs <= 0] = 0 #.Machine$double.xmin

    fitND = sapply(resND$GlobalOptimizer, function(x) x$optim$bestval)
    fitDD = resDecision$GlobalOptimizer$optim$bestval
    fit = list(total = fitND + fitDD,
               Decision = fitDD,
               ND = fitND)
  } else { # traditional analysis
    obs = resDecision$modelDist
    fit = list(total = resDecision$GlobalOptimizer$optim$bestval)
  }
  cor = apply(obs, 2, simpson, x = tt)
  obsNorm = obs %*% (diag(dim(obs)[2L]) / cor)
  npar = length(resDecision$Bestvals) - ifelse(!is.null(resDecision$fixed$fixedMat), dim(resDecision$fixed$fixedMat)[2L], 0)

  # observe any not observed conditions, makes later plotting easier
  obsIdx = which(colSums(resDecision$g.hat) == 0)
  res = list(obsNorm = obsNorm, obs = obs, tt = tt, fit = fit, npar = npar,
             obsIdx = obsIdx)
  class(res) = 'DstarM'
  return(res)
}


# TEST
# root = 'C:/Users/donvd/_Laptop/ResMas/Internship/DstarM/massSimulation/Simulation/'
# select = read.table(paste0(root, 'BestSetsExp4a.txt'))
#
# pp = 1
# idx = select[1, ]
# load(paste0(root, 'ResultsExp4a/', 'pp_', pp, '_rep_', idx, '.Rdata'))
# resDecision = res
# load(paste0(root, 'ResultsExp5/', 'ND', pp, '.Rdata'))
#
# obs = estObserved(resDecision, resND)
# plotObserved(resObserved = obs, data = )
# matplot(obs, type = 'l', lty = 1)
