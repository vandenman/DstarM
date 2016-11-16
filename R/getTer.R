#' Calculate Mean of the nondecision distribution.
#'
#' @param res An object of class D*M.
#' @param dat The data object used to create \code{res}.
#'
#' @return A vector containing estimates for the mean of the nondecision densities.

#' @details The object \code{res} can either be output from \code{estDstarM} or output from \code{estND}.
#' If the former is supplied it is also necessary to supply the data used for the estimation.
#' The mean will then be estimated by substracting the mean of the model densities from the mean of the data density.
#' If the latter is supplied than this is not required; the mean will be calculated by
#' integrating the nondecision distribution.

# calculate Ter according to splits used in analyses; returns scalar | groups
#' @export
getTer = function(res, dat) {
  stopifnot(class(res) == 'D*M')
  if (names(res)[1] == 'r.hat') {
    return(apply(res$r.hat, 2, nth.momentS, x = res$tt))
  }
  if (dim(dat)[1L] != res$n) {
    warning(sprintf('Number of observations used in analysis (%g) does not match number of observations in data provided (%g).',
                    res$n, dim(dat)[1L]), call. = FALSE, immediate. = TRUE)
  }

  if (is.null(res$splits) & !is.null(res$split)) res$splits = res$split # backward compatability

  ncondition = res$ncondition
  splits = res$splits
  m = res$modelDist

  group = groups(ncondition, splits)
  mm2 = matrix(0, 2*ncondition, dim(group)[2L])
  for (i in 1:dim(group)[2L]) {
    mm2[group[, i], i] = 1
  }

  m = m %*% mm2
  m = m %*% (diag(dim(m)[2L]) / (colSums(mm2) / 2))
  uniq = unique(dat$condition)
  group = groups(ncondition, splits, TRUE)
  for (i in 1:length(group)) {
    group[i] = uniq[i]
  }
  muDat = rep.int(0, dim(group)[2L])
  for (i in dim(group)[2L]) {
    muDat[i] = mean(dat$rt[dat$condition %in% group[, i]])
  }

  muMod = apply(m, 2, nth.momentS, x = res$tt)
  return(muDat - muMod)
}

