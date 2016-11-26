#' Calculate model fit
#'
#' @param resObserved output from \code{\link{estObserved}}.
#' @param data Either a dataframe containing data used for estimating the models or a matrix containing custom densities used for estimating.
#' @param ncr The number of observations per condition-response pair. This is used to weight the objective function. Should only be supplied when data is a matrix containing custom densities.
#' @param DstarM Logical. Should the DstarM fit measure be calculated or the traditional fit measure?
#'
#' @details This function allows a user to manually calculate a chi-square goodness of fit measure for a model.
#' This is usefull to comparing a traditional analysis and a D*M analysis. For completion, this function can also calculate a
#' D*M fit measure. We do not recommend usage of the D*M measure. While the chi-square fit measure is
#' identical to the value of the optimizer when fitting, the DstarM fit measure is not equal to that of a DstarM analysis.
#' This is because this function calculates the DstarM fit measure on the complete distribution, not on the
#' model distributions, as is done during the optimization.
#'
#' @examples
#' tt = seq(0, 5, .1)
#'pars = c(.8, 2, .5, .5, .5, # condition 1
#'         .8, 3, .5, .5, .5,  # condition 2
#'         .8, 4, .5, .5, .5)  # condition 3
#'pdfND = dbeta(tt, 10, 30)
#'# simulate data
#'data = simData(n = 3e3, pars = pars, tt = tt, pdfND = pdfND)
#'# define restriction matrix
#'restr = matrix(1:5, 5, 3)
#'restr[2, 2:3] = 6:7 # allow drift rates to differ
#'# fix variance parameters
#'fixed = matrix(c('sz1', .5, 'sv1', .5), 2, 2)
#' \dontrun{
#'# Run D*M analysis
#'res = estDstarM(data = data, tt = tt, restr = restr, fixed = fixed)
#'coef(res)
#'summary(res)
#'}
#'
#' @export
chisqFit = function(resObserved, data, ncr = NULL, DstarM = FALSE) {
  tt = resObserved$tt
  m = resObserved$obs
  by = unique(zapsmall(diff(tt)))

  if (is.data.frame(data)) { # assume raw data
    ncondition = max(c(1, length(unique(data$condition)))) # get number of conditions
    if (ncondition == 1 & is.null(data$condition)) {
      data$condition = 1 # necessary for split(data, condition & response)
    }
    if (!is.null(resObserved$ncondition) && ncondition != resObserved$ncondition) {
      stop(sprintf("Number of conditions in resObserved (%d) does not match number of conditions in the data (%d)",
                   as.integer(resObserved$ncondition), as.integer(ncondition)))
    }

    # helper matrix
    mm = matrix(0, ncondition * 2, ncondition)
    mm[1:dim(mm)[1L] + dim(mm)[1L] * rep(1:dim(mm)[2L] - 1, each = 2)] = 1

    ncondition = resObserved$ncondition
    rt = split(data$rt, list(data$response, data$condition))
    ql = lengths(rt)
    g = getGhat(rt = rt, tt = tt, ncondition = ncondition, mm = mm, by = by)

  } else if (!is.matrix(data)) {# else assume custom data densities.
    stop(sprintf("Argument data should be either a dataframe with observed responses or a matrix with custom densities. The supplied object has mode %s", mode(data)))
  } else {
    if (is.null(ncr)) stop("The number of observations per condition-response pairs (ncr) is missing.")
    g = data
    ql = ncr # TODO fix this naming and call ql everywhere ncr.
  }
  if (DstarM) {
    tmp = 1:(ncol(obs)-1)
    ii = rep(tmp, times = rev(tmp))
    jj = unlist(lapply(tmp, function(x, m) (x+1):m, m = ncol(obs)))
    out = numeric(length(ii))
    for (l in 1:length(ii)) {
        a = customConvolveO(g[, ii[l]], by * rev(m[, jj[l]]))[seq_along(tt)]
        b = customConvolveO(g[, jj[l]], by * rev(m[, ii[l]]))[seq_along(tt)]
        out[i] = chisq(tt = tt, a = a, b = b) * 100 * (ql[ii[l]] + ql[jj[l]]) / sum(ql)
    }

  } else {
    out = numeric(ncol(obs))
    for (i in 1:ncol(obs)) {
      out[i] = chisq(tt = tt, a = m[, i], b = g[, i]) * 100 * ql[i] / sum(ql)
    }
  }
  return(list(sum = sum(out), chisq = out))
}

# todo: allow for input of modeldist?


# obsDstarM$obsNorm
# apply(obsTraditional$obsNorm, 2, DstarM:::simpson, x = obsTraditional$tt)
#
#
# chisqFit(obsTraditional, data = df)
# chisqFit(obsTraditional, data = df, DstarM = TRUE)$sum
# obsTraditional$fit

# chisqFit(obsDstarM, data = df, DstarM = FALSE)
# chisqFit(obsDstarM, data = df, DstarM = TRUE)$sum
# chisqFit(resDstarM$modelDist, data = df, DstarM = TRUE)$sum
# obsDstarM$fit
