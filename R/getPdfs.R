#' (Re)Calculate model densities with given parameters and time grid
#'
#' @param resDecision output of \code{\link{estDstarM}}.
#' @param tt Time grid to calculate the model densities on.
#' @param pars Model parameters, can be a matrix where every column is a set of parameters.
#' @param fun.density density function to calculate pdfs from.
#' @param arg.density Additional arguments for fun.density
#'
#' @description This function is a convenience function for calculating model pdfs for
#' multiple sets of parameters at a specified timegrid. If \code{resDecision} is supplied,
#' the density function and any additional arguments for the density function will be
#' extracted from that object. If \code{pars} is missing these will also be extracted from
#' this object. This function is intended to recalculate model densities at a new timegrid.
#'
#' @return A matrix containing model pdfs.

#' @export
getPdfs = function(resDecision, tt, pars, DstarM = TRUE, fun.density = Voss.density, args.density = list()) {

  if (!missing(resDecision)) {
    if (!is.DstarM(resDecision) || names(resDecision)[1L] != "Bestvals") {
      stop("Argument res must be output of estDstarM()")
    } else {
      fun.density = resDecision$fun.density
      args.density = resDecision$args.density
      if (missing(pars)) {
        pars = resDecision$Bestvals[c(resDecision$restr.mat)]
        dim(pars) = dim(resDecision$restr.mat)
        if (DstarM != resDecision$DstarM) {
          warning("Argument DstarM does not match the amount ")
        }
      }
    }
  }


  ncondition = NCOL(pars)
  mm = matrix(0, ncondition * 2, ncondition)
  mm[1:dim(mm)[1L] + dim(mm)[1L] * rep(1:dim(mm)[2L] - 1L, each = 2)] = 1

  pars.list = unlist(apply(pars, 2, list), recursive = FALSE)
  m = getPdf(pars.list = pars.list, tt = tt, DstarM = DstarM, mm = mm,
                      oscPdf = FALSE, fun.density = fun.density,
                      args.density = list())

  if (!is.null(colnames(pars))) {
    colnames(m) = colnames(pars)
  }

  return(m)

}
