#' @export
summary.DstarM = function(object, ...) {
  if (names(object)[1] != 'Bestvals') {
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
                  object$fixed$fixedMat[1, ], object$fixed$fixedMat[2, ]))
    } else if (dim(object$fixed$fixedMat)[2L] == 2) {
      cat(sprintf('Parameters %s were fixed to %s objectpectively. ',
                  paste(object$fixed$fixedMat[1, 1], 'and', object$fixed$fixedMat[1, 2]),
                  paste(object$fixed$fixedMat[2, 1], 'and', object$fixed$fixedMat[2, 2])))
    } else {
      cat(sprintf('Parameters %s were fixed to %s objectpectively. ',
                  paste0(paste(object$fixed$fixedMat[1, -dim(object$fixed$fixedMat)[2L]], collapse = ', '),
                         ', and ', object$fixed$fixedMat[1, dim(object$fixed$fixedMat)[2L]]),
                  paste0(paste(object$fixed$fixedMat[2, -dim(object$fixed$fixedMat)[2L]], collapse = ', '),
                         ', and ', object$fixed$fixedMat[2, dim(object$fixed$fixedMat)[2L]])))
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

