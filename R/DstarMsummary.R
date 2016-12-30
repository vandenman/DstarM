#' @export
summary.DstarM = function(object, ...) {
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

