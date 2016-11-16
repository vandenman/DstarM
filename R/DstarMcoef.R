#' @export
coef.DstarM = function(object, ...) {
  if (is.list(object)) {
    if (names(object)[1] == 'Bestvals') {
      return(object$Bestvals)
    } else {
      idx = sapply(object, is.DstarM)
      return(do.call(rbind, lapply(object[idx], coef.DstarM)))
    }
  }
}

