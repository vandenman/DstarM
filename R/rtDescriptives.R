#' Descriptives of reaction time data
#'
#' @param data A reaction time dataset. Must be a dataframe with $rt, $condition
#' and $response.
#'
#' @return An object of class 'D*M', containing raw counts and proportions for
#' condition response pairs, conditions, and responses.
#'
#' @details This function and \code{\link{rtHist}} are helper functions to inspect raw data.
#'
#' @examples
#' tt = seq(0, 5, .01)
#' dat = simData(n = 3e5, pars = rep(.5, 5), tt = tt, pdfND = dbeta(tt, 10, 30))
#' x = rtDescriptives(dat)
#' x
#' print(x, what = 'cr')
#' print(x, what = 'c')
#' print(x, what = 'r')
#' print(x, digits = 20)

#' @export
rtDescriptives = function(data) {
  lenCR = tapply(data$rt, list(data$condition, data$response), length)
  d = dim(lenCR)
  lenC = .rowSums(lenCR, m = d[1], n = d[2])
  lenR = .colSums(lenCR, m = d[1], n = d[2])
  out = list(
    counts = list(
      conditionResponse = lenCR,
      condition = lenC,
      response = lenR),
    props = list(
      conditionResponse = lenCR / lenC,
      condition = lenC / sum(lenC),
      response = lenR / sum(lenR)),
    responses = colnames(lenCR)
  )
  class(out) = 'DstarM'
  return(out)
}





