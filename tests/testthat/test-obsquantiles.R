context("test-obsquantiles.R")

test_that("multiplication works", {
  set.seed(123)
  tt <- seq(0, 5, .01)
  pars <- c(.8, 2, .5, .5, .5, # condition 1
           .8, 3, .5, .5, .5)  # condition 3
  pdfND <- dbeta(tt, 10, 30)
  data <- simData(n = 100, pars = pars, tt = tt, pdfND = pdfND)
  probs <- seq(0, 1, .1)
  q <- obsQuantiles(data, probs = probs)
  expect_equal2(
    object = q,
    expected = structure(
      c(0.2, 0.208, 0.228, 0.24, 0.254, 0.31, 0.31, 0.316,
        0.328, 0.346, 0.37, 0.19, 0.27, 0.3, 0.3, 0.32, 0.34, 0.37, 0.4,
        0.44, 0.49, 0.76, 0.18, 0.222, 0.268, 0.312, 0.326, 0.36, 0.39,
        0.39, 0.408, 0.426, 0.44, 0.22, 0.27, 0.29, 0.33, 0.34, 0.35,
        0.36, 0.397, 0.428, 0.459, 0.56), .Dim = c(11L, 4L),
      .Dimnames = list(c("0%", "10%", "20%", "30%", "40%", "50%", "60%",
                         "70%","80%", "90%", "100%"),
                       c("lower.1", "upper.1", "lower.2", "upper.2")))

  )
})
