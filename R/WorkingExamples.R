if (FALSE) {
  library(DstarM)
  tt = seq(0, 5, .001)

  pars = c(.8, 2, .2, .5, .5, .5, .2, # condition 1
           .8, 3, .2, .5, .5, .5, .2,  # condition 2
           .8, 4, .2, .5, .5, .5, .2)  # condition 3

  # simulate data
  data = simData(n = 3e2, pars = pars, tt = tt, pdfND = NULL, normalizePdfs = FALSE, return.pdf = TRUE,
                 args.density = list(DstarM = FALSE), npars = 7)

  # define restriction matrix
  restr = matrix(1:5, 5, 3)
  restr[2, 2:3] = 6:7 # allow drift rates to differ

  # fix variance parameters
  # fixed = matrix(c('a1', .8, 'z1', .5, 'sz1', .5, 'sv1', .5), 2)
  fixed = matrix(c('sz1', .5, 'sv1', .5), 2)

  # Run traditional analysis
  tt.est = seq(0, 5, .1)
  restr.ter <- rbind(restr,rep(8,3),rep(9,3))

  idx <- seq(1, length(tt), length.out = length(tt.est))
  res1b <- estDstarM(data = data$dat, tt = tt.est, restr = restr.ter, fixed = fixed, DstarM = FALSE,
                   mg = data$pdfUnnormalized[idx, ], useRcpp = FALSE, pars = pars)
  res1b$objVals
  # res <- estDstarM(data = data$dat, tt = tt.est, restr = restr.ter, fixed = fixed, DstarM = FALSE,
  #                  mg = data$pdfUnnormalized[idx, ], useRcpp = FALSE, Optim = list(parallelType = 1))
  # summary(res)

  idx <- seq(1, length(tt), length.out = length(tt.est))
  res2b <- estDstarM(data = data$dat, tt = tt.est, restr = restr.ter, fixed = fixed, DstarM = FALSE,
            mg = data$pdfUnnormalized[idx, ], useRcpp = TRUE, pars = pars)
  res2b$objVals
  res2 <- estDstarM(data = data$dat, tt = tt.est, restr = restr.ter, fixed = fixed, DstarM = FALSE,
                   mg = data$pdfUnnormalized[idx, ], useRcpp = TRUE, Optim = list(parallelType = 1))
  summary(res2)
  beepr::beep(5)
}

if (FALSE) {
  tt <- seq(0, 5, .01)

  pars <- c(.8, 2, .5, .5, .5,  # condition 1
           .8, 3, .5, .5, .5,  # condition 2
           .8, 4, .5, .5, .5)  # condition 3
  parsTest <- c(.8, 2, 0, .5, .5, .5, .001,
                .8, 3, 0, .5, .5, .5, .001,
                .8, 4, 0, .5, .5, .5, .001)

  # take delta-peak instead
  pdfND = 0*tt
  pdfND[1] = 1/diff(tt)[1]

  # data = simData(n = 3e6, pars = pars, tt = tt, pdfND = pdfND, normalizePdfs = FALSE, return.pdf = TRUE)
  data = simData(n = 3e6, pars = parsTest, tt = tt, pdfND = NULL, normalizePdfs = FALSE, return.pdf = TRUE,
                 args.density = list(DstarM = FALSE), npars = 7)

  # define restriction matrix
  restr = matrix(1:5, 5, 3)
  restr[2, 2:3] = 6:7 # allow drift rates to differ

  # fix variance parameters
  # fixed = matrix(c('a1', .8, 'z1', .5, 'sz1', .5, 'sv1', .5), 2)
  fixed = matrix(c('sz1', .5, 'sv1', .5), 2)

  # Run traditional analysis
  tt.est = seq(0, 5, .1)
  restr.ter <- rbind(restr,rep(8,3),rep(9,3))

  idx <- seq(1, length(tt), length.out = length(tt.est))


  res3b <- estDstarM(data = data$dat, tt = tt.est, restr = restr.ter, fixed = fixed, DstarM = FALSE,
                    useRcpp = FALSE, pars = parsTest)
  res3b$objVals
  res3c <- estDstarM(data = data$dat, tt = tt.est, restr = restr.ter, fixed = fixed, DstarM = FALSE,
                     mg = data$pdfUnnormalized[idx, ], useRcpp = FALSE, pars = parsTest)
  res3c$objVals
  res3d <- estDstarM(data = data$dat, tt = tt.est, restr = restr.ter, fixed = fixed, DstarM = FALSE,
                     useRcpp = FALSE, pars = parsTest, densityMethod = Density)
  res3d$objVals

  # works!
  res3true <- estDstarM(data = data$dat, tt = tt.est, restr = restr.ter, fixed = fixed, DstarM = FALSE,
                     mg = data$pdfUnnormalized[idx, ], useRcpp = FALSE, Optim = list(parallelType = 1))
  summary(res3true)
  res3data <- estDstarM(data = data$dat, tt = tt.est, restr = restr.ter, fixed = fixed, DstarM = FALSE,
                     useRcpp = FALSE, Optim = list(parallelType = 1), densityMethod = Density)
  summary(res3data)
  round(res3data$Bestvals, 2)

  res3dataDens <- estDstarM(data = data$dat, tt = tt.est, restr = restr.ter, fixed = fixed, DstarM = FALSE,
                        useRcpp = FALSE, Optim = list(parallelType = 1), h = 7)
  summary(res3dataDens)
  round(res3dataDens$Bestvals, 2)
beepr::beep(5)

  idx <- seq(1, length(tt), length.out = length(tt.est))
  res4b <- estDstarM(data = data$dat, tt = tt.est, restr = restr.ter, fixed = fixed, DstarM = FALSE,
                     useRcpp = TRUE, pars = parsTest)
  res4b$objVals
  res4c <- estDstarM(data = data$dat, tt = tt.est, restr = restr.ter, fixed = fixed, DstarM = FALSE,
                     mg = data$pdfUnnormalized[idx, ], useRcpp = TRUE, pars = parsTest)
  res4c$objVals

  # res4 <- estDstarM(data = data$dat, tt = tt.est, restr = restr.ter, fixed = fixed, DstarM = FALSE,
  #                   useRcpp = TRUE, Optim = list(parallelType = 1))
  # summary(res4)

}


# library(DstarM)
# # pars0 <- pars
# pars0 <-  c(.8, 2, .2, .5, .5, .5, .2, # condition 1
#             .8, 3, .2, .5, .5, .5, .2,  # condition 2
#             .8, 4, .2, .5, .5, .5, .2)  # condition 3
#
# pars <- pars0
#
# pars <- matrix(pars, ncol = 3)
# pars[3,] <- 0.5
# pars[7,] <- 0.2
#
# tt = seq(0, 5, .001)
# tt.est = seq(0, 5, .1)
#
# ncondition <- NCOL(pars)
# mm <- matrix(0, ncondition * 2, ncondition)
# mm[1:dim(mm)[1L] + dim(mm)[1L] * rep(1:dim(mm)[2L] - 1L, each = 2)] <- 1
#
#
# pars.list <- unlist(apply(pars, 2, list), recursive = FALSE)
# m1 <- DstarM:::getPdf(pars.list = pars.list, tt = tt.est, DstarM = FALSE, mm = mm,
#                       oscPdf = FALSE, fun.density = Voss.density, args.density = list())
#
# # t0  <- pars[3, 1]
# # st0 <- pars[7L, 1] * 2 * pars0[3L, 1]
# # pars[3, ] <- .4
#
# m2 <- DstarM:::getPdfC(tt = tt.est, pars = pars, mm = mm, DstarM = FALSE, oscPdf = FALSE, precision = 3)
# matplot(tt.est, m2, type = 'l')
#
# matplot(tt.est, abs(m1 - m2), type = 'l')
#
# max(abs(m1 - m2))
#
#
# expect_equal
#
# expect_that({}
#
