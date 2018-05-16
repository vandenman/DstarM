# library(microbenchmark)
# library(DstarM)
#
# # 1D ----
# x <- seq(0, 2, .01)
# f <- dunif(x, 0, .5)
# g <- f
#
# r0 <- DstarM:::customConvolveO(f, rev(g))
# c0 <- DstarM:::convolveC(f, g)
# length(r0)
# length(c0)
# matplot(seq_along(c0), cbind(r0[seq_along(c0)], c0), type = "l")
# all.equal(r0[seq_along(c0)], c(c0))
#
# timing <- microbenchmark(
# 	r0 <- DstarM:::customConvolveO(f, rev(g)),
# 	c0 <- DstarM:::convolveC(f, g)
# )
# print(timing, unit = "relative")
#
# # 2D ----
# f2 <- matrix(f, length(x), 16)
# g2 <- f2
#
# foo <- function(x, y) {
#
# 	out <- matrix(nrow = 2*nrow(x) - 1, ncol = ncol(x))
# 	y <- y[nrow(y):1, ]
# 	for (i in 1:ncol(x)) {
#
# 		out[, i] <- DstarM:::customConvolveO(x[, i], y[, i])
#
# 	}
#
# 	return(out)
#
# }
# r2 <- foo(f2, g2)
# c2 <- convolveC2(f2, g2)
# all.equal(r2, c2)
#
# timing <- microbenchmark(
# 	r2 <- foo(f2, g2),
# 	c2 <- convolveC2(f2, g2),
# 	times = 50
# )
# print(timing, unit = "relative")
#
# # chisq ----
# x <- seq(-5, 5, .01)
# f <- dnorm(x)
# g <- dnorm(x, 1, 1.5)
# r3 <- DstarM:::chisq(x, f, g)
# c3 <- DstarM:::chisqC(x, f, g)
# all.equal(r3, c3)
# timing <- microbenchmark(
# 	r3 <- DstarM:::chisq(x, f, g),
# 	c3 <- DstarM:::chisqC(x, f, g)
# )
# print(timing, unit = "relative")
#
# # r.obj ----
# tt <- seq(0, 1, .01)
# cc <- dunif(tt, .4, .6)
# b <- dunif(tt, .4, .6)
# a <- DstarM:::convolveC(cc, b)#[seq_along(tt)]
# lpre = rep(0, 5)
# lpost = rep(0, 5)
# i1 = !(1:length(a) %in% 1:length(lpre))
# i2 = !rev(1:length(a) %in% 1:length(lpost))
# i3 = i1 & i2
#
# rFun = getFromNamespace("r.obj", asNamespace("DstarM"))
# cppFun = getFromNamespace("rObjC3", asNamespace("DstarM"))
#
# cc = rnorm(length(tt))
#
# r4 <- rFun(tt = tt, a = a, bb = b, lenPre = lpre, lenPost = lpost, r = cc[i3])
# c4 <- cppFun(tt = tt, a = a, bb = b, lenPre = lpre, lenPost = lpost, r = cc[i3])
# all.equal(r4, c4)
#
# r4 <- rFun(tt = tt, a = a, bb = b, lenPre = lpre, lenPost = numeric(), r = cc[i1])
# c4 <- DstarM:::rObjC2(tt = tt, a = a, bb = b, lenPre = lpre, r = cc[i1])
# all.equal(r4, c4)
#
# r4 <- rFun(tt = tt, a = a, bb = b, lenPre = numeric(), lenPost = lpost, r = cc[i2])
# c4 <- DstarM:::rObjC1(tt = tt, a = a, bb = b, lenPost = lpost, r = cc[i2])
# all.equal(r4, c4)
#
# r4 <- rFun(tt = tt, a = a, bb = b, lenPre = numeric(), lenPost = numeric(), r = cc)
# c4 <- DstarM:::rObjC0(tt = tt, a = a, bb = b, r = cc)
# all.equal(r4, c4)
#
#
#
#
# timing <- microbenchmark(
# 	r4 <- rFun(tt = tt, a = a, bb = b, lenPre = 0, lenPost = 0, r = c[-c(1, length(c))]),
# 	c4 <- cppFun(tt = tt, a = a, bb = b, lenPre = 0, lenPost = 0, r = c[-c(1, length(c))])
# )
# print(timing, unit = "relative")
#
# # nthMoment ----
# x <- seq(-5, 5, .01)
# y <- dnorm(x)
# r5 <- DstarM:::nth.momentS(x, y, 2)
# c5 <- nthMomentSC(x, y, 2)
# all.equal(r5, c5)
#
# timing <- microbenchmark(
# 	r5 <- DstarM:::nth.momentS(x, y, 2),
# 	c5 <- nthMomentSC(x, y, 2)
# )
# print(timing, unit = "relative")
#
# # nthMoment ----
# x <- seq(-5, 5, .01)
# y <- dnorm(x)
# r6 <- DstarM:::nth.cmomentS(x, y, 2)
# c6 <- nthCMomentSC(x, y, 2)
# all.equal(r6, c6)
#
# timing <- microbenchmark(
# 	DstarM:::nth.cmomentS(x, y, 2),
# 	nthCMomentSC(x, y, 2)
# )
# print(timing, unit = "relative")
#
# # getVar ----
# x <- seq(-7, 7, .01)
# Pdf <- cbind(dnorm(x), dnorm(x, 0, 2))
# mm <- diag(2)
# r7 <- DstarM:::getVar(Pdf, x, mm)
# c7 <- getVarC(Pdf, x, mm)
# all.equal(r7, c(c7))
#
# timing <- microbenchmark(
# 	DstarM:::getVar(Pdf, x, mm),
# 	getVarC(Pdf, x, mm)
# )
# print(timing, unit = "relative")
#
# # oscCheck ----
# x <- seq(-3, 3, .001)
# pdf0 <- dnorm(x) + 1e-3*dnorm(x, 2, .01)
# pdf1 <- dnorm(x)
# pdfm0 <- matrix(pdf0)
# pdfm1 <- matrix(pdf1)
#
# r80 <- DstarM:::oscCheck(pdf0)
# c80 <- oscCheckC(pdfm0)
# r81 <- DstarM:::oscCheck(pdf1)
# c81 <- oscCheckC(pdfm1)
#
# all.equal(r80, c80)
# all.equal(r81, c81)
#
# timing <- microbenchmark(
# 	r8 = DstarM:::oscCheck(pdf),
# 	c8 = oscCheckC(pdfm)
# )
# print(timing, unit = "relative")
#
# # Voss DstarM ----
# # explicitly import functions to avoid biasing timings by repeatedly importing
# rFun = getFromNamespace("Voss.density", asNamespace("DstarM"))
# cppFun = getFromNamespace("getVoss", asNamespace("DstarM"))
#
# rt <- seq(0, 5, .01)
# #                a    v   t0    d    szr  szv count  z
# pars <- matrix(c(1.2, 2.0, 0.0, 0.0, 0.6, 2.3, 0.0, .75))
# precision <- 3.0
#
# parsD <- pars[c(1:2, 8, 5:6)]
#
# r9 <- cbind(rFun(rt, parsD, boundary = "lower", DstarM = TRUE),
# 			rFun(rt, parsD, boundary = "upper", DstarM = TRUE))
# c9 <- cppFun(rt, pars, precision)
# c99 <- DstarM:::getPdfC(tt = rt, pars = matrix(parsD), mm = matrix(c(1, 1), 2, 1), DstarM = TRUE, precision = precision, oscPdf = TRUE)
# c999 <- DstarM:::getPdfC(tt = rt, pars = matrix(parsD, 5, 2), mm = matrix(c(1, 1, 0, 0, 0, 0, 1, 1), 4, 2),
# 						 DstarM = TRUE, precision = precision, oscPdf = TRUE)
# matplot(rt, c9 - c99, 'l')
# all.equal(r9, c9)
#
# timing <- microbenchmark(
# 	r9 = cbind(rFun(rt, parsD, boundary = "lower", DstarM = TRUE),
# 			rFun(rt, parsD, boundary = "upper", DstarM = TRUE)),
# 	c9 = cppFun(rt, pars, precision)
# )
# print(timing, unit = "relative")
#
# # Voss Chisq ----
# # explicitly import functions to avoid biasing timings by repeatedly importing
# rFun = getFromNamespace("Voss.density", asNamespace("DstarM"))
# cppFun = getFromNamespace("getVoss", asNamespace("DstarM"))
#
# rt <- seq(0, 5, .01)
# #                a    v   t0    d    szr  szv  st0   z
# pars <- matrix(c(1.2, 2.0, 0.4, 0.0, 0.6, 2.3, 0.2, .75))
# precision <- 3.0
#
# parsD <- pars[c(1:3, 8, 5:6, 7)]
#
# r92 <- cbind(rFun(rt, parsD, boundary = "lower", DstarM = FALSE),
# 			 rFun(rt, parsD, boundary = "upper", DstarM = FALSE))
# c92 <- getVoss(rt, pars, precision)
#
# matplot(rt, cbind(r92, c92), 'l', lty = c(1, 1, 2, 2))
# all.equal(r92, c92) # some differences
# # difficult to compare due to
# c922 <- DstarM:::getPdfC(tt = rt, pars = matrix(parsD), mm = matrix(c(1, 1), 2, 1), DstarM = FALSE, precision = precision, oscPdf = TRUE)
# r922 <- DstarM:::getPdf(tt = rt, pars = list(parsD), mm = matrix(c(1, 1), 2, 1), DstarM = FALSE, oscPdf = TRUE)
# matplot(rt, cbind(r922, c922), 'l', lty = c(1, 1, 2, 2))
# all.equal(r922, c922) # no differences after normalization
#
# par(mfrow = 1:2, bty = "n", las = 1)
# matplot(rt, cbind(r92, c92), type = 'l', lty = c(1, 1, 2, 2), xlim = 0:1)
# matplot(rt, r92 - c92, type = 'l', lty = c(1, 2), xlim = 0:1) # vaag verschil
# legend('topright', legend = rep(c("r", "c"), each = 2), col = 1:4, lty = 1, bty = "n")
# par(mfrow = c(1, 1))
#
# timing <- microbenchmark(
# 	r92 = cbind(rFun(rt, parsD, boundary = "lower", DstarM = FALSE),
# 				rFun(rt, parsD, boundary = "upper", DstarM = FALSE)),
# 	c92 = cppFun(rt, pars, precision)
# )
# print(timing, unit = "relative")
#
# # getPdfC ----
# load("C:/Users/donvd/_Laptop/ResMas/Internship/githubVraag/myfile.RData")
# restr.mat2 = do.call(cbind, restr.mat)
#
# ppp = pars[c(restr.mat2)]
# dim(ppp) = dim(restr.mat2)
# ppp0 = ppp
# #ppp[7] = ppp[2]
#
# ppp[4, ] = ppp[4, ] / ppp[3, ] * 2 * ppp[1, ] * min(c(ppp[3, ], 1 - ppp[3, ]))
# ppp[3, ] = pars[3, ] * pars[1, ]
# qq = DstarM:::getPdfC(pars = ppp, DstarM = DstarM, tt = tt, precision = 3.0, mm = mm, oscPdf = TRUE)
# matplot(tt, qq) # looks ok
#
#
# ppp2 = list(ppp0[, 1], ppp0[, 2])
# ww = DstarM:::getPdf(pars.list = ppp2, DstarM = TRUE, tt = tt, mm = mm, oscPdf = TRUE)
# all.equal(ww, qq)
#
# # total.objective ----
# load("C:/Users/donvd/_Laptop/ResMas/Internship/githubVraag/myfile.RData")
#
# restr2 = do.call(cbind, restr.mat) - 1
# r10 = DstarM:::total.objective(pars = pars, restr.mat = restr.mat, DstarM = DstarM, tt = tt, ql = ql, fixed = list(),
# 							   g = g, mm = mm, mm2 = mm2, ii = ii, jj = jj, oscPdf = oscPdf, all = FALSE, forceRestriction = TRUE,
# 							   fun.density = Voss.density, args.density = args.density, fun.dist = chisq, args.dist = args.dist,
# 							   var.data = var.data, parnames = parnames, by = by)
# c10 = totalobjectiveC(pars = pars, tt = tt, ql = ql, ii = ii-1, jj = jj-1, varData = var.data, g = g, restr = restr2, mm = mm, mm2 = mm2,
# 					  DstarM = DstarM, oscPdf = TRUE, forceRestriction = TRUE, precision = precision)
#
#
# timing = microbenchmark(
# 	r10 = replicate(100, DstarM:::total.objective(pars = pars, restr.mat = restr.mat, DstarM = DstarM, tt = tt, ql = ql, fixed = list(),
# 							   g = g, mm = mm, mm2 = mm2, ii = ii, jj = jj, oscPdf = oscPdf, all = T, forceRestriction = TRUE,
# 							   fun.density = Voss.density, args.density = args.density, fun.dist = chisq, args.dist = args.dist,
# 							   var.data = var.data, parnames = parnames, by = by)),
# 	c10 = replicate(100, totalobjectiveC(pars = pars, tt = tt, ql = ql, ii = ii-1, jj = jj-1, varData = var.data, g = g, restr = restr2, mm = mm, mm2 = mm2,
# 						  DstarM = DstarM, oscPdf = TRUE, forceRestriction = TRUE, precision = precision)),
# 	times = 50
# )
# print(timing, unit = "relative")
#
# # estDstarM ----
# set.seed(1)
# tt = seq(0, 5, .01)
# pars = c(1, 2, .5, .5, .5, # condition 1
#         1, 3, .5, .5, .5)  # condition 2
# pdfND = dunif(tt, .2, .4)
# # simulate data
# tmp = simData(n = 5e6, pars = pars, tt = tt, pdfND = pdfND, return.pdf = TRUE)
# data = tmp$dat
# # define restriction matrix
# restr = matrix(1:5, 5, 2)
# restr[2, 2] = 6 # allow drift rates to differ
#
# # Run D*M analysis
# timing1 = vector("list", 3)
# set.seed(1)
# timing1[[1]] = Sys.time()
# res1R = estDstarM(data = data, tt = tt, restr = restr)
#
# timing1[[2]] = Sys.time(); set.seed(1)
#
# res1cpp = estDstarM(data = data, tt = tt, restr = restr, useRcpp = TRUE)
# timing1[[3]] = Sys.time()
# beepr::beep(8)
# timing1[[3]] - timing1[[2]]; timing1[[2]] - timing1[[1]]
#
# timing2 = vector("list", 3)
# restrC = matrix(1:7, 7, 2); restrC[9] = 8
# timing2[[1]] = Sys.time()
# res2R = estDstarM(data = data, tt = tt, restr = restrC, DstarM = FALSE,
# 				  Optim = list(parallelType = 1))
#
# timing2[[2]] = Sys.time(); set.seed(1)
#
# res2cpp = estDstarM(data = data, tt = tt, restr = restrC, useRcpp = TRUE, DstarM = FALSE)
# timing2[[3]] = Sys.time()
#
# timing2[[3]] - timing2[[2]]; timing2[[2]] - timing2[[1]]
# beepr::beep(8)
#
# par(mfrow = c(1, 3))
# matplot(tt, tmp$pdfUnnormalized, type = 'l', xlim = 0:1, lty = 1)
# plot(res2R, type = 'l', xlim = 0:1)
# plot(res2cpp, type = 'l', xlim = 0:1)
#
# truePars = matrix(pars, 5, 2)
# truePars = rbind(truePars[1:2, ], .3, truePars[3:5, ], .1)
#
# res2cpp = estDstarM(data = data, tt = tt, useRcpp = F, DstarM = FALSE,
# 					pars = c(truePars))
#
# # estND ----
# tt = seq(0, 5, .01)
# pars = c(.8, 2, .5, .5, .5, # condition 1
#         .8, 3, .5, .5, .5, # condition 2
#         .8, 4, .5, .5, .5) # condition 3
# pdfND = dbeta(tt, 10, 30)
# # simulate data
# ttt = simData(n = 3e5, pars = pars, tt = tt, pdfND = pdfND, return.pdf = TRUE)
# dat = ttt$dat
#
# restr = matrix(1:5, 5, 3)
# restr[2, 2:3] = 6:7 # allow drift rates to differ
# res = estDstarM(data = dat, tt = tt, restr = restr, useRcpp = TRUE, Optim = list(parallelType = 0))
# resND = estND(res, useRcpp = TRUE, Optim = list(parallelType = 0))
#
# plot(estND)
# lines(tt, pdfND, col = 2, lty = 2)
#
#
# gg = fft(DstarM:::convolveC(rowMeans(res$modelDist), dunif(tt)))
# matplot(tt, gg)
# hh = fft(rowMeans(res$g.hat))
# ff = hh / gg
# rr = fft(ff, inverse = TRUE)
# matplot(tt, abs(Conj(rr)), type = 'l')
#
# qq =pracma::deconv(DstarM:::convolveC(rowMeans(res$modelDist), dunif(tt)), rowMeans(res$g.hat))
# plot(tt[-1], qq[[2]])
#
#
# # restrictions
# pars <- c(1:5)
#
# DstarM:::imposeFixations()
# DstarM:::imposeFixationsC()
# DstarM::imposeFixationsC()
