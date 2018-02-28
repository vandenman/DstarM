context("test-simulate.R")

set.seed(42)
tt <- seq(0, 5, .2)
pdfND <- dbeta(tt, 10, 30)
n <- 100
pars <- c(1, 2, .5, .5, .5)
dat <- simData(n, pars, tt, pdfND)
fixed <- matrix(c('z1', .5,  'sz1', .5, 'sv1', .5), 2, 3)

test_that("estimation works", {

	sum <- summary(dat)
	expect_equal(
		object = summary(dat),
		expected = structure(c("Min.   :0.400  ", "1st Qu.:0.400  ", "Median :0.400  ",
							   "Mean   :0.474  ", "3rd Qu.:0.600  ", "Max.   :1.200  ", "lower:20  ",
							   "upper:80  ", NA, NA, NA, NA, "Min.   :1  ", "1st Qu.:1  ", "Median :1  ",
							   "Mean   :1  ", "3rd Qu.:1  ", "Max.   :1  "), .Dim = c(6L, 3L
							   ), .Dimnames = list(c("", "", "", "", "", ""), c("      rt",
							   												 " response", "  condition")), class = "table")


	)
})

test_that("estDstarM useRcpp = FALSE works", {

	fitD <- estDstarM(data = dat, tt = tt, fixed = fixed, verbose = FALSE)

	expect_equal(
		object = fitD$Bestvals,
		expected = structure(c(0.949695690096916, 1.57237466724544, 0.5, 0.5, 0.5
							   ), .Names = c("a1", "v1", "z1", "sz1", "sv1")),
		tol = 1e-4
	)
})

test_that("estDstarM useRcpp = TRUE works", {

	fitD <- estDstarM(data = dat, tt = tt, fixed = fixed, verbose = FALSE, useRcpp = TRUE)

	expect_equal(
		object = fitD$Bestvals,
		expected = structure(c(0.949695690096916, 1.57237466724544, 0.5, 0.5, 0.5
		), .Names = c("a1", "v1", "z1", "sz1", "sv1")),
		tol = 1e-4
	)
})
