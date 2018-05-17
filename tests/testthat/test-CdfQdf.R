context("estCdf & estQdf")

tol <- 1e-4
xx <- seq(-5, 5, length.out = 1e5)

test_that("estCdf works.", {
  approxCdf <- estCdf(dnorm(xx))
  trueCdf<- pnorm(xx)
  expect_equal(
    object = approxCdf,
    expected = trueCdf,
    tolerance = tol
  )
})

test_that("estQdf works", {
  d <- dnorm(xx)
  p <- seq(0, 1, length.out = 21)
  cEst <- estCdf(d)
  approxQdf <- estQdf(p = p, x = xx, cdf = cEst)
  trueQdf <- qnorm(p)
  expect_equal(
    object = approxQdf[-c(1, length(approxQdf))],
    expected = trueQdf[-c(1, length(trueQdf))],
    tolerance = tol
  )
})

test_that("Normalize works", {

  d <- dnorm(xx)
  d0 <- normalize(d, xx)
  expect_equal(
    object = DstarM:::simpson(xx, d),
    expected = 0.9999994,
    label = "simpson integrates to approximately 1.",
    tolerance = tol
  )
  expect_equal(
    object = DstarM:::simpson(xx, d0),
    expected = 1,
    label = "After normalization integrates to 1.",
    tolerance = tol
  )
  d1 <- normalize(d, xx, props = .5)
  expect_equal(
    object = DstarM:::simpson(xx, d1),
    expected = .5,
    label = "After normalization integrates to 0.5.",
    tolerance = tol
  )
})

tt <- seq(0, 9, length.out = 1e4)
# 2 poper densities
x1 <- cbind(dexp(tt, .5), dexp(tt, 2))
# still 2 poper densities
x2 <- normalize(10*x1, tt)
# 2 densities that integrate to .5
x3 <- normalize(x1, tt, props = c(.5, .5))

# plot the results
matplot(tt, cbind(x1, x2, x3), type = "l", ylab = "density",
        col = rep(1:3, each = 2), lty = rep(1:2, 3), las = 1, bty = "n")
legend("topright", legend = rep(paste0("x", 1:3), each = 2),
       col = rep(1:3, each = 2), lty = rep(1:2, 3), bty = "n")
