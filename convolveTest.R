library(microbenchmark)

# 1D ----
x <- seq(-3, 2, .01)
f <- dunif(x, -.5, .5)
g <- f

r0 <- DstarM:::customConvolveO(f, rev(g))
c0 <- convolveC(f, g)
length(r0)
length(c0)
matplot(seq_along(r0), cbind(r0, c0), type = "l")
all.equal(r0, c(c0))

timing <- microbenchmark(
	r0 <- DstarM:::customConvolveO(f, rev(g)),
	c0 <- convolveC(f, g)
)
print(timing, unit = "relative")

# 2D ----
f2 <- matrix(f, length(x), 16)
g2 <- f2

foo <- function(x, y) {

	out <- matrix(nrow = 2*nrow(x) - 1, ncol = ncol(x))
	y <- y[nrow(y):1, ]
	for (i in 1:ncol(x)) {

		out[, i] <- DstarM:::customConvolveO(x[, i], y[, i])

	}

	return(out)

}
r2 <- foo(f2, g2)
c2 <- convolveC2(f2, g2)
all.equal(r2, c2)

timing <- microbenchmark(
	r2 <- foo(f2, g2),
	c2 <- convolveC2(f2, g2),
	times = 50
)
print(timing, unit = "relative")

# chisq ----
x <- seq(-5, 5, .01)
f <- dnorm(x)
g <- dnorm(x, 1, 1.5)
r3 <- chisq(x, f, g)
c3 <- chisqC(x, f, g)
all.equal(r3, c3)
timing <- microbenchmark(
	r3 <- chisq(x, f, g),
	c3 <- chisqC(x, f, g)
)
print(timing, unit = "relative")

# r.obj ----
tt <- seq(0, 1, .01)
c <- dunif(tt, .4, .6)
b <- dunif(tt, .4, .6)
a <- convolveC(c, b)[seq_along(tt)]

r4 <- DstarM:::r.obj(tt = tt, a = a, bb = b, lenPre = 0, lenPost = 0, r = c[-c(1, length(c))])
c4 <- rObjC2(tt = tt, a = a, bb = b, lenPre = 0, lenPost = 0, r = c[-c(1, length(c))])
all.equal(r4, c4)

timing <- microbenchmark(
	r4 <- DstarM:::r.obj(tt = tt, a = a, bb = b, lenPre = 0, lenPost = 0, r = c[-c(1, length(c))]),
	c4 <- rObjC2(tt = tt, a = a, bb = b, lenPre = 0, lenPost = 0, r = c[-c(1, length(c))])
)
print(timing, unit = "relative")

# nthMoment ----
x <- seq(-5, 5, .01)
y <- dnorm(x)
r5 <- DstarM:::nth.momentS(x, y, 2)
c5 <- nthMomentSC(x, y, 2)
all.equal(r5, c5)

timing <- microbenchmark(
	r5 <- DstarM:::nth.momentS(x, y, 2),
	c5 <- nthMomentSC(x, y, 2)
)
print(timing, unit = "relative")

# nthMoment ----
x <- seq(-5, 5, .01)
y <- dnorm(x)
r6 <- DstarM:::nth.cmomentS(x, y, 2)
c6 <- nthCMomentSC(x, y, 2)
all.equal(r6, c6)

timing <- microbenchmark(
	DstarM:::nth.cmomentS(x, y, 2),
	nthCMomentSC(x, y, 2)
)
print(timing, unit = "relative")

# getVar ----
x <- seq(-7, 7, .01)
Pdf <- cbind(dnorm(x), dnorm(x, 0, 2))
mm <- diag(2)
r7 <- DstarM:::getVar(Pdf, x, mm)
c7 <- getVarC(Pdf, x, mm)
all.equal(r7, c(c7))

timing <- microbenchmark(
	DstarM:::getVar(Pdf, x, mm),
	getVarC(Pdf, x, mm)
)
print(timing, unit = "relative")

# oscCheck ----
x <- seq(-3, 3, .001)
pdf0 <- dnorm(x) + 1e-3*dnorm(x, 2, .01)
pdf1 <- dnorm(x)
pdfm0 <- matrix(pdf0)
pdfm1 <- matrix(pdf1)

r80 <- DstarM:::oscCheck(pdf0)
c80 <- oscCheckC(pdfm0)
r81 <- DstarM:::oscCheck(pdf1)
c81 <- oscCheckC(pdfm1)

all.equal(r80, c80)
all.equal(r81, c81)

timing <- microbenchmark(
	r8 = DstarM:::oscCheck(pdf),
	c8 = oscCheckC(pdfm)
)
print(timing, unit = "relative")

